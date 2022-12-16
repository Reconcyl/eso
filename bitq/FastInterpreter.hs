{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module FastInterpreter (compile) where

import Control.Monad (when, (>=>))
import Control.Arrow ((>>>))
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
import Control.Monad.ST.Strict (runST)

import Data.List (nub, intercalate, zip4)
import Data.Void (Void, absurd)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as BVector
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Vector
import Data.Vector.Unboxed (Vector)

import qualified Parser
import Parser (Ins' (..))

newtype Fn = Fn Int deriving (Eq, Ord, Show)

type ResolvedIns = Ins' Void Fn -- all calls are Anon
type Fns = Map Fn ResolvedIns

type NameResolve = State ( Int -- next ID
                         , Fns -- definitions
                         , [String] -- reference errors
                         )

nameResolve :: Parser.Pgm -> Either String (Fn, Fns)
nameResolve pgm =
  let
    pgmBindings :: Map String Fn
    pgmDefns :: Map Fn Parser.Ins
    (pgmBindings, pgmDefns) =
      ( Map.fromList [ (name, id) | (id, (name, _)) <- zs ]
      , Map.fromList [ (id, defn) | (id, (_, defn)) <- zs ] )
      where zs = zip ids $ Map.toList (Parser.fns pgm)
            ids = map Fn [0..]

    addReferenceError :: String -> NameResolve ()
    addReferenceError e = State.modify $ (\(n, d, es) -> (n, d, e : es))

    createDefinition :: ResolvedIns -> NameResolve Fn
    createDefinition body = State.state $ \(n, defns, errs) ->
      (Fn n, (n + 1, Map.insert (Fn n) body defns, errs))

    resolve :: Parser.Ins -> NameResolve ResolvedIns
    resolve = \case
      Add0 -> pure Add0; Add1 -> pure Add1
      Call s -> case Map.lookup s pgmBindings of
        Just fn -> pure (Anon fn)
        Nothing -> do
          addReferenceError s
          pure (Call (error "no function"))
      Ret -> pure Ret; RestartCaller -> pure RestartCaller
      Recur -> pure Recur
      Anon (Parser.Fn body) ->
        Anon <$> (createDefinition =<< resolve body)
      Cond c -> Cond <$> resolve c
      Block b -> Block <$> mapM resolve b
      Input -> pure Input; Output -> pure Output

    resolveAll :: NameResolve Fn
    resolveAll = do
      -- process the definitions of functions
      topLevelDefns <- traverse resolve pgmDefns
      -- add these definitions to the mapping
      State.modify $
        let addDefns m =
              Map.unionWith
                (\_ _ -> error "impossible: duplicate definition")
                m topLevelDefns
        in \(n, ds, es) -> (n, addDefns ds, es)
      -- process the definition of main
      main <- mapM resolve (Parser.main_ pgm)
      -- add the definition of main to the map
      createDefinition (Block main)

    initState :: (Int, Fns, [String])
    initState =
      ( Map.size pgmDefns -- leave lower IDs for top level definitions
      , Map.empty, [] )

  in
  case State.runState resolveAll initState of
    (main, (_, defns, [])) -> Right (main, defns)
    (_, (_, _, es)) -> Left $ "undefined function: " ++
                              intercalate ", " (nub es)

deadFunctionElim :: (v -> [Ins' s Fn])
                 -> (Fn, Map Fn v)
                 -> (Fn, Map Fn v)
deadFunctionElim getInsns (main, fns) = (main, used fns) where

  addFn :: Fn -> State (Set Fn) ()
  addFn id = do
    seen <- State.gets (Set.member id)
    when (not seen) $ do
      State.modify (Set.insert id)
      mapM_ addAll (getInsns (fns Map.! id))

  addAll :: Ins' s Fn -> State (Set Fn) ()
  addAll = \case
    Anon f -> addFn f
    Cond c -> addAll c
    Block b -> mapM_ addAll b
    _ -> return ()

  present = State.execState (addFn main) Set.empty

  used :: Map Fn a -> Map Fn a
  used = Map.filterWithKey (\k _ -> Set.member k present)

expandRecur :: Map Fn (Ins' s Fn) -> Map Fn (Ins' s Fn)
expandRecur = Map.mapWithKey $ \k def ->
  let
    go = \case
      Recur -> Anon k
      Cond c -> Cond (go c)
      Block b -> Block (map go b)
      o -> o
  in go def

data FlatOp = Jmp Int | CondJmp Int | CondRet deriving (Eq, Show)
type FlatIns = Ins' FlatOp Fn

flatten :: ResolvedIns -> [FlatIns]
flatten = snd . go where
  go :: ResolvedIns -> (Int, [FlatIns])
  go = \case
    Add0 -> sing Add0; Add1 -> sing Add1; Ret -> sing Ret
    RestartCaller -> sing RestartCaller; Recur -> sing Recur
    Anon f -> sing (Anon f); Call v -> absurd v
    Cond c ->
      let (w, is) = go c in (w + 1, Call (CondJmp (w + 1)) : is)
    Block bs ->
      foldr concat (0, []) (map go bs)
    Input -> sing Input; Output -> sing Output
    where
      sing i = (1, [i])
      concat (w1, i1) (w2, i2) = (w1 + w2, i1 ++ i2)

explicitRet :: [Ins' s f] -> [Ins' s f]
explicitRet = (++ [Ret])

deadCodeElim :: [Ins' FlatOp f] -> [Ins' FlatOp f]
deadCodeElim is = filterIns is (findAlive is) where

  filterIns :: [Ins' FlatOp f] -> [Bool] -> [Ins' FlatOp f]
  filterIns defn mask =
    let newIdxs :: Vector Int
        newIdxs = Vector.unfoldrExactN (length mask) step (mask, 0) where
          step :: ([Bool], Int) -> (Int, ([Bool], Int))
          step (True : t, i) = (i, (t, i + 1))
          step (False : t, i) = (-1, (t, i))
          step ([], _) = error "impossible"
    in
    [ case ins of
        Call o -> Call $ case o of
          Jmp d -> Jmp $ fixDelta d
          CondJmp d -> CondJmp $ fixDelta d
          CondRet -> CondRet
        Block _ -> error "not flat"
        Cond _ -> error "not flat"
        _ -> ins
    | (True, here, here', ins) <- zip4 mask [0..] (Vector.toList newIdxs) defn
    , let fixDelta d = case newIdxs Vector.! (here + d) of
                         -1 -> error "jump to removed instruction"
                         there' -> there' - here'
    ]

  findAlive :: [Ins' FlatOp f] -> [Bool]
  findAlive (BVector.fromList -> insns) = runST $ do
    alive <- Vector.new (length insns)
    let visit idx = do
          visited <- Vector.read alive idx
          when (not visited) $ do
          Vector.write alive idx True
          case insns BVector.! idx of
            Add0 -> next; Add1 -> next; Input -> next; Output -> next
            Call (Jmp d) -> visit (idx + d)
            Call (CondJmp d) -> do visit d; next
            Call CondRet -> next
            Ret -> done; RestartCaller -> done
            Anon _ -> next
            Recur -> error "not handled"
            Cond _ -> error "not handled"
            Block _ -> error "not handled"
            where done = pure ()
                  next = visit (idx + 1)
    visit 0
    Vector.toList <$> Vector.freeze alive

simplifyJumps :: [Ins' FlatOp f] -> [Ins' FlatOp f]
simplifyJumps (BVector.fromList -> is) =
  reverse . snd $ BVector.foldl
    (\(idx, t) ins -> (idx + 1, go idx ins : t)) (0, []) is
  where
    go here = \case
      Call (Jmp d) -> case is BVector.! (here + d) of
        Call (Jmp d2) -> Call (Jmp $ d + d2)
        Call (CondJmp d2) -> Call (CondJmp $ d + d2)
        Call CondRet -> Call CondRet
        RestartCaller -> RestartCaller
        Ret -> Ret
        _ -> Call (Jmp d)
      Call (CondJmp d) -> case is BVector.! (here + d) of
        Call (Jmp d2) -> Call (CondJmp $ d + d2)
        Ret -> Call CondRet
        _ -> Call (CondJmp d)
      i -> i

countUsages :: (Fn, Map Fn [Ins' s Fn]) -> Map Fn Int
countUsages (main, fns) = usages where

  addFn :: Fn -> State (Map Fn Int) ()
  addFn id = State.gets (Map.lookup id) >>= \case
    Nothing -> do
      State.modify (Map.insert id 1)
      mapM_ addAll (fns Map.! id)
    Just n ->
      State.modify (Map.insert id (n + 1))

  addAll :: Ins' s Fn -> State (Map Fn Int) ()
  addAll = \case
    Anon f -> addFn f
    Recur -> error "not handled"
    Cond _ -> error "not handled"
    Block _ -> error "not handled"
    _ -> return ()

  usages :: Map Fn Int
  usages = State.execState (addFn main) Map.empty

deadFunctionElim2 :: (Fn, Map Fn [Ins' s Fn]) -> (Fn, Map Fn (Int, [Ins' s Fn]))
deadFunctionElim2 (main, fns) = (main, Map.intersectionWith (,) usages fns)
  where usages = countUsages (main, fns)

amendBody :: -- function to possibly amend an instruction
             (Int -> Ins' FlatOp f -> Maybe [Ins' FlatOp f])
          -> [Ins' FlatOp f] -> [Ins' FlatOp f]
amendBody amend defn =
  let
    -- `amend` is passed the index of the new instruction
    -- as an argument. this means we have recursion between
    -- `newIdxs` and `defn'`, which is not desirable, but
    -- will work as long as the lengths of the functions
    -- returned by `amend` do not depend on the offset.
    -- this circular dependency is not really needed.
    -- a better solution would be to interleave the calls
    -- to `amend` with the calculation of `newIdxs`.
    defn' = BVector.fromList [ (ins, amend offset ins)
                             | (i, ins) <- zip [0..] defn
                             , let offset = newIdxs Vector.! i ]
    newIdxs :: Vector Int
    newIdxs = Vector.unfoldrExactN (length defn') step (0, 0) where
      step :: (Int, Int) -> (Int, (Int, Int))
      step (i, pos) = (pos, (i + 1, pos + offset))
        where offset = maybe 1 length (snd $ defn' BVector.! i)
  in
  zip [0..] (BVector.toList defn') >>= \case
    (here, (_, Just amended)) -> amended
    (here, (ins, Nothing)) -> return $
      let fixDelta = subtract here . (Vector.!) newIdxs . (here +) in
      case ins of
        Call o -> Call $ case o of
          Jmp d -> Jmp $ fixDelta d
          CondJmp d -> CondJmp $ fixDelta d
          CondRet -> CondRet
        Block _ -> error "not handled"
        Cond _ -> error "not handled"
        i -> i

inline :: Map Fn (Int, [Ins' FlatOp Fn]) -> Map Fn [Ins' FlatOp Fn]
inline fns = Map.map (go . snd) fns where

  usesRestartCaller :: [Ins' FlatOp Fn] -> Bool
  usesRestartCaller is = or [True | RestartCaller <- is]

  fnsUsingRestartCaller :: Set Fn
  fnsUsingRestartCaller = Set.fromList [id | (id, (_, f)) <- Map.toList fns
                                           , usesRestartCaller f]

  -- check if this function calls a function which might restart it
  restartable :: [Ins' s Fn] -> Bool
  restartable is = or [Set.member f fnsUsingRestartCaller | Anon f <- is]

  isRecursive :: Eq f => f -> [Ins' s f] -> Bool
  isRecursive f body = or [f == f' | Anon f' <- body]

  shouldInline :: Fn -> (Int, [Ins' s Fn]) -> Bool
  shouldInline id (uses, fn) =
    ((uses == 1) || (length fn <= 5 && not (isRecursive id fn)))
    && not (restartable fn)

  inlineFns :: Set Fn
  inlineFns = Set.fromList [id | (id, f) <- Map.toList fns
                               , shouldInline id f]

  prepareForInlining :: Int -> [Ins' FlatOp Fn] -> [Ins' FlatOp Fn]
  prepareForInlining offset body = do
    -- TODO: maybe elide the 'ret' and the end of a function
    let n = length body
    (i, ins) <- zip [0..] body
    case ins of
      Add0 -> [Add0]; Add1 -> [Add1]; Input -> [Input]; Output -> [Output]
      Call o -> return $ Call $ case o of
        Jmp d -> Jmp d
        CondJmp d -> CondJmp d
        CondRet -> CondJmp (n - i)
      Ret -> [Call $ Jmp (n - i)]
      RestartCaller -> [Call $ Jmp (- (offset + i))]
      Anon f -> [Anon f]
      Recur -> error "not handled"
      Block _ -> error "not handled"
      Cond _ -> error "not handled"

  go :: [Ins' FlatOp Fn] -> [Ins' FlatOp Fn]
  go = amendBody $ \offset ins ->
    case ins of
      Anon f | Set.member f inlineFns ->
        let (_, body) = fns Map.! f in
        Just $ prepareForInlining offset body
      _ -> Nothing

compile :: String -> Either String (Fn, Map Fn [FlatIns])
compile = Parser.parsePgm
      >=> nameResolve
      >=> deadFunctionElim (\i -> [i])
      >>> mapFns expandRecur
      >>> mapFn (flatten
             >>> explicitRet)
      >>> loopPass (mapFn (simplifyJumps
                       >>> deadCodeElim)
                >>> deadFunctionElim2
                >>> mapFns inline)
      >>> return
  where
    mapFns = fmap
    mapFn = mapFns . Map.map
    loopPass :: Eq a => (a -> a) -> (a -> a)
    loopPass f x
      | x == x' = x
      | otherwise = loopPass f x'
      where x' = f x
