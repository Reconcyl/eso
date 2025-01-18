{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (fromJust)

newtype Name = Name String deriving (Eq, Ord)
type Support = Set Name

instance Show Name where show (Name n) = '\'' : n

names :: [Name]
names = Name <$>
  flip (liftA2 (flip (:))) "abcdefghijklmnopqrstuvwxyz" ("" : map show [1..])

class Rename a where
  rename :: Name -> Name -> (a -> a)

class Rename a => Nominal a where
  support :: a -> Support

fresh :: Nominal a => a -> Name
fresh x = head $ filter (`Set.notMember` (support x)) names

instance Rename Name where
  rename a b n1 | a == b    = error "rename should only be used to swap two names"
                | n1 == a   = b
                | n1 == b   = a
                | otherwise = n1

instance Nominal Name where
  support = Set.singleton

instance Rename Support where
  rename a b = Set.map (rename a b)

instance Nominal Support where
  support = id

instance (Rename a, Rename b) => Rename (a, b) where
  rename a b (x, y) = (rename a b x, rename a b y)

instance (Nominal a, Nominal b) => Nominal (a, b) where
  support (x, y) = Set.union (support x) (support y)

instance Rename a => Rename [a] where
  rename a b = map (rename a b)

instance Nominal a => Nominal [a] where
  support = Set.unions . map support

instance Rename a => Rename (IO a) where
  rename a b = fmap (rename a b)

instance (Rename a, Rename b) => Rename (a -> b) where
  rename a b f = rename a b . f . rename b a

data Supported a = Supported Support a

withSupport :: Nominal a => a -> Supported a
withSupport a = Supported (support a) a

instance Rename a => Rename (Supported a) where
  rename a b (o@(Supported s x))
    | a `Set.notMember` s, b `Set.notMember` s = o
    | otherwise = Supported (rename a b s) (rename a b x)

instance Rename a => Nominal (Supported a) where
  support (Supported sup _) = sup

data Binder a = Binder Name a

instance Show a => Show (Binder a) where
  show (Binder n a) = show n ++ " . " ++ show a

instance (Eq a, Nominal a) => Eq (Binder a) where
  Binder a x == Binder a' x' = (rename b a x == rename b a' x')
    where b = fresh ((a, x), (a', x'))

instance Nominal a => Rename (Binder a) where
  rename a b (Binder c x)
    | c /= a, c /= b = Binder c (rename a b x)
    | otherwise      = Binder d (rename a b (rename c d x))
      where d = fresh ((a, b), x)

instance Nominal a => Nominal (Binder a) where
  support (Binder n a) = Set.delete n (support a)

bind :: Nominal a => Supported (Name -> a) -> Binder a
bind (Supported sup f) = Binder b (f b)
  where b = fresh sup

data Value
  = VN Name
  | VI Integer
  | VNil
  | VC Value Value
  | VF (Supported ([Value] -> IO Value))
  | VB (Binder Value)

instance Rename Value where
  rename a b (VN n)   = VN (rename a b n)
  rename _ _ (VI i)   = VI i
  rename _ _ VNil     = VNil
  rename a b (VC u v) = VC (rename a b u) (rename a b v)
  rename a b (VF f)   = VF (rename a b f)
  rename a b (VB v)   = VB (rename a b v)

instance Nominal Value where
  support (VN n)   = support n
  support (VI _)   = Set.empty
  support VNil     = Set.empty
  support (VC u v) = support (u, v)
  support (VF f)   = support f
  support (VB b)   = support b

makeList :: [Value] -> Value
makeList = foldr VC VNil

getList :: Value -> [Value]
getList = go [] where
  go xs (VC h t) = go (h : xs) t
  go xs VNil     = reverse xs
  go _  _        = error "list does not end with nil"

type Env = [(Name, Value)]

eval :: Env -> Value -> IO Value
eval env expr =
  case expr of
    VN n -> pure (fromJust (lookup n env))
    VI _ -> pure expr
    VNil -> pure expr
    VF _ -> pure expr
    VC v1 v2 -> eval env v1 >>= \case
      VF (Supported _ f) -> do
        args <- mapM (eval env) (getList v2)
        f args
      _ -> error "attempted to apply something that's not a function"
    VB (Binder x e) -> pure (VF (function env x e))

function :: Env -> Name -> Value -> Supported ([Value] -> IO Value)
function env n e = Supported sup f where
  sup = support ((n, env),e)
  f = \xs -> eval ((n, makeList xs) : env) e

main :: IO ()
main = print (take 100 names)
