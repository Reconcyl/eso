(import pat)

(defn tuple/new-of-type [ty]
  (pat/match ty
    :parens   tuple
    :brackets tuple/brackets))

(defn tuple/replace-range [tup i1 i2 new]
  (assert (<= i1 i2))
  [;(tuple/slice tup 0 i1) ;new ;(tuple/slice tup i2)])

(defdyn *3forth-stack*)
(defdyn *3forth-cont*)

(defn mkbool       [x] (if x '[swap] '[]))
(defn block?       [x] (and (tuple? x) (= :brackets (tuple/type x))))
(defn forth-value? [x] (or (block? x) (symbol? x)))

(defn push        [& xs] (assert (all forth-value? xs)) (array/concat (dyn *3forth-stack*) xs))
(defn pop         []     (assert (array/pop (dyn *3forth-stack*))))
(defn pop-block   []     (def x (pop)) (assert (tuple? x) x) x)
(defn concat-cont [xs]   (array/concat (dyn *3forth-cont*) (reverse xs)))

(defn swap   [] (push (pop) (pop)))
(defn rot    [] (let [x (pop) y (pop) z (pop)] (push y x z)))
(defn dup    [] (let [x (pop)] (push x x)))
(defn cat    [] (let [x (pop-block) y (pop-block)] (push (tuple/brackets ;y ;x))))
(defn quot   [] (push (tuple/brackets (pop))))
(defn app    [] (concat-cont (pop-block)))
(defn rdump  [] (push ;(reverse (pop-block))))
(defn out    [] (pp (pop)))
(defn block? [] (push (mkbool (tuple? (pop)))))
(defn eq?    [] (push (mkbool (= (pop) (pop)))))
(defn cons   [] (push (tuple/brackets (pop) ;(pop-block))))
(defn uncons [] (def x (pop-block))
                (assert (not (empty? x)))
                (push (tuple/brackets ;(slice x 1)) (x 0)) )
(defn dip    [] (def x (pop-block))
                (def y (pop))
                (concat-cont [;x (tuple/brackets y) 'rdump]) )

(def interpreter-dict-no-reflection
  { 'pop     pop
    'swap    swap
    'rot     rot
    'dup     dup
    'cat     cat
    'quot    quot
    'app     app
    'rdump   rdump
    'out     out
    'block?  block?
    'eq?     eq?
    'cons    cons
    'uncons  uncons
    'dip     dip
  })

(defn unexpand-definition [tupl definition]
  (def [name _ body] definition)
  (assert (= :parens (tuple/type body)))
  (def L (length body))
  (defn pattern-occurs-at? [i] (= body (slice tupl i (+ i L))))
  (var new-tupl nil)
  (loop [i :range [0 (inc (- (length tupl) (length body)))]]
    (when (pattern-occurs-at? i)
      (set new-tupl (tuple/replace-range tupl i (+ i L) [name]))
      (break) ))
  (if new-tupl (unexpand-definition new-tupl definition) tupl))

(def forth-defs @[])
(defn defs-as-tbl []
  (tabseq [definition :in forth-defs :let [[name] definition]] name definition))

(defn expand-definitions [form]
  (def defs (defs-as-tbl))
  (defn rec [form]
    (pat/match form
      |symbol? form
      |tuple?
        ((tuple/new-of-type (tuple/type form))
          ;(catseq [child :in form]
             (if-let [definition (defs child)]
               (let [[_ expanded-form _] definition] expanded-form)
               [(rec child)] )))))
  (rec form))

(defn unexpand-definitions [form]
  (pat/match form
    |symbol? form
    |tuple?
      ((tuple/new-of-type (tuple/type form))
        ;(reduce unexpand-definition (map unexpand-definitions form) forth-defs) )))

(defn fmt-sequence [tup]
  (string/join (map |(string/format " %q" $) tup)))

(defn debug-forth []
  (printf "*%s //%s"
    (fmt-sequence (identity [;(dyn *3forth-stack*)]))
    (fmt-sequence (identity [;(reverse (dyn *3forth-cont*))])) ))

(defn run-to-completion [debug? dict code]
  (with-dyns [*3forth-stack* @[] *3forth-cont* @[]]
    (concat-cont code)
    (while (not (empty? (dyn *3forth-cont*)))
      (when debug?
        (debug-forth))
      (pat/match (array/pop (dyn *3forth-cont*))
        (and |tuple? insn) (push insn)
        (and |symbol? sym) ((or (dict sym) (errorf "unknown insn: %m"))) ))
    [;(dyn *3forth-stack*)] ))

(defn push-definition [name body]
  (assert (symbol? name))
  (assert (tuple? body))
  (array/push forth-defs [name body (unexpand-definitions body)]) )

(defmacro def-forth [name & body]
  (assert (symbol? name))
  ~(,push-definition ',name ',(expand-definitions [;body])) )

# a symbol dictionary is a block of the form
# dict  ::= [ entry* ]
# entry ::= [ sym [<implementation>] ]
#           ( <implementation> : dict k st -- dict' k' st' )

# The following routines convert a symbol dictionary into lookup code, as follows:
# []                    ↦ [ [ unknown symbol: ] swap quot cat out ]
# [[key [;impl]] ;rest] ↦ [ dup [ key ] eq? [;fallback] [ pop ;impl ] rot app pop app ]
#                         where [;rest] ↦ [;fallback]

(def-forth <symdict-codegen-1step>
  # ( [;fallback] [;impl] key -- [ dup [ key ] eq? [;fallback] [ pop ;impl ] rot app pop app ] )
  quot quot [eq?] cat [dup] swap cat [pop] rot cat quot [rot app pop app] cat rot cons cat)

(def-forth <symdict-codegen-inner>
  # ( [;symdict] [<symdict-codegen>] -- [;out] )
  swap dup [] eq?
  [ # symdict is not empty
    uncons
    # [<symdict-codegen>] [;symdict-rest] dict-entry
    [ swap app ] dip
    # [;fallback] dict-entry
    rdump
    # [;fallback] [;impl] key
    <symdict-codegen-1step>
  ]
  [ # symdict is empty, just emit the base case
    pop pop [ [unknown-symbol: ] swap quot cat out ]
  ]
  rot app pop app)

(def-forth <symdict-codegen>
  [ quot [dup app] cat <symdict-codegen-inner> ] dup app)

# Now comes the actual definitions of each of the symbols

(def-forth <pop-impl> # ( [x ;st] -- st )
  uncons pop)

(def-forth <swap-impl> # ( [x y ;st] -- [y x ;st] )
  uncons swap uncons swap rot cons swap cons)

(def-forth <rot-impl> # ( [x y z ;st] -- [z x y ;st] )
  uncons quot swap uncons quot swap uncons [ cat cat ] dip cons)

(def-forth <dup-impl> # ( [x ;st] -- [x x ;st] )
  uncons quot dup cat swap cat)

(def-forth <cat-impl> # ( [x y ;st] -- [[;y ;x] ;st] )
  uncons swap uncons rot cat cons)

(def-forth <quot-impl> # ( [x ;st] -- [[x] ;st] )
  uncons quot cons)

(def-forth <app-impl> # ( k [x ;st] -- [;x ;k] st )
  uncons rot cat swap)

(def-forth <rdump-impl> # ( [x ;st] -- [;x ;st] )
  uncons swap cat)

(def-forth <out-impl>
  uncons out)

(def-forth <block?-impl> # ( [x ;st] -- [? ;st] )
  uncons block? cons)

(def-forth <eq?-impl> # ( [x y ;st] -- [? ;st] )
  uncons swap uncons rot eq? cons)

(def-forth <uncons-impl> # ( [[x ;y] ;st] -- [x y ;st] )
  uncons uncons [ cons ] dip cons)

(def-forth <cons-impl> # ( [x y ;st] -- [[x ;y] ;st] )
  uncons swap uncons rot cons cons)

(def-forth <dip-impl> # ( k [f x ;st] -- [;f [x] rdump ;k] st )
  uncons swap uncons swap [ quot quot [ rdump ] cat rot cat cat ] dip)

(def-forth <reflect-impl> # ( dict k [f( dict k st -- dict' k' st' ) ;st] -- dict' k' st' )
  uncons app)

(def-forth <symbols>
  [pop     [<pop-impl>]]
  [swap    [<swap-impl>]]
  [rot     [<rot-impl>]]
  [dup     [<dup-impl>]]
  [cat     [<cat-impl>]]
  [quot    [<quot-impl>]]
  [app     [<app-impl>]]
  [rdump   [<rdump-impl>]]
  [out     [<out-impl>]]
  [block?  [<block?-impl>]]
  [eq?     [<eq?-impl>]]
  [cons    [<cons-impl>]]
  [uncons  [<uncons-impl>]]
  [dip     [<dip-impl>]]
  [reflect [<reflect-impl>]]
  )

(push-definition '<std-dispatch>
  (let [st (run-to-completion false interpreter-dict-no-reflection
             (expand-definitions '([<symbols>] <symdict-codegen>)) )]
    (assert (= (length st) 1))
    (expand-definitions [;(st 0)]) ))

(def-forth <std-dict>
  [[<symbols>] <symdict-codegen>] <std-dispatch>)

(def <std-dict> (expand-definitions '(<std-dict>)))

# ( st dict k sym -- st' dict' k' )
(def-forth <dispatch-sym>
  quot
  # ( st dict k [sym] )
  [ [ dup rot ] dip swap rot ] dip
  # ( dict k st dict [sym] )
  swap uncons pop app
  # ( dict' k' st' )
  rot rot)

(def-forth <dispatch-block> # ( st dict k [;b] -- [[;b] st] dict k )
  swap [ swap [ cons ] dip ] dip)

(def-forth <dispatch> # ( st dict [i ;k] -- st' dict' k' )
  uncons dup block? [<dispatch-sym>] [<dispatch-block>] rot app pop app)

(def-forth <mc-inner> # ( st dict k [<mc>] -- final-st final-dict )
  swap
  dup [] eq?
  # ( st dict [<mc>] k empty:bool )
  [ # no k=[i ;k'] is not empty
    # ( st dict [<mc>] [i ;k'] )
    [<dispatch>] rot cat
    # ( st dict [i ;k'] [<dispatch> <mc>] -- final-st final-dict )
    app
  ] [
    # yes k is empty
    # ( final-st final-dict [<mc>] [] -- final-st final-dict )
    pop pop
  ] rot app pop app)

(def-forth <mc> # ( st dict k -- final-st final-dict )
  [ quot [dup app] cat <mc-inner> ] dup app)

(def-forth <metacircular-init> # ( code -- final-st final-dict )
  [] [<std-dict>] rot <mc>)

(def <mc> (expand-definitions '(<mc>)))

(defn reflect []
  (def x (pop-block))
  (def stack (dyn *3forth-stack*))
  (def cont (dyn *3forth-cont*))
  (def old-stack (tuple/brackets ;stack))
  (def old-cont (tuple/brackets ;(reverse cont)))
  (array/clear stack)
  (array/clear cont)
  (push (tuple/brackets ;<std-dict>) old-cont old-stack)
  (concat-cont [;x 'rot 'rot ;<mc>]) )

(def interpreter-dict-with-reflection
  (merge
    interpreter-dict-no-reflection
    { 'reflect reflect }))

# run a quotation in the interpreter's context,
# with the ability to make modifications to the dictionary
(def-forth <with-dict>
  [ uncons swap pop uncons ] swap cat
  [ cons dup app swap cons ] cat
  [ dip ] swap cons
  [ dip ] swap cons
  reflect)

# modify the interpreter we are running on
# to add a new defined instruction
(def-forth <create-definition>
  [ cons ] swap cons
  <with-dict>)

(def-forth <main>
  [ myfunction [ [ Hello World ] out ] ] <create-definition>
  myfunction myfunction myfunction)

(def final-stack (run-to-completion false interpreter-dict-with-reflection (expand-definitions '(<main>))))
(printf "final stack: %q" (unexpand-definitions final-stack))
