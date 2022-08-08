(load "lazier.scm")

(lazy-def 'I 'i)
(lazy-def 'K 'k)
(lazy-def 'S 's)

(lazy-def '(compose f g x) '(f (g x)))
(lazy-def '($ x y) '(y x))
(lazy-def 'omega '(lambda (x) (x x)))
(lazy-def '(fix f) '(S I (compose f) omega))
(lazy-def '(pair x y p) '(p x y))
(lazy-def '(diag f x) '(f x x))

(lazy-def 'succ '(lambda (n f x) (f (n f x))))
(lazy-def 'presucc '(S S))
(lazy-def '2n+1 '(S (S I S) (S I (S S (K I))) compose)) ; 27
(lazy-def '2n+2 '(S (S I (S I (S S (K I)))) S compose)) ; 27
(lazy-def '(+ a b) '(a succ b))
(lazy-def '(pre+ a b) '(S (S a S) b))
(lazy-def '(* a b) '(compose a b))
(lazy-def '(pre* a b) '(S (S I a) b))
(lazy-def 'pre^ 'S)
(lazy-def 'preomega '(diag pre^))
(lazy-def 'oblong '(lambda (n) (* n (succ n))))
(lazy-def 'preoblong '(S (S S S)))
(lazy-def '(if-even x a b) '(x (S I K) 1 (K a) b)) ; the 'S I K' term was found by brute force search

(lazy-def       0 '(K I))
(lazy-def       1 'I)
(lazy-def   'pre2 'diag)
(lazy-def       2 '(pre2 compose))
(lazy-def   'pre3 '(presucc pre2))
(lazy-def   'pre4 '(preomega pre2))
(lazy-def   'pre5 '(presucc pre4))
(lazy-def   'pre6 '(preoblong pre2))
(lazy-def   'pre9 '(pre^ pre2 pre3))
(lazy-def  'pre12 '(preoblong pre3))
(lazy-def  'pre13 '(presucc pre12))
(lazy-def  'pre16 '($ pre2 (lambda (two) (S pre^ preomega two))))
(lazy-def      16 '($ 2 (lambda (two) (two two two))))
(lazy-def  'pre25 '(pre^ pre2 pre5))
(lazy-def  'pre27 '(preomega pre3))
(lazy-def      27 '(pre27 compose))
(lazy-def  'pre28 '(presucc pre27))
(lazy-def  'pre80 '(pre* pre5 pre16))
(lazy-def  'pre96 '($ pre2 (lambda (two) (pre* (preoblong two) (S pre^ preomega two))))) ; 41
(lazy-def      96 '($ 2 (S (S omega (S S S) compose) (S I omega)))) ; 45
(lazy-def 'pre105 '(pre* pre5 (presucc (preoblong pre4)))) ; 49
(lazy-def     105 '(pre105 compose)) ; 57
(lazy-def 'pre107 '(pre+ pre27 pre80)) ; TODO: this can probably be shorter
(lazy-def 'pre115 '(2 (compose presucc (pre* pre2)) pre28)) ; 55
(lazy-def     115 '(S (S S (compose (S (S I S)) (S I)) diag) pre28 compose)) ; 59
(lazy-def 'pre117 '(pre* pre9 pre13)) ; 45
(lazy-def     117 '(pre117 compose)) ; 53
(lazy-def     256 '($ 2 (lambda (x) ((x x) (x x))))) ; 23

(lazy-def 'succ%3 '(S (S I I) K)) ; found by brute force search
(lazy-def '(branch%3 n rem0 rem1 rem2)
          '(n succ%3 I (K (K rem0)) (K rem1) rem2))

(lazy-def '(match-ski ss kk ii term)
          '(term (K I) (K (K ss)) (K ii) kk) )

(lazy-def '(parse input) '(input parse-pair K))
(lazy-def 'parse-pair
  ; TODO: this can be simplified by using `S S K` instead of `fix`
  '(fix (lambda (recurse char)
          ; '`' = 96 is even but combinators are odd
          (if-even char
            ; TODO: this can probably be simplified by using 'pair'
            (lambda (rest)
              (rest recurse (lambda (f rest2)
                              (rest2 recurse (lambda (x rest3)
                                               (pair (f x) rest3))))))
            ; 'i' = 105 = 0 mod 3
            ; 's' = 115 = 1 mod 3
            ; 'k' = 107 = 2 mod 3
            (pair (branch%3 char I S K)) ))))

(lazy-def 'eof '(fix (pair 256)))

(lazy-def 'stringify-helper
  '(lambda (term)
     (term (K I)
           (K (K ($ pre115))) ; S case
           (K ($ pre105)) ; I case
           ($ pre107) ; K case
           (lambda (n) (pair (n compose))))))

(lazy-def '(stringify-app s) '(lambda (_ _ _ _ _) s))
(lazy-def '(stringify-recursor a b)
  '(stringify-app (compose (pair 96) (S (compose compose ($ a)) ($ b) stringify-helper))))

; I am fairly confident that this function is correct.
(lazy-def '(stringify encoded)
  '(encoded stringify-recursor
      I I I I I ; dummy arguments since we know this is 'stringify-app'
      eof))

(define (encode x)
  (cond ((eq? x 's) '(K S))
        ((eq? x 'k) '(K K))
        ((eq? x 'i) '(K I))
        ((symbol? x) (error #f "only closed terms can be encoded"))
        ((= 2 (length x))
         (if (eq? 'splice (car x))
           (cadr x)
           (let ((a (encode (car x)))
                 (b (encode (cadr x))))
             `((S ((S I) ,a)) ,b))))
        (else (error #f "this term cannot be encoded")) ))

(lazy-def 'encode-encoded-helper
  '(lambda (term)
     (term (K I)
           (K (K ($ S)))
           (K ($ I))
           ($ K)
           (lambda (term) (S (S I (K K)) (K term))) )))

(lazy-def '(encode-encoded-app a b)
  `(lambda (_ _ _ _ _)
     ,(encode (laze (encode '((splice (splice a))
                              (splice (splice b)) ))))))

(lazy-def 'encode-encoded-recursor
  '(lambda (a b)
     (S (compose encode-encoded-app ($ a)) ($ b) encode-encoded-helper)))

(lazy-def '(encode-encoded x)
  '(x encode-encoded-recursor
      I I I I I)) ; dummy arguments

(lazy-def 'escape-encoded
  `(lambda (t)
     ,(encode '((((s s) k) (splice (encode-encoded t))) i))))

; like 'fix', but instead of the term getting itself as an argument,
; it gets its encoded form as an argument
(define (refix term)
  (define fullterm `(compose ,term escape-encoded))
  (define fullterm-encoded (encode (laze fullterm)))
  (laze `(S S K ,fullterm-encoded I)))

(lazy-def 'main-of-main
  '(lambda (self input)
     (parse input (stringify self) (omega omega) 0)))

(lazy-def 'main (refix 'main-of-main))
(dump 'main)
