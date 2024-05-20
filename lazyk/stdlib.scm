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
(lazy-def       3 '(pre3 compose))
(lazy-def   'pre4 '(preomega pre2))
(lazy-def       4 '(omega 2))
(lazy-def   'pre5 '(presucc pre4))
(lazy-def   'pre6 '(preoblong pre2))
(lazy-def   'pre7 '(presucc pre6)) ; 19
(lazy-def   'pre9 '(pre^ pre2 pre3))
(lazy-def  'pre10 '(presucc pre9))
(lazy-def  'pre12 '(preoblong pre3))
(lazy-def  'pre13 '(presucc pre12))
(lazy-def  'pre16 '($ pre2 (lambda (two) (S pre^ preomega two))))
(lazy-def      16 '($ 2 (lambda (two) (two two two))))
(lazy-def  'pre25 '(pre^ pre2 pre5))
(lazy-def  'pre27 '(preomega pre3))
(lazy-def      27 '(pre27 compose))
(lazy-def  'pre28 '(presucc pre27))
(lazy-def  'pre56 '(preoblong pre7)) ; 27
(lazy-def  'pre80 '(pre* pre5 pre16))
(lazy-def  'pre96 '($ pre2 (lambda (two) (pre* (preoblong two) (S pre^ preomega two))))) ; 41
(lazy-def      96 '($ 2 (S (S omega (S S S) compose) (S I omega)))) ; 45
(lazy-def 'pre105 '(pre* pre5 (presucc (preoblong pre4)))) ; 49
(lazy-def     105 '(pre105 compose)) ; 57
(lazy-def 'pre107 '(pre+ pre27 pre80)) ; TODO: this can probably be shorter
(lazy-def 'pre114 '(pre* pre2 (presucc pre56))) ; 45
(lazy-def 'pre115 '(presucc pre114)) ; 49
(lazy-def     115 '(pre115 compose)) ; 57
(lazy-def 'pre117 '(pre* pre9 pre13)) ; 45
(lazy-def     117 '(pre117 compose)) ; 53
(lazy-def     256 '($ 2 (lambda (x) ((x x) (x x))))) ; 23
