(load "lazier.scm")

(lazy-def 'I 'i)
(lazy-def 'K 'k)
(lazy-def 'S 's)

(lazy-def '(compose f g x) '(f (g x)))
(lazy-def '($ x y) '(y x))
(lazy-def '(pair x y p) '(p x y))
(lazy-def '(diag f x) '(f x x))

(lazy-def 'presucc '(S S))
(lazy-def '(pre* a b) '(S (S I a) b))
(lazy-def 'pre^ 'S)
(lazy-def 'preomega '(diag pre^))
(lazy-def 'preoblong '(S (S S S)))

(lazy-def       0 '(K I))
(lazy-def   'pre2 'diag)
(lazy-def       2 '(pre2 compose))
(lazy-def   'pre2 'diag)
(lazy-def   'pre4 '(preomega pre2))
(lazy-def   'pre5 '(presucc pre4))
(lazy-def 'pre105 '(pre* pre5 (presucc (preoblong pre4)))) ; 49
(lazy-def     105 '(pre105 compose)) ; 57
(lazy-def     256 '($ 2 (lambda (x) ((x x) (x x))))) ; 23

;  H   e   l   l   o  ,     W   o   r   l   d  !
; 72 101 108 108 111 44 32 87 111 114 108 100 33

(dump '(K (pair 105 (K 256))))
