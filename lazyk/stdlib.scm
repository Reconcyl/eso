(load "lazier.scm")
(load "minifier.scm")

(define-syntax overload-arity
  (syntax-rules ()
    ((_ (arity func) ...)
     (lambda args
       (define n (length args))
       (case n
         ((arity) (apply func args)) ...
         (else (error #f (string-append "function did not expect " (number->string n) " arguments"))) )))))

(define lazy-def
  (let ((lazy-def-old lazy-def))
    (overload-arity
      (2 lazy-def-old)
      (3 (lambda (expected-size name body)
           (define new-pair (lazy-def-old name body))
           (define actual-size (string-length (minify (dump-to-string (cdr new-pair)))))
           (when (not (= expected-size actual-size))
             (let* ((fatal? (< expected-size actual-size))
                    (diagnostic
                      (if fatal? "\x1B;[1m\x1B;[91m" #; bold-red
                                 "\x1B;[1m\x1B;[93m" #; bold-yellow)))
               (display (string-append diagnostic
                                       "warning\x1B;[m: "
                                       "(lazy-def "
                                       (number->string expected-size)
                                       " \x1B;[92m"
                                       (with-output-to-string (lambda () (display name)))
                                       "\x1b;[m\t...): actual size is "
                                       (number->string actual-size)
                                       "\n" )
                        (current-error-port) ))))))))

(lazy-def 'I 'i)
(lazy-def 'K 'k)
(lazy-def 'S 's)

(lazy-def '(compose f g x) '(f (g x)))
(lazy-def '(• f g x)       '(f (g x)))
(lazy-def '($ x y) '(y x))
(lazy-def 'omega '(lambda (x) (x x)))
(lazy-def '(fix f) '(S I (• f) omega))
(lazy-def 'fixalt '(fix (S I))) ; equivalent to fix, but shorter as a standalone combinator
(lazy-def '(pair x y p) '(p x y))
(lazy-def '(diag f x) '(f x x))

(lazy-def    'succ '(lambda (n f x) (f (n f x))))
(lazy-def    'presucc '(S S))
(lazy-def 11 'n+m+1 '(S S K succ))
(lazy-def 15 '2n+1 '(omega n+m+1))
(lazy-def 18 '2n+2 '(S (• S (S S K)) I succ)) ; = (S n+m+1 succ)
(lazy-def 15 '2n+3 '(S I S n+m+1))            ; = • succ (S n+m+1 succ)
(lazy-def 17 '3n+4 '(S (S I) S n+m+1))        ; = (S n+m+1 2n+3)
(lazy-def    '(+ a b) '(a succ b))
(lazy-def    '(pre+ a b) '(S (S a S) b))
(lazy-def    '(pre2+ a b) '(S (S a (S (S I S) S)) b))
(lazy-def    '(* a b) '(• a b))
(lazy-def    '(pre* a b) '(S (S I a) b))
(lazy-def    'pre^ 'S)
(lazy-def    'preomega '(diag pre^))
(lazy-def    'oblong '(lambda (n) (* n (succ n)))) ; x(1 + x)
(lazy-def 13 'oblonger '(succ oblong)) ; x²(1 + x)
(lazy-def    'oblong+1 '(S (S S S) S •))
(lazy-def    'preoblong '(S (S S S))) ; x(1 + x)
(lazy-def    'preoblong2 '(S (S S (S (S I S) S)))) ; x(2 + x)
(lazy-def    '(if-even x a b) '(x (S I K) I (K a) b)) ; the 'S I K' term was found by brute force search

; [1] = These definitions were taken from the list here:
;         https://codegolf.stackexchange.com/a/233481/61384
(lazy-def          0 '(K I))
(lazy-def          1 'I)
(lazy-def      'pre2 'diag)
(lazy-def          2 '(pre2 •))
(lazy-def      'pre3 '(presucc pre2))
(lazy-def 15       3 '(• (S S (S (S S) I)) K)) ; [1]
(lazy-def      'pre4 '(preomega pre2))
(lazy-def          4 '(omega 2))
(lazy-def 12   'pre5 '(presucc pre4))
(lazy-def 16       5 '(S (S (S S) S) S • I)) ; [1]
(lazy-def 11   'pre6 '(preoblong pre2))
(lazy-def 15       6 '(omega oblong 1)) ; [1]
(lazy-def 15   'pre7 '(presucc pre6))
(lazy-def 18       7 '(3n+4 1)) ; [1]
(lazy-def 15   'pre8 '(pre^ pre3 pre2))
(lazy-def 17       8 '(diag (• S S) succ 1)) ; [1]
(lazy-def 10   'pre9 '(pre^ pre2 pre3))
(lazy-def 18       9 '(omega (S S (K succ)) 1)) ; [1]
(lazy-def 14  'pre10 '(presucc pre9))
(lazy-def 18      10 '(S I (S S) (S S K succ) 1)) ; [1]
(lazy-def     'pre12 '(preoblong pre3))
(lazy-def 16      12 '(diag (S (S I)) succ 1)) ; [1] - see oblonger
(lazy-def     'pre13 '(presucc pre12))
(lazy-def 18      13 '(diag (S (S S) I) succ 1)) ; [1]
(lazy-def 12  'pre16 '(pre^ pre2 (preomega pre2)))
(lazy-def 15      16 '(S (S (S I)) succ 1)) ; [1]
(lazy-def 18      20 '(diag (S (S I S)) succ 1)) ; [1]
(lazy-def 20  'pre25 '($ pre2 (lambda (two) (pre^ two (presucc (preomega two))))))
(lazy-def 12  'pre27 '(preomega pre3))
(lazy-def 17      27 '(S S S (S S) (K succ) 1)) ; [1]
(lazy-def 16  'pre28 '(presucc pre27))
(lazy-def 20  'pre32 '(pre^ pre5 pre2))
(lazy-def 23      32 '(S S (S (S (S I)) S) (K 2))) ; [1]
(lazy-def 21      33 '(S (S (S S I)) (S S K) succ 1)) ; [1]
(lazy-def 24  'pre33 '(K 33))
(lazy-def 19  'pre42 '(preoblong pre6))
(lazy-def 27  'pre44 '(presucc (presucc pre42)))
(lazy-def 23  'pre56 '(preoblong pre7))
(lazy-def 23  'pre72 '(preoblong pre8))
(lazy-def 31  'pre80 '(preoblong2 pre8))
(lazy-def 36  'pre87 '(pre* pre3 (presucc (presucc pre27))))
(lazy-def 25      96 '(S I S (S (S (S S I) (K (S (S (K S) K))))) I I))
(lazy-def 28  'pre96 '(K 96))
(lazy-def 22 'pre100 '(pre^ pre2 pre10))
(lazy-def 22     100 '(S I S (S (S (S S) K succ)) I I)) ; [1]
(lazy-def 26 'pre101 '(presucc pre100))
(lazy-def 29 'pre108 '(pre* pre4 pre27))
#;
(lazy-def 29 'pre108 '(pre* pre27 pre4))
#;
(lazy-def 29 'pre108 '(S (• S (S (S S K (S I I)))) presucc pre3))
(lazy-def 41 'pre105 '(pre* pre5 (presucc (preoblong pre4))))
(lazy-def 47     105 '(pre105 •))
(lazy-def 52 'pre107 '(pre+ pre27 pre80)) ; TODO: this can probably be shorter
(lazy-def 22 'pre110 '(preoblong pre10))
(lazy-def 26 'pre111 '(presucc pre110))
(lazy-def 39 'pre114 '(pre* pre2 (presucc pre56)))
(lazy-def 43 'pre115 '(presucc pre114))
(lazy-def 49     115 '(pre115 •))
(lazy-def 38 'pre117 '(pre* pre9 pre13))
(lazy-def 44     117 '(pre117 •))
(lazy-def 13 'pre256 '(preomega pre4))
(lazy-def 21     256 '($ 2 (lambda (x) ((x x) (x x)))))
