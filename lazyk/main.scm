(load "stdlib.scm")

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
           (lambda (n) (pair (n •))))))

(lazy-def '(stringify-app s) '(lambda (_ _ _ _ _) s))
(lazy-def '(stringify-recursor a b)
  '(stringify-app (• (pair 96) (S (• • ($ a)) ($ b) stringify-helper))))

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
     (S (• encode-encoded-app ($ a)) ($ b) encode-encoded-helper)))

(lazy-def '(encode-encoded x)
  '(x encode-encoded-recursor
      I I I I I)) ; dummy arguments

(lazy-def 'escape-encoded
  `(lambda (t)
     ,(encode '((((s s) k) (splice (encode-encoded t))) i))))

; like 'fix', but instead of the term getting itself as an argument,
; it gets its encoded form as an argument
(define (kleene term)
  (define fullterm `(• ,term escape-encoded))
  (define fullterm-encoded (encode (laze fullterm)))
  (laze `(S S K ,fullterm-encoded I)))

(lazy-def 'main-of-main
  '(lambda (self input)
     (parse input (stringify self) (omega omega) 0)))

(lazy-def 'main (kleene 'main-of-main))
(dump 'main)
