(load "stdlib.scm")

(define (dump-quiet tm)
  (print-as-unlambda (laze tm)))

;  H   e   l   l   o  ,     W   o   r   l   d  !
; 72 101 108 108 111 44 32 87 111 114 108 100 33

(lazy-def '(cc n) '(pair (n compose)))
#;(dump-quiet
  '(K (cc pre72
          (cc pre101
              (2 (cc pre108)
                 (cc pre111
                     (cc pre44
                         (cc pre32
                             (cc pre87
                                 (cc pre111
                                     (cc pre114
                                         (cc pre108
                                             (cc pre100
                                                 (cc pre33
                                                     (fix (pair 256)))))))))))))))) ; 703

(dump-quiet
  '(K (S ($ pre72)
       (S ($ pre101)
        (2 (S ($ pre108))
         (S ($ pre111)
          (S ($ pre44)
           (S ($ pre32)
            (S ($ pre87)
             (S ($ pre111)
              (S ($ pre114)
               (S ($ pre108)
                (S ($ pre100)
                 (S ($ pre33)
                    (compose fixalt ($ pre256))
                    )))))))))))
       (lambda (n) (pair (n compose))) ))) ; 633
