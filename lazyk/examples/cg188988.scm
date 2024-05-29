(load "stdlib.scm")

; Solves https://codegolf.stackexchange.com/questions/188988/ddoouubbllee-ssppeeaakk
; (repeat every byte of STDIN twice)

(dump
  '(fix (lambda (self p)
          (2 (pair (p K))
             (self (p (K I)))))))

#;(dump
  '(S S K (lambda (p self)
          (2 (pair (p K))
             (S S K self (p (K I)))))))

#;(dump
  '(omega (lambda (self p)
          (2 (pair (p K))
             (omega self (p (K I)))))))
