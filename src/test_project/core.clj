(ns test-project.core)
; Some stuff regarding linked lists and the difference between reading and evaluating.

(let [c (cons 37 nil)
      b (cons 99 c)
      a (cons 12 b)]
  a)

(cons 12 (cons 99 (cons 37 nil)))

(print (cons 'if (cons (cons '= (cons 'idx (cons (cons 'count (cons 'pie nil)) nil))) (cons 'pie (cons 3 nil)))))

(print (eval (cons (symbol "+") (cons 3 (cons 3 nil)))))
