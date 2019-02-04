`(1 2 ~(+ 3 1))
`(1 2 ~@(list 3 4))

working syntax quote support

- add support for var-args
  - lexer emits & as its own symbol
  - analyzer parses &
  - compiler emits var-arg flag on fn
  - compiler emits extra argument in every fn call
    to indicate # of arguments included
  - vm uses var-arg flag and # args to roll up
    var-args into a final list argument

- use var-args to implement concat
- lexer

(def concat (fn (& seqs)
                (let (concat-two (fn concat-two (seq-a seq-b)
                                  (if (nil? seq-a)
                                      seq-b
                                    (concat-two (rest seq-a) (cons (first seq-a) seq-b))))

                      concat-n (fn concat-n (concated remaining)
                                   (if (nil? remaining)
                                       concated
                                       (let (next (first remaining)
                                             remaining (rest remaining))
                                         (concat-n (concat-two (reverse concated) next) remaining)))))

                  (concat-n '() seqs))))





(concat-n '() '((1 2 3) (4 5 6)))

(concat '(1 2 3) '(4 5 6) '(7 8 9))
(concat '(1 2 3) '(4 5 6))
(concat '(1 2 3) nil) ; ok
(concat nil '(1 2 3)) ; ok
(concat nil) ; ok
(concat) ; ok
(concat '(1 2 3)) ; fail


(def concat-two (fn concat-two (seq-a seq-b)
  (if (nil? seq-a)
    seq-b
    (concat-two (rest seq-a) (cons (first seq-a) seq-b)))))

(def concat-n (fn concat-n (concated remaining)
  (if (nil? remaining)
    concated
    (let (next (first remaining)
          todo (rest remaining))
      (list (concat-two concated next) todo)))))



;; temporary concat, until we fix bugs in closures
;;(def concat-two (fn concat-two (seq-a seq-b)
;;                    (if (nil? seq-a)
;;                        seq-b
;;                      (concat-two (rest seq-a) (cons (first seq-a) seq-b)))))
;;
;;(def concat-n (fn concat-n (concated remaining)
;;                  (if (nil? remaining)
;;                      concated
;;                    (let (next (first remaining)
;;                               remaining (rest remaining))
;;                      (concat-n (concat-two (reverse concated) next) remaining)))))
;;
;;(def concat (fn (& seqs)
;;                (concat-n '() seqs)))
;; end temporary concat

;; non-variadic concat
;(def concat (fn (seq-a seq-b)
;                (let (_concat (fn loop (seq-a seq-b)
;                                  (if (nil? seq-a)
;                                      seq-b
;                                      (loop (rest seq-a) (cons (first seq-a) seq-b)))))
;                  (_concat (reverse seq-a) seq-b))))

;; preferred variadic concat, uses closures

;;(def concat (fn (& seqs)
;;                (let (concat-two (fn concat-two (seq-a seq-b)
;;                                     (if (nil? seq-a)
;;                                         seq-b
;;                                       (concat-two (rest seq-a) (cons (first seq-a) seq-b))))
;;
;;                                 concat-n (fn concat-n (concated remaining)
;;                                              (if (nil? remaining)
;;                                                  concated
;;                                                (let (next (first remaining)
;;