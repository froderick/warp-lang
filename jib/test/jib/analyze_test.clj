(ns jib.analyze-test
  (:require [jib.analyze :as sut]
            [clojure.test :as t]))

(comment
  (->>
   (parse-let 
    '{:class :sexpr,
      :value
      ({:class :atom, :type :symbol, :value "let"}
       {:class :sexpr,
        :value
        ({:class :atom, :type :symbol, :value "a"}
         {:class :atom, :type :number, :value "1"}
         {:class :atom, :type :symbol, :value "a"}
         {:class :atom, :type :number, :value "2"}
         )}
       {:class :sexpr,
        :value
        ({:class :atom, :type :symbol, :value "prn"}
         {:class :atom, :type :symbol, :value "a"})})})
   pprint)

  (analyze-if [] '{:class :sexpr 
                   :value ({:class :atom :type :symbol :value "if"}
                           {:class :sexpr 
                            :value ({:class :atom  :type :symbol  :value ">"}
                                    {:class :atom  :type :symbol  :value "d"}
                                    {:class :atom  :type :number  :value "0"})}
                           {:class :sexpr 
                            :value ({:class :atom  :type :symbol  :value "prn"}
                                    {:class :atom  :type :symbol  :value "d"})})}
   )


;; new form types:
;; - func
;; - if
;; - let
;; - builtin
;; - fn-call
;; - binding-ref

  (->> "(func main (a b c)
          (let (d (+ a b c))
            (let (q 200 q 300)
              (if (> q 0)
                (prn d)
                100))
              d))"
       (antlr/parse parser)
       mapify-ast
       first
       analyze-top-level-expr
       pprint)

  )
