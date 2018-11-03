(ns interp.core
  (:require
   [clojure.pprint :refer [pprint]]))

(defn make-heap-object
  []
  {:color :white} ;; white gray black
  )

(defn make-vm
  [instructions]
  {:vars {}
   :frames []
   :heap []
   :stack []
   :instructions instructions
   :ip 0})

(defn log [ip stack msg]
  (printf "[vm; ip=%s; stack=%s] %s\n" ip stack msg)
  nil)

(defn run-vm
  [{:keys [vars stack heap instructions ip frames] :as vm}]

  (let [[next-inst next-args] (nth instructions ip) 
        vm (update-in vm [:ip] inc)]

    (case next-inst
      :halt (log ip stack "halting")
      :push (let [[ref-type ref-arg] next-args
                  val (case ref-type
                        :args  ;; relative function argument index
                        (if-let [{:keys [saved-stack] :as frame} (last frames)]
                          (nth saved-stack ref-arg)
                          (throw (Exception. (format "no current stack frame for %s" next-args))))

                        :const ;; constant value
                        ref-arg)]
              (log ip stack (format "push %s %s" ref-type val))
              (recur (assoc vm :stack (conj stack val))))

      :plus (let [[ref-type ref-arg] next-args
                  b (first stack)
                  a (second stack)
                  result (+ a b)
                  stack (-> stack
                            pop
                            pop
                            (conj result))]

              (log ip stack (format "plus %s %s = %s" a b result))

              (recur (assoc vm :stack stack)))

      :pop (let [[arg] next-args]
             (log ip stack "pop")
             (recur (assoc vm :stack (pop stack))))

      :call (let [[ref-type new-ip] next-args]
              (log ip stack (format "call %s %s" ref-type new-ip))
              
              (case ref-type
                :address 
                (recur
                 (assoc vm
                        :frames (conj frames {:saved-stack stack
                                              :saved-ip (inc ip)})
                        :stack []
                        :ip new-ip))))

      :ret (let [[ref-type new-ip] next-args]
             (log ip stack (format "ret"))

             (if-let [{:keys [saved-stack saved-ip] :as frame} (last frames)]
               (recur
                (assoc vm
                       :frames (pop frames) 
                       :stack (if (seq stack)                       ;; if the stack is not empty, push the top item
                                (conj saved-stack (last stack))     ;; into the parent stack as a 'return' value
                                saved-stack)
                       :ip saved-ip))
               (throw (Exception. "no current stack frame"))))

      (println (format "[vm] invalid instruction %s, halting", next-inst)))))

(comment

  (-> [;; procedure 'f'
       [:push [:args 0]]
       [:push [:args 1]]
       [:plus]
       [:push [:const 50]]
       [:plus]
       [:ret]
       ;; entry point
       [:push [:const 100]]
       [:push [:const 200]]
       [:call [:address 0]]
       [:pop]
       [:halt]
       ]
      make-vm
      (assoc :ip 6)
      run-vm)


  )
