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

(defn log [{:keys [ip stack] :as vm} msg]
  (printf "[vm; ip=%s; stack=%s] %s\n" ip stack msg)
  nil)

(defn current-frame [{:keys [frames] :as vm}]
  (if-let [frame (peek frames)]
    frame
    (throw (Exception. (format "no current stack frame")))))

(defn run-vm
  [{:keys [vars stack heap instructions ip frames] :as vm}]

  (let [[next-inst next-args] (nth instructions ip)
        next-vm (update-in vm [:ip] inc)]

    (case next-inst
      :halt (log vm "halting")

      :push (let [[ref-type ref-arg] next-args
                  val (case ref-type

                        :args  ;; relative function argument index
                        (let [{:keys [saved-stack]} (current-frame vm)]
                          (when-not (integer? ref-arg)
                            (throw (Exception. (format "arg-index must be an integer" next-args))))
                          (nth saved-stack ref-arg))

                        :local ;; relative local index
                        (let [{:keys [locals] :as frame} (current-frame vm)]
                          (when-not (integer? ref-arg)
                            (throw (Exception. (format "local-index must be an integer" next-args))))
                          (get locals ref-arg))

                        :const ;; constant value
                        ref-arg)]

              (log vm (format "push %s %s" ref-type val))
              (recur (assoc next-vm :stack (conj stack val))))

      :plus (let [[ref-type ref-arg] next-args
                  b (first stack)
                  a (second stack)
                  result (+ a b)
                  stack (-> stack
                            pop
                            pop
                            (conj result))]

              (log vm (format "plus %s %s = %s" a b result))

              (recur (assoc next-vm :stack stack)))

      :pop (let [head (peek stack)
                 [ref-type ref-arg] next-args]

             (when-not head
               (throw (Exception. (format "cannot pop stack, it is empty" next-args))))

             (case ref-type

               ;; pop to a local
               :local (let [{:keys [locals] :as frame} (current-frame vm)
                            local-index ref-arg]

                        (when-not (integer? local-index)
                          (throw (Exception. (format "local-index must be an integer" next-args))))

                        (log vm (format "pop %s to local[%s]" head local-index))

                        (recur
                         (-> next-vm
                             (update-in [:frames (.indexOf frames frame) :locals] #(assoc % local-index head))
                             (update-in [:stack] pop))))

               ;; just pop the value
               nil (do
                     (log vm "pop")
                     (recur (assoc next-vm :stack (pop stack))))))
      

      :call (let [[ref-type new-ip] next-args]
              (log vm (format "call %s %s" ref-type new-ip))
              
              (case ref-type
                :address 
                (recur
                 (assoc next-vm
                        :frames (conj frames {:saved-stack stack
                                              :saved-ip (inc ip)
                                              :locals {}})
                        :stack []
                        :ip new-ip))))

      :ret (do
             (log vm (format "ret"))

             (let [{:keys [saved-stack saved-ip] :as frame} (current-frame vm)]
               (recur
                (assoc vm
                       :frames (pop frames) 
                       :stack (if (seq stack)                       ;; if the stack is not empty, push the top item
                                (conj saved-stack (peek stack))     ;; into the parent stack as a 'return' value
                                saved-stack)
                       :ip saved-ip))))

      (log vm (format "invalid instruction %s, halting" next-inst)))))


;; what is missing
;; - [:test [[:addr 'if'] [:addr 'else']]
;; - [:jump [[:addr 0]]

(comment

  (-> [;; procedure 'f'
       [:push [:const 99]]
       [:pop  [:local 0]]
       [:push [:local 0]]
       [:push [:args 0]]
       [:push [:args 1]]
       [:plus]
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
      (assoc :ip 10)
      run-vm)


  )
