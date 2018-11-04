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

(defn inc-ip
  [vm]
  (update-in vm [:ip] inc))

(defn vm-push
  [{:keys [stack frames] :as vm} [ref-type ref-arg :as args]]
  (let [val (case ref-type

              :args  ;; relative function argument index
              (let [{:keys [saved-stack]} (current-frame vm)]
                (when-not (integer? ref-arg)
                  (throw (Exception. (format "arg-index must be an integer" args))))
                (nth saved-stack ref-arg))

              :local ;; relative local index
              (let [{:keys [locals] :as frame} (current-frame vm)]
                (when-not (integer? ref-arg)
                  (throw (Exception. (format "local-index must be an integer" args))))
                (get locals ref-arg))

              :const ;; constant value
              ref-arg)]

    (log vm (format "push %s %s" ref-type val))

    (-> vm
        inc-ip
        (assoc :stack (conj stack val)))))

(defn vm-plus
  [{:keys [stack frames] :as vm} [ref-type ref-arg]]
  (let [b (first stack)
        a (second stack)
        result (+ a b)
        stack (-> stack
                  pop
                  pop
                  (conj result))]

    (log vm (format "plus %s %s = %s" a b result))

    (-> vm
        inc-ip
        (assoc :stack stack))))

(defn vm-pop
  [{:keys [stack frames] :as vm} [ref-type ref-arg :as args]]
  (let [head (peek stack)]

    (when-not head
      (throw (Exception. (format "cannot pop stack, it is empty" args))))

    (case ref-type

      ;; pop to a local
      :local (let [{:keys [locals] :as frame} (current-frame vm)
                   local-index ref-arg]

               (when-not (integer? local-index)
                 (throw (Exception. (format "local-index must be an integer" args))))

               (log vm (format "pop %s to local[%s]" head local-index))

               (-> vm
                   inc-ip
                   (update-in [:frames (.indexOf frames frame) :locals] #(assoc % local-index head))
                   (update-in [:stack] pop)))

      ;; just pop the value
      nil (do
            (log vm "pop")
            (-> vm
                inc-ip
                (assoc :stack (pop stack)))))))

(defn vm-call
  [{:keys [stack frames ip] :as vm} [ref-type new-ip]]
  (log vm (format "call %s %s" ref-type new-ip))
  
  (case ref-type
    :address 
    (assoc vm
           :ip new-ip
           :frames (conj frames {:saved-stack stack
                                 :saved-ip (inc ip)
                                 :locals {}})
           :stack [])))

(defn vm-ret
  [{:keys [stack frames] :as vm}]
  (do
    (log vm (format "ret"))

    (let [{:keys [saved-stack saved-ip] :as frame} (current-frame vm)]
       (assoc vm
              :ip saved-ip
              :frames (pop frames) 
              :stack (if (seq stack)                       ;; if the stack is not empty, push the top item
                       (conj saved-stack (peek stack))     ;; into the parent stack as a 'return' value
                       saved-stack)))))

(defn run-vm
  [{:keys [instructions ip] :as vm}]

  (let [[inst args] (nth instructions ip)
        vm (case inst
             :halt (log vm "halting")
             :push (vm-push vm args)
             :plus (vm-plus vm args)
             :pop  (vm-pop  vm args)
             :call (vm-call vm args)
             :ret  (vm-ret  vm)
             (log vm (format "invalid instruction %s %s, halting" inst args)))]
    (when vm
      (recur vm))))

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
