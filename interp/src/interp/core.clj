(ns interp.core)

(defn make-heap-object
  []
  {:color :white} ;; white gray black
  )

(defn make-stack-frame
  []
  {:args []
   :locals []}
  )

(defn make-vm
  [instructions]
  {:vars {}
   :stack []
   :heap []
   :instructions instructions})

