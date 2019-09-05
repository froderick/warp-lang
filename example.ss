(ns warp.reader)

;; source
{:state {}
 :read-fn nil
 :unread-fn nil
 :close-fn nil}

;; buffer
{:data (array 0)
 :used 0}

;; lexer-state
{:position 0
 :line-number 0
 :col-number 0
 :buffer {})

;; token
{:type nil
 :text ""
 :source {:position 0
          :length 0
          :line-number 0
          :col-number 0}}

(defn buffer-clear (b)
  (set b :used 0)
  b)

(defn whitespace? (ch) (or (= ch '\n') (= ch ' ') (= ch '\t')))
(defn newline? (ch) (= ch '\n'))

(defn alpha-numeric? (x)
  (and (>= x 'A') (<= x 'z')))

(defn symbol-start? (ch)
  (or
    (alpha-numeric? ch)
    (= ch '+')
    (= ch '-')
    (= ch '!')
    (= ch '=')
    (= ch '_')
    (= ch '?')
    (= ch '*')))

(defn symbol-continue? (ch)
  (or
   (alpha-numeric? ch)
   (= ch '+')
   (= ch '-')
   (= ch '!')
   (= ch '=')
   (= ch '_')
   (= ch '?')
   (= ch '/')
   (= ch '>')
   (= ch '<')
   (= ch '*')))

(defn digit? (ch)
  (and (>= ch '0') (<= ch '9')))
                                                                 
(defn source-read-char (source)
  (let (r (:read-fn source))
    (r source)))

(defn state-inc-position (state)
  (let (pos (:position state))
    (set state :position (inc pos))))

(defn read-first-char (source state)
  (let loop (ch (source-read-char source))
    (if (whitespace? ch)
      (do 
        (state-inc-position state)
        (if (newline? ch)
          (do 
            (set state :line-number (inc (:line-number state)))
            (set state :col-number 1)))
        (loop (source-read-char source)))
      ch)))

;; tokenization error
{:position 0
 :line-number 0
 :col-number 0}

(defn position-info (state)
  (select-keys [:position :line-number :col-number] state))

(defn make-token (type text state)
  )

(defn token-read (source state)
  
  (buffer-clear (:buffer state))

  (let (ch (read-first-char source state)
        token (cond 
               (= ch '(') (make-token :oparen "(" state)
               (= ch ')') (make-token :cparen ")" state)
               (= ch '[') (make-token :ovec "[" state)
               (= ch ']') (make-token :cvec "]" state)
               (= ch '{') (make-token :obracket "{" state)
               (= ch '}') (make-token :cbracket "}" state)
               (= ch '`') (make-token :syntax-quote"`" state)
               (= ch '&') (make-token :symbol "&" state)
               (= ch '~')
               (digit? ch)
               (= ch '\\')
               (symbol-start? ch)
               (= ch ':')
               (= ch '\"')
               (= ch '\;')
               (throw-value (format "unexpected character: %s" ch) (position-info state)))
        token-length (count (:text token)))

    (set state :position (+ (:position state) token-length))
    (set state :col-number token-length)))

;; how clojuric are we going for here?
;; - destructuring?
;; - persistent data structures (set map vector)?
;; - multi-function arguments?
;; - protocols?
;; - deep value equivalence?
;; - case?
;; - character range comparision?
;; - pattern matching?
;; - fixed top-levels? declare vars dynamic optionally?
;; - structs/records?


