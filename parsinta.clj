
;; parser :: data → nil | [result data']

(defn parse [parser data]
   (println (str "Parsing '" data "'"))
   (if-let [[result data] (parser (seq data))]
      (do
         (if (not (empty? data))
            (println "Note: trailing garbage after parse: " data))
         result)
      (do
         (println "Parse failed :(")
         nil)))

;; -----------------------------------------------------------------------

(defn get-anything [data]
   (if (empty? data)
      nil
      [(first data) (rest data)]))

(defn get-this [x]
   (fn [data]
      (if (= (first data) x)
         [x (rest data)]
         nil)))

(defn get-if [ok?]
   (fn [data]
      (if (and (not (empty? data)) (ok? (first data)))
         [(first data) (rest data)]
         nil)))

(defn get-either [a b]
   (fn [data] (or (a data) (b data))))

(defn get-nothing [value]
   (fn [data] [value data]))

(defn get-maybe [parser]
   (get-either parser (get-nothing ())))

(defn get-both [a b]
   (fn [data]
      (if-let [[aval data] (a data)]
         (if-let [[bval data] (b data)]
            [[aval bval] data]))))

(defn get-maybe-many [a]
   (get-either 
      (fn [data]
         (if-let [[this data] (a data)]
            (if-let [[tail data] ((get-maybe-many a) data)]
               [(cons this tail) data])))
      (get-nothing ())))

(defn get-many [a]
   (fn [data]
      (if-let [[head data] (a data)]
         (if-let [[tail data] ((get-maybe-many a) data)]
            [(cons head tail) data]))))

(defn get-maybe-many [parser]
   (fn [data]
      (loop [data data matched ()]
         (if-let [res (parser data)]
            (let [[value data] res]
               (recur data (cons value matched)))
            [(reverse matched) data]))))

(defn get-these-chars [str]
   (get-if (let [wanted (set (seq str))] (partial wanted))))

(def get-symbol-char 
   (get-these-chars "abcdefghijklmnopqrstuvwxyz+*/_-?"))

(def get-whitespace-char
   (get-these-chars " \n\r"))

(def get-digit
   (get-these-chars "0123456789"))

(defn get-number [data]
   (if-let [[digits data] ((get-many get-digit) data)]
      [(reduce (fn [a b] (+ (* a 10) b)) (map (fn [x] (- (int x) 48)) digits)) data]))

(def get-word 
   (get-many get-symbol-char))

(defn get-symbol [data]
   (if-let
      [[chars data] ((get-many get-symbol-char) data)]
      [(symbol (apply str chars)) data]))

(defmacro if-let* [bindings body] 
   (if (empty? bindings) 
      body 
      (let [var (gensym)
            [name value & rest] bindings]
         (list 'let (vector var value)
            (list 'if var 
               (list 'let (vector name var)
                  (list 'if-let* rest body))
               var)))))


(def get-whitespace
   (get-many get-whitespace-char))

(def maybe-whitespace
   (get-maybe-many get-whitespace-char))

(defn get-simple-sexp [data]
   (if-let*
      [[skip data] (maybe-whitespace data)
       [val data]
         ((get-either get-number get-symbol) data)]
      [val data]))

(defn get-sexp [data]
   (if-let*
      [[skip data] (maybe-whitespace data)
       [value data]
         ((get-either get-simple-sexp
            (fn [data]
               (if-let*
                  [[skip data] ((get-this \() data)
                   [vals data] ((get-maybe-many get-sexp) data)
                   [skip data] ((get-this \)) data)]
                  [vals data])))
            data)]
      [value data]))

;;; ------------------------------------------------------------------

; (println (parse (get-word "foo") "foobar"))

(print (eval (parse get-sexp "  (reduce * (map (partial * 10) (list 1 2 3 4 5)))")))

(defn get-seq [things]
   (if (empty? things)
      (get-nothing ())
      (fn [data]
         (if-let*
            [[head data] ((first things) data)
             [tail data] ((get-seq (rest things)) data)]
            [(cons head tail) data]))))

(defn get-word [str]
   (let [ps (map get-this (seq str))]
      (fn [data]
         (if-let [[chars data] ((get-seq ps) data)]
            [str data]))))


