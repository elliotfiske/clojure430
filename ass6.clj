
(require '[clojure.core.reducers :as r])

; Declare functions to make compiler happy
(declare parse-if)
(declare binop?)
(declare parse-binop)
(declare parse-app)
(declare parse-lam)
(declare parse)
(declare desugar-arg)
(declare desugar-param)
(declare desugar-with)
(declare interp)
(declare interp-fn)
(declare interp-binop)
(declare check-equal?)

; Test Function
(def check-equal?
    (fn [_test result]
        (if (= _test result)
            (println "pass")
            (println "fail")
        )
    )
)

; ExprC defs
(defrecord NumC [n])
(defrecord BoolC [b])
(defrecord IdC [x])
(defrecord IfC [condition then else])
(defrecord BinopC [o l r])
(defrecord AppC [f a])
(defrecord LamC [p b])

; Value defs
(defrecord BoolV [b])
(defrecord CloV [params body clo-env])
(defrecord NumV [n])

; Default environment
(def top-env {})

; Parse IfC's
(def parse-if
    (fn [sexp]
        (let [
            c (parse (first (rest sexp)))
            t (parse (first (rest (rest sexp))))
            e (parse (first (rest (rest (rest sexp)))))
        ]
            (IfC. c t e)
        )
    )
)

; Check if symbol is a binop
(def binop?
    (fn [o]
        (if (or
            (= o '+)
            (= o '-)
            (= o '*)
            (= o '/)
            (= o '<=)
            (= o 'eq?)
        )
            true
            false
        )
    )
)

; Parse BinopC's
(def parse-binop
    (fn [sexp]
        (let [
            o (first sexp)
            l (parse (first (rest sexp)))
            r (parse (first (rest (rest sexp))))
        ]
            (BinopC. o l r)
        )
    )
)

; Parse AppC's
(def parse-app
    (fn [sexp]
        (let [
           f (parse (first sexp))
           a (map parse (rest sexp))
        ]
           (AppC. f a)
        )
    )
)

; Parse LamC's
(def parse-lam
    (fn [sexp]
        (let [
            p (first (rest sexp))
            b (parse (last sexp))
        ]
            (LamC. p b)
        )
    )
)

; Desugar the arg for with
(def desugar-arg
   (fn [sexp]
      (parse (last sexp))
   )
)

; Desugar the param for with
(def desugar-param
   (fn [sexp]
      (first sexp)
   )
)

; Desugar with
(def desugar-with
    (fn [sexp]
        (let [
            v (rest (butlast sexp))
            b (parse (last sexp))
        ]
            (AppC. (LamC. (map desugar-param v) b) (map desugar-arg v))
        )
    )
)

; Main parsing fuction
; List -> ExprC's
(def parse
    (fn [sexp]
        (if (number? sexp)
            (NumC. sexp)
        (if (or (= sexp 'true) (= sexp 'false))
            (BoolC. sexp)
        (if (symbol? sexp)
            (IdC. sexp)
        (if (= (first sexp) 'if)
            (parse-if sexp)
        (if (= (first sexp) 'with)
            (desugar-with sexp)
        (if (= (first sexp) 'lam)
            (parse-lam sexp)
        (if (binop? (first sexp))
            (parse-binop sexp)
            (parse-app sexp)
        )))))))
    )
)

(check-equal? (parse '5) (NumC. 5))
(check-equal? (parse 'true) (BoolC. true))
(check-equal? (parse 'false) (BoolC. false))
(check-equal? (parse 'x) (IdC. 'x))
(check-equal? (parse '(if true 1 0)) (IfC. (BoolC. true) (NumC. 1) (NumC. 0)))
(check-equal? (parse '(+ 1 2)) (BinopC. '+ (NumC. 1) (NumC. 2)))
(check-equal? (parse '(lam (a b c) 3)) (LamC. (list 'a 'b 'c) (NumC. 3)))
(check-equal? (parse '((lam (a b c) 3) 1 2 3)) (AppC. (LamC. (list 'a 'b 'c) (NumC. 3)) (list (NumC. 1) (NumC. 2) (NumC. 3))))
(check-equal? (parse '(with (z = (+ 9 14)) (y = 98) (+ z y)))
   (AppC. (LamC. (list 'z 'y) (BinopC. '+ (IdC. 'z) (IdC. 'y))) (list (BinopC. '+ (NumC. 9) (NumC. 14)) (NumC. 98))))

; Interp takes in an ExprC and evaluates it.
; (: interp (ExprC map -> Value))
(def interp (fn [a env]
   ; BoolC
   (if (instance? BoolC a)
     (BoolV. (:b a))

     ; NumC
     (if (instance? NumC a)
        (NumV. (:n a))

        ; IdC
        (if (instance? IdC a)
          (if (contains? env (:x a))
            (get env (:x a))
            ((print "DFLY: No variable called ") (print (:x a))))

          ; IfC
          (if (instance? IfC a)
            (let [condition (interp (:condition a) env)]
              (if (instance? BoolV condition)
                (if (:b condition)
                     (interp (:then a) env)
                     (interp (:else a) env))
                ((print "DFLY: An if statement condition
                       evaluated to something other than true or false: ")

                (print condition ))))
              (if (instance? BinopC a)
                  (interp-binop a env)
                  ; LamC and AppC
                  (interp-fn a env)
              )
            )

   )))))

; Interpt the binops
(def interp-binop
    (fn [b env]
        (let [
            o (:o b)
            l (interp (:l b) env)
            r (interp (:r b) env)
        ]
            (if (and (= o '+) (instance? NumV l) (instance? NumV r))
                (NumV. (+ (:n l) (:n r)))
            (if (and (= o '-) (instance? NumV l) (instance? NumV r))
                (NumV. (- (:n l) (:n r)))
            (if (and (= o '*) (instance? NumV l) (instance? NumV r))
                (NumV. (* (:n l) (:n r)))
            (if (and (= o '/) (instance? NumV l) (instance? NumV r) (not (= (:n r) 0)))
                (NumV. (/ (:n l) (:n r)))
            (if (and (= o '<=))
                (if (and (instance? NumV l) (instance? NumV r))
                    (BoolV. (<= (:n l) (:n r)))
                    (BoolV. false)
                )
            (if (and (= o 'eq?))
                (if (and (instance? NumV l) (instance? NumV r))
                     (BoolV. (= (:n l) (:n r)))
                     (BoolV. false)
                )
                (throw (Exception. "binop error"))
            ))))))
        )
    )
)

(check-equal? (interp (BinopC. '+ (NumC. 5) (NumC. 3)) top-env) (NumV. 8))
(check-equal? (interp (BinopC. '- (NumC. 5) (NumC. 3)) top-env) (NumV. 2))
(check-equal? (interp (BinopC. '* (NumC. 5) (NumC. 3)) top-env) (NumV. 15))
(check-equal? (interp (BinopC. '/ (NumC. 9) (NumC. 3)) top-env) (NumV. 3))
(check-equal? (interp (BinopC. '<= (NumC. 9) (NumC. 3)) top-env) (BoolV. false))
(check-equal? (interp (BinopC. '<= (NumC. 3) (NumC. 9)) top-env) (BoolV. true))
(check-equal? (interp (BinopC. 'eq? (NumC. 9) (NumC. 3)) top-env) (BoolV. false))
(check-equal? (interp (BinopC. 'eq? (NumC. 9) (NumC. 9)) top-env) (BoolV. true))

; Take in an outer environment and a lambda, and spit out the new environment
;  the body of the lambda will use. It combines the outer env with the parameters
;  from the lambda.
; (: prepare-env (CloV (List Value) Env -> Env))
(def prepare-env (fn [closure args outer-env]
  (when (not (= (count (:params closure)) (count args)))
    (throw (Exception. (format "DFLY: Function expected %s args, you gave %s args"
                               (count (:params closure)) (count args)))))

(reduce (fn [curr-hash vec-arg-param]
            (print "Are you a vector: ")
            (print (vector? vec-arg-param))
            (assoc curr-hash (nth vec-arg-param 1) (nth vec-arg-param 0))
            ) outer-env (map vector args (:params closure)))))


(println (prepare-env
          (CloV. '(a b)
                 (BinopC. '+ (IdC. 'a) (IdC. 'b))
                 top-env)
           (list (NumV. 4) (NumV. 5))
           {'x (NumV. -1)
            'y (NumV. 37)}))

            ; Expect the hash: {'a (NumV. 4)
            ;                    'b (NumV. 5)
            ;   'x (NumV. -1)
            ;   'y (NumV. 37)}


;(reduce (fn [a-b curr-hash] (assoc curr-hash (nth a-b 1) (nth a-b 0))) {} (map vector '(1 2 3) '(4 5 6)))

; Helper function that interps LamCs and AppCs
; (: interp-fn (ExprC map -> Value))
(def interp-fn (fn [a env]
   ; LamC
   (if (instance? LamC a)
     (CloV. (:p a) (:b a) env)

     ; AppC
     (if (instance? AppC a)
       (let [fval (interp (:f a) env)
             argvals (map
                     (fn [arg] (interp arg env)) (:a a))]
         (when (not (instance? CloV fval))
           (throw (Exception. (format "DFLY: There was an AppC
that contained %s instead of a CloV" fval))))
         (interp (:body fval) )

         )
        (throw (Exception. (format "DFLY: Interped something that's not an ExprC: %s" a)))
       ))))

(println (interp (NumC. 3) top-env)) ; (NumV 3)
(println (interp (BoolC. true) top-env)) ; (BoolV true)
(println (interp (IfC. (BoolC. false) (NumC. 4) (NumC. 5)) top-env)) ; (NumV 5)

(println (interp (parse '(lam (a) 3)) top-env) (CloV. '(a) (NumC. 3) top-env))

(check-equal? (top-interp '{{if {eq? 3 3} 3 4}}) "3")
