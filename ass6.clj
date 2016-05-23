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
(declare interp-fn)

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
(def top-env {:+ :+Prim
              :- :-Prim
              :* :*Prim
              :div :divPrim
              :<= :<=Prim
              :eq? :eq?Prim})

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
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(println (= (parse '5) (NumC. 5)))
(println (= (parse 'true) (BoolC. true)))
(println (= (parse 'false) (BoolC. false)))
(println (= (parse 'x) (IdC. 'x)))
(println (= (parse '(if true 1 0)) (IfC. (BoolC. true) (NumC. 1) (NumC. 0))))
(println (= (parse '(+ 1 2)) (BinopC. '+ (NumC. 1) (NumC. 2))))
(println (= (parse '(lam (a b c) 3)) (LamC. (list 'a 'b 'c) (NumC. 3))))
(println (= (parse '((lam (a b c) 3) 1 2 3)) (AppC. (LamC. (list 'a 'b 'c) (NumC. 3)) (list (NumC. 1) (NumC. 2) (NumC. 3)))))
(println (= (parse '(with (z = (+ 9 14)) (y = 98) (+ z y)))
   (AppC. (LamC. (list 'z 'y) (BinopC. '+ (IdC. 'z) (IdC. 'y))) (list (BinopC. '+ (NumC. 9) (NumC. 14)) (NumC. 98)))))


; All the primitive functions live here!

; Primitive plus
; (: my+ ((list NumV NumV) -> NumV)
(def my+ (fn [args]
           ))

(def my- (fn [args]
          ))

(def primitive-fundefs
  {:+Prim my+
   :-Prim my-})


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

              ; LamC and AppC
              (interp-fn a env)
            )

   )))))


; Helper function that interps LamCs and AppCs
; (: interp-fn (ExprC map -> Value))
;(def interp-fn (fn [a env]
;   ; LamC
 ;  (if (instance? LamC a)
;     (CloV. (:p a) (:b a) env)

     ; AppC
;     (if (instance? AppC a)
;       (let [fval (interp (:f a) (:a a) env)
;             argvals (map )]))
;     )))

(println (interp (NumC. 3) top-env)) ; (NumV 3)
(println (interp (IdC. :+) top-env)) ; :+Prim
(println (interp (BoolC. true) top-env)) ; (BoolV true)
(println (interp (IfC. (BoolC. false) (NumC. 4) (NumC. 5)) top-env)) ; (NumV 5)
