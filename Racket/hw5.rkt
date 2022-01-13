;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 


;; Problem 1
; part a
(define (racketlist->mupllist rs)
  (if (empty? rs)
      (aunit)
      (apair (car rs) (racketlist->mupllist (cdr rs)))))

; part b
(define (mupllist->racketlist ms)
  (if (aunit? ms)
      null
      (cons (apair-e1 ms) (mupllist->racketlist (apair-e2 ms)))))

;; Problem 2

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

; MUPL interpreter
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))] ; var-string is getting the string part of var e
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(fun? e) (closure env e)]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater condition applied to non-number")))]
        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
                [new-env (cons (cons (mlet-var e) v) env)])
          (eval-under-env (mlet-body e) new-env))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([cenv (closure-env v1)]
                      [cfun (closure-fun v1)]
                      [fname (fun-nameopt cfun)]
                      [new-env (cons (cons (fun-formal cfun) v2)
                                     (if fname
                                         (cons (cons fname v1) cenv)
                                         cenv))])
                 (eval-under-env (fun-body cfun) new-env))
               (error "MUPL call funexp applied to non-closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))


(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
; part a
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

; part b
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([hd (car lstlst)]
            [tl (cdr lstlst)])
        (mlet (car hd) (cdr hd) (mlet* tl e2)))))

; part c
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4
; part a
(define mupl-map
  (fun #f
       "mf"
       (fun "mmap"
            "ml"
            (ifaunit (var "ml")
                     (aunit)
                     (apair (call (var "mf") (fst (var "ml")))
                            (call (var "mmap") (snd (var "ml"))))))))

; part b
(define mupl-mapAddN 
  (mlet "map"
        mupl-map
        (fun "addi"
             "i"
             (call (var "map")
                   (fun #f "x" (add (var "x") (var "i")))))))


