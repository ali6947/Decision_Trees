#lang racket

(require "decision_functions.rkt")
;(require "decision_functions_sig.rkt")
(require 2htdp/batch-io)
;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

(provide titanicout)
(define titanicout "../output/titanic-decision-tree.dot")

(provide mushroomout)
(define mushroomout "../output/mushroom-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
;(define init1
;  (cdr (string-split  (call-with-input-file  toytrain (lambda (x) (port->string x))))))
;  (define (convert2 currnum str)
;  (cond ((= 0 (string-length str)) (list (list->string currnum)))
;        ( ( equal? #\, (string-ref str 0)) ( cons (list->string currnum) (convert2 '() (substring str 1))))
;        (else (convert2 (append currnum (list (string-ref str 0))) (substring str 1)))))
;  (define toy-raw (map (lambda (t) (convert '() t)) init1) )
(define toy-raw (cdr (read-csv-file toytrain)))
  

;(define titanictrain "../data/titanic_train.csv")
(provide titanic-raw)
;(define (init2c cur cl ce ) ; li is current element being formed sp tells if second \ has been crossed acc is full list , cur is current data
;(cond  ( ( = 0 (string-length  cur)) (list (append cl (list (list->string ce )))))
;       ( ( equal? #\return (string-ref cur 0)) (cons  (append cl (list (list->string ce )))  (init2c (substring cur 2) '()  '())))
;       ( ( equal? #\newline (string-ref cur 0)) (cons  cl  (init2c (substring cur 1) '()  '())))
;       ( ( equal? #\, (string-ref cur 0)) (init2c (substring cur 1) (append cl (list (list->string ce ))) '() ))
;       
;       ( else (init2c (substring cur 1) cl (append ce (list (string-ref cur 0) ))) )  
;       ))
;(define (filter1 l) (filter string->number l) )
;
;(define titanic-raw (map cdr (map filter1 (cdr (init2c (string-trim (call-with-input-file  titanictrain (lambda (x) (port->string x)))) '() '() ) ) )))
(define titanic-raw (map cddr (cdr (read-csv-file titanictrain) )))

(provide mushroom-raw)
(define mushroom-raw  (cdr (read-csv-file mushroomtrain) )) 

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data) (cons (map string->number (cdr data)) (string->number (car data))))

;list of (features . result)
(provide toy)
(define toy (map format toy-raw))

(provide titanic)
(define titanic (map format titanic-raw))

(provide mushroom)
(define mushroom (map format mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (/ (foldr (lambda (x y) (if (= 1 (cdr x)) ( + y 1) y)) 0 data) (foldr (lambda (x y)  ( + y 1) ) 0 data)) 
  )

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  ( let ([ p (get-leaf-prob data)])
    ( if ( or ( = p 1 ) ( = p 0)) 0 ( - ( + ( * p (log p 2)) ( * ( - 1 p ) ( log ( - 1 p ) 2))))
  )))

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff)
(provide tablec)
(define (tablec f data acc)
  ( cond ((null? data ) acc )
         ( (index-where acc ( lambda (t) (equal? (car t) (f (caar data))))) (tablec f (cdr data) (list-update acc (index-where acc ( lambda (t) (equal? (car t) (f (caar data)))))
                                                                       (lambda (p) ( list-set p ( + 1 (cdar data)) ( + 1 (list-ref p ( + 1 (cdar data)))))))))
         (else    (tablec f (cdr data)  
                       (if ( = 0 (cdr (car data))) (cons (list ( f ( caar data)) 1 0) acc) (cons (list ( f ( caar data)) 0 1) acc))))))
(define (entropy-diff f data)
  ( let ( [ inp (tablec f data '())]
        ( l (length data)))
    (define (find-entropy x)
      ( match x [ (list a p n ) (let ( [ y (/ p ( + p n))]) ( if (or (= y 1) (= y 0)) 0
                                                     (* (/ ( + p n) l) ( - ( + ( * y (log y 2)) ( * ( - 1 y ) ( log ( - 1 y ) 2)))))))]))
     ( - (get-entropy data ) (foldr (lambda (x y) (+ (find-entropy x) y)) 0 inp) )
  ))

;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
  (argmax (lambda (p) (entropy-diff (cdr p) data)) candidates) 
  )

(provide DTree)
(struct DTree (desc func kids) #:transparent)

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
  (define (helper cf cdata cd lbest ) ; current candidate func , cdata current data cd- current depth , iehow many dec func have been incopr till now
    ( cond ( ( or ( null? cf ) ( = cd depth) ( = 1 (get-leaf-prob cdata) ) ( = 0 (get-leaf-prob cdata) ))
             (DTree  (cons ((cdr lbest) (caar cdata )) (number->string (get-leaf-prob cdata))) 0 '() ) )
           ( else ( let* ( [ best (choose-f cf cdata)]
                     [ segdata (sort (group-by (lambda (t) ((cdr best) (car t)))  cdata ) < #:key ( lambda (t) ( (cdr best) ( caar t))))])
                     
                     ( DTree (cons ((cdr lbest) (caar cdata )) (number->string (get-leaf-prob cdata)))
                      best (map (lambda (t) (helper (remove best cf) t ( + 1 cd ) best)) segdata ))))))
  (helper candidates data 0 (cons "init" (lambda (l) "bg")) ))

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test)
  ( match tree [ (DTree d f k ) (cond ((null? k) (string->number (cdr d)))
                                      ( ( index-where k (lambda (t) (equal? ((cdr f) test) (car (DTree-desc t)))))
                                    (make-decision (list-ref k ( index-where k (lambda (t) (equal? ((cdr f) test) (car (DTree-desc t))))))  test))
                                      (else 0  ))])
  )

;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
;(define (pair-idx lst n)
;  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
;  )
;
;;generate tree edges (parent to child) and recurse to generate sub trees
;(define (dot-child children prefix tabs)
;  (apply string-append (map (lambda (t)  (string-append tabs "r" prefix "--" "r" prefix (~a (cdr t)) "[label=\"" (~a (cdr t)) "\"];" "\n" (dot-helper (car t) (string-append prefix (~a (cdr t))) (string-append tabs "\t")))) children))
;  )
;
;;generate tree nodes and call function to generate edges
;(define (dot-helper tree prefix tabs)
;  (let* ([node (match tree [(DTree d f c) (cons d c)])]
;         [f (car node)]
;         [c (cdr node)])
;    (string-append tabs "r" prefix "[label=\"" f "\"];" "\n\n" (dot-child (pair-idx c 0) prefix tabs))
;    )
;  )
;
;;output tree (dot file)
;(provide display-tree)
;(define (display-tree tree dtfile)
;  (write-file dtfile (string-append "graph \"decision-tree\" {" "\n" (dot-helper tree "" "\t") "}"))
;  )

;annotate list with indices
(define (pair-idx lst n)
  ""
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  ""
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  ""
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree dtfile)
  ""
  )
;============================================================================================================
;============================================================================================================
;============================================================================================================