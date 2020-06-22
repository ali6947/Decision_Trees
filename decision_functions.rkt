#lang racket

;candidate functions for the toy dataset
(provide y1)
(provide y2)
(provide y3)
(provide y4>62)

(define y1  (cons "feature1" (lambda (p) (list-ref p 0 )))) ; returns the value of feature 1 for a given test sample
(define y2  (cons "feature2" (lambda (p) (list-ref p 1 ))))
(define y3  (cons "feature3" (lambda (p) (list-ref p 2 ))))
(define y4>62  (cons "feature4>62" (lambda (p) ( if ( > (list-ref p 3 ) 62 )
                                                1 0)))) ; returns 1 if the value of feature 4 > 62, else 0

;candidate functions for the titanic dataset
(provide pclass)
(provide sex)
(provide age>25)
(provide sibsp)
(provide parch)
(provide fare>50)
(provide emb)

(define pclass  (cons "pclass" (lambda (p) (list-ref p 0 )))) ; returns the value of pclass for a given test sample
(define sex  (cons "sex" (lambda (p) (list-ref p 1 ))))
(define age>25 (cons "age>25" (lambda (p) ( if ( >  (list-ref p 2 ) 25 )
                                                1 0))))
(define sibsp (cons "sibsp" (lambda (p) (list-ref p 3 ))))
(define parch (cons "parch" (lambda (p) (list-ref p 4 ))))
(define fare>50  (cons "fare>50" (lambda (p) ( if ( > (list-ref p 5 ) 50 )
                                                1 0))))
(define emb  (cons "emb" (lambda (p) (list-ref p 6 ))))

;candidate functions for the mushroom dataset
(provide cshape)
(provide csurf)
(provide bruise)
(provide odor)
(provide gatch)
(provide gspace)
(provide gsize)
(provide sshape)
(provide nring)
(provide pop)
(provide hab)

(define cshape (cons "cshape" (lambda (p) (list-ref p 0 ))))
(define csurf  (cons "csurf" (lambda (p) (list-ref p 1 ))))
(define bruise  (cons "bruise" (lambda (p) (list-ref p 2 ))))
(define odor  (cons "odor" (lambda (p) (list-ref p 3 ))))
(define gatch  (cons "gatch" (lambda (p) (list-ref p 4 ))))
(define gspace (cons "gspace" (lambda (p) (list-ref p 5 ))))
(define gsize (cons "gsize" (lambda (p) (list-ref p 6 ))))
(define sshape (cons "sshape" (lambda (p) (list-ref p 7 ))))
(define nring (cons "nring" (lambda (p) (list-ref p 8 ))))
(define pop (cons "pop" (lambda (p) (list-ref p 9 ))))
(define hab (cons "hab" (lambda (p) (list-ref p 10 ))))
