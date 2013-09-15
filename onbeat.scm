;;--------
;  Sun Feb 17 02:07 GMT 2013
;  
;;--------




; =====================
; 
; 1. ATOMIC PREDICATES 
; Note: they accept only atomic values to match, not lists.
; 
; =====================




;;--------
;  
;  <BEAT?> 
;
;  Predicate that checks if the current beat matches the measure/downbeat passed. 
;  Note: 0-based
;
; (beat? 4 1)   => true every second beat in a four-beats bar
; (beat? 2 0)   => true every first beat in a two-beats bar 
; To match more than one beat:
; (or (beat? 2 0) (beat? 2 1))
;;--------

;;<signature>
(define beat? (lambda (measure downbeat) '()))
;;</signature>

(define-macro (beat? x y)
   `(equal? (modulo beat ,x) ,y))






;;--------
;  
;  <BEAT-GT?> 
;
;;--------

;;<signature>
(define beat-gt? (lambda (measure downbeat) '()))
;;</signature>

(define-macro (beat-gt? x y)
   `(> (modulo beat ,x) ,y))



;;--------
;  
;  <BEAT-LT?> 
;
;;--------

;;<signature>
(define beat-lt? (lambda (measure downbeat) '()))
;;</signature>

(define-macro (beat-lt? x y)
   `(< (modulo beat ,x) ,y))







;;--------
;  
;  <BEAT-BTW?> 
;
;(let ((beat (*metro* 'get-beat)))
;   (barlength 3 (beat-btw? 1 4)))
;
;;--------

;;<signature>
(define beat-btw? (lambda (measure downbeat1 downbeat2) '()))
;;</signature>

(define-macro (beat-btw? x y z)
   `(and (> (modulo beat ,x) ,y) (< (modulo beat ,x) ,z)))










; =====================
; 
; 2. CONDITIONAL STATEMENTS
; 
; =====================






;;--------
;  
;  <ONBEAT?> 
;
;  => (when (beat? n) (do....)) form.
;
;  Accepts both single numbers and lists
;
;
;; (let ((beat (*metro* 'get-beat)))      
;;   (onbeat? 2 0 (play dls 60 60 1))
;;   (onbeat? 2 1 (play dls 48 60 1)))
;
;;;--------

;;<signature>
(define onbeat? (lambda (measure downbeat &procedures) '()))
;;</signature>

(define-macro (onbeat? x y . args)
   `(for-each (lambda (step) 
                 (if (equal? (modulo beat ,x) step)
                     (begin ,@args)))
              (if (list? ,y)
                  ,y
                  (list ,y))))








;;--------
;  
;  <IFBEAT?>
;
;  =>  (if (beat? n) (do....) (else do.....)) form.
;
;  Accepts both single numbers and lists
;
;
;;;--------

;;<signature>
(define ifbeat? (lambda (measure downbeat procedure  &elseprocedure) '()))
;;</signature>

(define-macro (ifbeat? x y args . elseargs)
   `(for-each (lambda (step) 
                 (if (equal? (modulo beat ,x) ,y)
                     ,args
                     ,@elseargs))
              (if (list? ,y)
                  ,y
                  (list ,y))))





;;--------
;  
;  <IFBEAT-GT?>
;
;;;--------

;;<signature>
(define ifbeat-gt?  (lambda (measure downbeat procedure &elseprocedure) '()))
;;</signature>

(define-macro (ifbeat-gt? x y args . elseargs)
   `(if (> (modulo beat ,x) ,y)
        ,args
        ,@elseargs))







;;--------
;  
;  <IFBEAT-LT?>
;
;;;--------

;;<signature>
(define ifbeat-lt? (lambda (measure downbeat procedure &elseprocedure) '()))
;;</signature>

(define-macro (ifbeat-lt? x y args . elseargs)
   `(if (< (modulo beat ,x) ,y)
        ,args
        ,@elseargs))








;;--------
;  
;  <IFBEAT-BTW?>
;
;
;;;--------

;;<signature>
(define ifbeat-btw? (lambda (measure downbeat1 downbeat2 procedure &elseprocedure) '()))
;;</signature>

(define-macro (ifbeat-btw? x y z args . elseargs)
   `(if (and (> (modulo beat ,x) ,y) (< (modulo beat ,x) ,z))
        ,args
        ,@elseargs))



















				
				


