;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  cl:
;;  generic common lisp additions
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;




(define cl:first car)
(define cl:rest cdr)
(define cl:count length)

;; reverse of cons: (cons 'b '(a))
(define cl:l-put
   (lambda (obj lst)
      (reverse (cons obj (reverse lst)))))

;; dont know why but I like it reversed..
(define cl:nth (lambda (index_0_based lst)
               (list-ref lst index_0_based)))


;; OLD implemetation: mind that I was taking 1 as lowest index (while now nth takes 0)
;;; the scheme equivalent is list-ref
;(define cl:nth 
;   (lambda (index ls)
;      ; are we at the required index?
;      (if (= index 1) 
;          ; return the head
;          (car ls) 
;          ; keep going, decrement the index and make the list shorter
;          (cl:nth (- index 1) (cdr ls) ))))



(define (1+ n) (+ n 1))
(define (1- n) (- n 1))


;; (subst 9 7 '(5 (5 6 7(6 7))))    =>  (5 (5 6 9 (6 9)))      
(define (cl:subst new old tree)
  (if (pair? tree)
      (let ((left (subst new old (car tree)))
            (right (subst new old (cdr tree))))
        (if (and (eq? left (car tree))
                 (eq? right (cdr tree)))
            tree
            (cons left right)))
      (if (eqv? old tree)
          new
          tree)))



;; (sublis '((6 . 9) (7 . 10)) '(5 (5 6 7 (6 7)))))  => (5 (5 9 10 (9 10)))
(define (cl:sublis alist tree)
  (if (pair? tree)
      (let ((left (sublis alist (car tree)))
            (right (sublis alist (cdr tree))))
        (if (and (eq? left (car tree))
                 (eq? right (cdr tree)))
            tree
            (cons left right)))
      (let ((new (assv tree alist)))
        (if new
            (cdr new)
            tree) ) ) )


;; (copy-tree '(5 (5 6 7(6 7))))
(define (cl:copy-tree x)
  (if (pair? x)
      (cons (copy-tree (car x))
            (copy-tree (cdr x)))
      x))


; Convert a floating-point number to a string of sign and at most 4 characters.
; Rounds the number so that 1.999 will come out as 2.00 , very small as 0.0 .
; numstring is written assuming that num is not too large or too small,
; i.e. num must be printable in 4 digits.
(define (cl:numstring num)
  (let* ((numc (abs num)) (sign (if (< num 0) -1 1)) (exponent 0))
    (if (< numc 1.0e-6)
    "0.0"
    (begin
      (if (< numc 1.0)
          (begin (while (< numc 100)
                (set! numc (* numc 10))
                (set! exponent (1- exponent)))
             (set! numc (* (round numc) (expt 10 exponent))) )
          (set! numc (* numc 1.0001)))
      (if (< sign 0)
          (string-append "-"
                 (substring (number->string numc) 0
                   (min 4 (string-length (number->string numc)))))
          (substring (number->string numc) 0
             (min 4 (string-length (number->string numc))))) ) ) ))




;(list-flatten '(9 9 (9 9 9 ))))  = (9 9 9 9 9)

(define cl:list-flatten 
   (lambda (l)
      (cond ((null? l)
             '())
            ((atom? l)
             (list l))
            (#t (append (cl:list-flatten  (car l)) (cl:list-flatten  (cdr l)))))))


;; added on 21/1/11 cause the default one wasn't working!
(define cl:last (lambda (lista)
                   (list-ref lista (- (length lista) 1))))




;; tip for shuffling a list
(define (cl:shuffle l)
  (cl:sort l
        (lambda (x y)
          (equal? 0 (random 2)))))



;; given a list returns a new one with the last element duplicated N times (thus increasing the length of the initial list!)
(define cl:expand-list
   (lambda (l n)
      (if (list? l)
          (let ((xx (make-list n (cl:last l))))
             (append l xx))
          (let ((xx (make-list n l)))
             xx))))

;;;;;; better version ==> 
;; given a list or atom replicates the last el all the times needed reach N length
;;(cl:expand-list2 '(50 30) 5)
(define cl:expand-list2 
   (lambda (l n)
      (if (list? l)
          (let ((xx (make-list (- n (length l)) (cl:last l))))
             (append l xx))
          (let ((xx (make-list n l)))
             xx))))




;; (list-to-string '(5 6 7))
;; from a list returns a string equivalent  - similar to atom->string..
;; there is list->string too, but I don't know how to use it!
(define cl:list-to-string 
   (lambda (lst)
      (let* ((out "(")
             (nlst (map (lambda (x)
                           (atom->string x))
                        lst)))
         (map (lambda (x)
                 (set! out (string-append out " " x)))
              nlst)
         (set! out (string-append out " )"))
         out)))





; slice a list from beginning, returns a list
(when #f (cl:slice-left '(1 2 3 4) 2))

(define cl:slice-left
    (lambda (lst num)
       (if (< (length lst) num)
           (print "Error: list slicing index too big!")
           (if (> num 0)
               (cons (car lst) (cl:slice-left (cdr lst) (- num 1)))
               '())))) ;'


; slice a list from specified position, returns a list
(when #f (cl:slice '(1 2 3 4) 2 2))

(define cl:slice
    (lambda (lst start count)
        (if (> start 1)
            (cl:slice (cdr lst) (- start 1) count)
            (cl:slice-left lst count))))










;;;;;;;;;;;;;;;;;;;
;STRING HELPERS
;;;;;;;;;;;;;;;;;;;


;; returns a char from a string of length 1, or a list of chars from a longer string
;;; utility on top of string->list
(define cl:char 
   (lambda (string_char)
      (if (string? string_char)
          (if (> (string-length string_char) 0)
              (if (> (string-length string_char) 1)
                  (string->list string_char)
                  (car (string->list string_char))))            
          (print 'please 'enter 'a 'string))))



;; matches a single character in a string, and replaces it
(define cl:string-replace 
   (lambda (s match replacement)
      (let ((ll (string->list s))
            (match1 (cl:char match))
            (replacement1 (cl:char replacement)))
         (if (= (string-length match) 1)
             (let ((z (map (lambda (x)
                              (if (equal? x match1)
                                  replacement1
                                  x))
                           ll)))
                (list->string (cl:list-flatten z)))
                ;z)
             (print "i can match only single characters for now")))))



;; makes a string upper case
(define cl:string-capitalize
   (lambda (s)
      (string-append (string (char-upcase (string-ref s 0))) (substring s 1 (string-length s)))))


;; makes a string lower case
(define cl:string-lower
   (lambda (s)
      (let ((out ""))
         (for-each (lambda (x)
             (set! out (string-append out (string (char-downcase x)))))
          (string->list s))
         out)))


;; makes a string upper case
(define cl:string-upper
   (lambda (s)
      (let ((out ""))
         (for-each (lambda (x)
             (set! out (string-append out (string (char-upcase x)))))
          (string->list s))
         out)))



;; works only with single chars for now
;; return the position of a char in a string
(define cl:string-find-char 
   (lambda (z singlechar)
      (if (> (string-length singlechar) 1)
          (print "can't match strings longer than 1 char")
          (let p ((x 1))
             (let ((l (string->list z)))
                (if (equal? (cl:nth x l) (cl:char singlechar))
                    x
                    (if (< x (length l))
                        (p (+ x 1))
                        #f)))))))



(if #f (cl:string-find-char "M 3.4, Central California" "M"))



;; hack for checking is a string exists in another string
(define cl:string-find
   (lambda (s find)
      (if (> (string-length s)
             (string-length (string-replace s find "")))
          #t
          #f)))





;; divides a string on a selected char, left or right side
;; returns false if the singlechar is not found
;; the singlechar is excluded from the resulting string
(define cl:string-split 
   (lambda (z singlechar left-or-right)
      (if (> (string-length singlechar) 1)
          (print "can't match strings longer than 1 char")
          (let ((pos (cl:string-find z singlechar)))
             (if pos
                 (cond ((equal? left-or-right 'left)
                        (substring z 0 (- pos 1)))
                       ((equal? left-or-right 'right)
                        (substring z pos (string-length z))))
                 #f)))))

(if #f (cl:string-split "M 3.4, Central California" "," 'right))



(define cl:string-chunk
   (lambda (z char_left char_right)
      (if (or (> (string-length char_left) 1) (> (string-length char_right) 1))
          (print "can't use matching strings longer than 1 char")
          (let ((left_half (cl:string-split z char_left 'right)))
             (if left_half
                 (cl:string-split left_half char_right 'left)
                 #f)))))

(if #f (cl:string-chunk "M 3.4, Central California" "M" ","))





;; extension of string->number: eliminates white spaces
;; otherwise (print (string->number " 5.6 ")) would given an error
(define cl:string->number 
   (lambda (s)
      (string->number (list->string (remove-all (cl:char " ") (string->list s))))))



;; separates words delimited by empty spaces
(define cl:string-tokenize 
   (lambda (s)
      (string-split s " ")))



;; convoluted way to check if a ratio is a perfect integer. (un intero)
;; (todo: improve?)
;; essenzialmente creo una stringa (from the ratio) , poi la splitto, poi controllo che la seconda parte sia uguale a 0 trasformandola in numero di nuovo
(define cl:ratio_is_perfect
   (lambda (f)
      (equal? 0 (string->number (cadr (string-split (number->string (rational->real f)) "." ))))))








;;;;;;;;;;;;;;;;;;;
;MACROS 
;;;;;;;;;;;;;;;;;;;


(define-macro (def name args)
   `(define ,name ,args))

(define-macro (cl:sort-asc l)
   `(cl:sort ,l <))
(define-macro (cl:sort-desc l)
   `(cl:sort ,l >))



; shortcut for determining lists of equal length: takes both a list or a number for comparison
(define-macro (length-equal? x y)
   `(cond ((and (list? ,x) (list? ,y))
       	   (equal? (length ,x) (length ,y)))
           ((and (list? ,x) (number? ,y))
       	   (equal? (length ,x) ,y))
           (#t (print 'Error: 'length-equal? 'needs 'two 'lists 'or 'one 'list 'and 'a 'number))))



; ONEOF: like random, but doesn't requires parenthesis :  (oneof c5 c6 c7)
; (oneof 5 6)  => 5 or 6
; (oneof 5)  => 5 
; (oneof '(5 6))  => 5 or 6 
; (oneof c5 c6)   => 72 or 84
; (oneof (--domaggiore 45 8)) ==> a note in the scale

(define oneof (lambda (elements) '()))

(define-macro (oneof . args)
   (cond ((length-equal? args 0)
          (print 'please 'provide 'arguments))
         ((length-equal? args 1)
          `(cond ((list? ,@args)
                  (random ,@args))
                 (#t ,@args)))
         (#t `(random (list ,@args)))))





;; http://download.plt-scheme.org/doc/372/html/t-y-scheme/t-y-scheme-Z-H-10.html#node_chap_8
;; ----------------------
(define if-random (lambda (probability procedure . elseprocedure) '()))
;; (if-random .6 (print 'ciao) (print 'baby))
(define-macro (if-random x args . elseargs)
   `(if (> (random) ,x)
        ,args 
        ,@elseargs))


(define when-random (lambda (probability procedure . elseprocedure) '()))
;; (when (> (random) .1) (print 'ca) (print 'ciao))
(define-macro (when-random x args . elseargs)
   `(when (> (random) ,x)
        ,args 
        ,@elseargs))

(define chance (lambda (probability procedure . elseprocedure) '()))
(define chance when-random)
(define ifchance (lambda (probability procedure . elseprocedure) '()))
(define ifchance if-random)



(define if-cdr-notnull (lambda (list_totry_cdr alternative_list) '()))
;; if you can apply cdr to arg1, do it and return that, otherwise return arg2
(define-macro (if-cdr-notnull list1 list2)
   `(if (null? (cdr ,list1))
        ,list2
        (cdr ,list1)))


; enhanced version of cors that also floors the value (eg useful for generating notes, or precise beats)
(macro (cosrfloor args)
   (if (> (length args) 5)
       `(floor (+ ,(caddr args) (* ,(cadddr args) (cos (* 2pi (+ beat ,(cadr args)) ,(car (cddddr args)))))))
       `(floor (+ ,(cadr args) (* ,(caddr args) (cos (* 2pi beat ,(cadddr args))))))))


;; same but with ratios
(macro (cosratio args)
   (if (> (length args) 5)
       `(real->rational (+ ,(caddr args) (* ,(cadddr args) (cos (* 2pi (+ beat ,(cadr args)) ,(car (cddddr args)))))))
       `(real->rational (+ ,(cadr args) (* ,(caddr args) (cos (* 2pi beat ,(cadddr args))))))))







