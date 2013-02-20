;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  misc:
;;  miscellaneous stuff 
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;









;;;;;;;;;;;;;;;
;;  OO programming
;;;;;;;;;;;;;;;

(define oo:make-object
   (lambda ()
      (let* ((klassname '<object>)
             (super #f)
             (isa (lambda (t)
                     (if (equal? t klassname)
                         #t
                         (if super
                             (super 'isa t)
                             #f))))
             (dispatch (lambda (msg methods)
                          (if (assoc msg methods)
                              (cdr (assoc msg methods))
                              (begin (print-error 'No 'such 'method) (error "")))))
             (get-method (lambda (msg) (cdr (assoc msg methods))))
             (add-method (lambda (name closure) ;; for mixings
                            (set! methods (cons (cons name closure) methods)) #t))
             (get-methods (lambda () methods))
             (printer (lambda () (print "object")))
             (methods (list (cons 'isa isa)
                            (cons 'get-method get-method) (cons 'add-method add-method)
                            (cons 'dispatch dispatch) (cons 'get-methods get-methods)))
             (self (lambda (msg . args)
                      (apply sys:dynamic-call
                             ((cdr (assoc 'dispatch methods)) msg methods)
                             args))))
         self)))











;;;;;;;;;;;;;;;
;;  GRAPHICS functions
;;;;;;;;;;;;;;;





;==>EG: given a *canvas*...
;(let ((p (gra:rectangle_points 20 50 30 (list 100 100 *canvas_max_x* *canvas_max_y*) 'horizontal)))
;   (for-each (lambda (x)
;                (gfx:draw-path (now) *canvas* (gfx:make-rectangle (cl:nth 0 x) (cl:nth 1 x) (cl:nth 2 x) (cl:nth 3 x))
;                               '(0 0 0 1) '(0 0 1 0) 2.0))
;             p))

; ====> returns a nested list like this (the last number is the index of the button):
;((110.000000 110 30.000000 30 0) 
; (145.000000 110 30.000000 30 1) 
; (180.000000 110 30.000000 30 2) 
;etc.. )

(define gra:rectangle_points
   (lambda (howmany size_x size_y boundaries orientation)
      (let* ((startx (car boundaries))
             (starty (cadr boundaries))
             (maxx (caddr boundaries))
             (maxy (cadddr boundaries))
             (width (- maxx startx)) ;; the width of the drawable area
             (height (- maxy starty))  ;; the height of the drawable area
             (span (if (equal? orientation 'horizontal)
                       (/ width howmany)
                       (/ height howmany))) ;; = the interval
             (padding 5);; used to determine the relative max size if size is too big
             (ssize_x (if (equal? orientation 'horizontal)  ;;check which is the right 'lato' to check
                          (if (> size_x (- span padding))
                              (- span padding) size_x)
                          size_x))
             (ssize_y (if (equal? orientation 'horizontal) 
                          size_y
                          (if (> size_y (- span padding))
                              (- span padding) size_y)))
             (indent 10)) ;; not used
         ;(set! +seq_points_container+ '())
         (let innerloop ((i 0)
                         (out '()))
            (if (< i howmany)
                (let ((xx (if (equal? orientation 'horizontal)
                              (+ indent (* span i) startx)
                              (+ indent startx)))
                      (yy (if (equal? orientation 'horizontal)
                              (+ indent starty)
                              (+ indent (* span i) starty))))
                   (innerloop (+ i 1) (append out (list (list xx yy ssize_x ssize_y i))))) ;; the last i gives me a button-INDEX !!
                out)))))





