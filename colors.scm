;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  colors:
;;  functions for facilitatin working with colors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;




;; just a bunch of colors variables for a faster prototypying


(define color:black '(0 0 0 1))
(define color:white '(1 1 1 1))
(define color:red '(1 0 0 1))
(define color:green '(0 1 0 1))
(define color:yellow '(1 1 0 1))
(define color:blue '(0 0 1 1))
(define color:brown '(.2 0 0 1))

(define color:random (lambda () (list (random) (random) (random) (random))))

;; eg:
(if #f
    (gfx:clear-canvas (now) *canvas* (color:random)))