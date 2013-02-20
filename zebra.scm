;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  zb:
;;  functions for working with Zebra instrument
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; mappings

(define *zb:main_volume* 0)
(define *zb:x1* 3)
(define *zb:y1* 4)
(define *zb:x2* 5)
(define *zb:y2* 6)
(define *zb:x3* 7)
(define *zb:y3* 8)
(define *zb:x4* 9)
(define *zb:y4* 10)


;;; (au:set-param (now) zebra4 3 0 0 80)
;; todo: create a version with beat assumed to be there

;;(zb:setvolumenow zebra4 15)
;; (equal? (zb:volume zebra1 'show) 6)
(define zb:volume
   (lambda (zeb x . args)
      (cond ((number? x)
             (au:set-param (now) zeb 0 0 0 x))
            ((equal? x 'show)
             (au:get-param zeb 0 *au:global-scope* 0))
            ((equal? x '+)
             (let ((vol (au:get-param zeb 0 *au:global-scope* 0)))
                (au:set-param (now) zeb 0 0 0 
                              (if (null? args)
                                  (if (< vol 200)
                                      (+ vol 1)
                                      200)
                                  (if (< vol 200)
                                      (+ vol (car args))
                                      200)))))
            ((equal? x '-)
             (let ((vol (au:get-param zeb 0 *au:global-scope* 0)))
                (au:set-param (now) zeb 0 0 0 
                              (if (null? args)
                                  (if (> vol 0)
                                      (- vol 1)
                                      0)
                                  (if (> vol 0)
                                      (- vol (car args))
                                      0)))))
            (else 'bad-method-name))))
      ;;(print (au:get-param zeb 0 *au:global-scope* 0))))




;;(zb:setnow zebra4 *zb:main_volume* 10)
(define zb:setnow 
   (lambda (zeb *zb:param* val)
      (au:set-param (now) zeb *zb:param* 0 0 val)))



;; other related shortcuts:

(define zb:volumeup 
   (lambda (zeb)
      (zb:volume zeb '+ 1)))

(define zb:volumedown 
   (lambda (zeb)
      (zb:volume zeb '- 1)))

(define zb:volumeup_by 
   (lambda (zeb val)
      (zb:volume zeb '+ val)))

(define zb:volumedown_by 
   (lambda (zeb val)
      (zb:volume zeb '- val)))


(define zb:volumeup_till 
   (lambda (zeb val)
      (let ((vol (au:get-param zeb 0 *au:global-scope* 0)))
         (if (< vol val)
             (zb:volume zeb '+ 1)))))

(define zb:volumedown_till
   (lambda (zeb val)
      (let ((vol (au:get-param zeb 0 *au:global-scope* 0)))
         (if (> vol val)
             (zb:volume zeb '- 1)))))


