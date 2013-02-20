;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  setup:
;;  functions for settings things up
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;



;; an assoc list of the insts I normally use
(define *setup:instruments* '((++dls . ("aumu" "dls " "appl"))
                            (++crystal .  ("aumu" "AtFr" "GOSW"))
                            (++chip32 . ("aumu" "Chip" "Sam "))
                            (++zebra . ("aumu" "SMD2" "UHfX"))
                            (++absynth4 . ("aumu" "CWam" "-NI-"))
                            (++zebralette . ("aumu" "SMD3" "UHfX"))
                            (++alpha . ("aumu" "Alph" "LinP"))
                            (++bassdrum . ("aumu" "Sbdm" "ScBh"))
                            (++automat . ("aumu" "aut1" "Alfa"))
                            (++triplecheese . ("aumu" "cbSy" "UHfX"))
                            (++rainysynth . ("aumu" "Srns" "ScBh"))
                            (++msdevicedesign . ("aumu" "S_d3" "ScBh"));;what is tis?
                            (++flyingmonkey . ("aumu" "Sfms" "ScBh"))
                            (++8bitplug . ("aumu" "synt" "ymck"))
                            (++battery3 . ("aumu" "NBa3" "-NI-"))
                            (++fm8 . ("aumu" "Nif8" "-NI-"))
                            (++kontakt4 . ("aumu" "Nik4" "-NI-"))
                            (++miroslav . ("aumu" "PH10" "ikm_"))
                            (++ew_symphonic . ("aumu" "EwPl" "-EW-"))  ;; appears as 'Play'
                            (++impromptugate . ("aumu" "gate" "MOSO"))
                            (++impromptusampler . ("aumu" "play" "MOSO"))))
    

(map (lambda (pair)
        (print "Adding au symbol to interaction env: " pair)
        (eval `(define ,(car pair) ',(car pair))
              (interaction-environment)))
     *setup:instruments*)
; ==> we're loading this so that we can get autocompletion on *setup:....
; the symbol is set to its literal value, so that is can be used as a key in the assoc list above
; this is equivalent to: (define ++dls  '++dls)



;; ==> loads an audiounit from the ones mapped in *setup:instruments* on the fly
;; requires (setup:base) to be loaded first, as it relies on the mixer

(define setup:create-au (lambda (namee ++instrument outputbus) '()))  ;; trick for autocompletion

(define-macro (setup:create-au namee ++instrument outputbus)
   `(begin 
    (define ,namee (au:make-node (cadr (assoc ,++instrument *setup:instruments*))
                                (caddr (assoc ,++instrument *setup:instruments*))
                                (cadddr (assoc ,++instrument *setup:instruments*))
                                ))
                (au:connect-node ,namee 0 *mixer* ,outputbus)
                (au:update-graph)
                (patch:printdir ,++instrument)))





;; macro that loads up the basic stuff you need to get going

(define-macro (setup:base)
   `(begin (au:clear-graph)
           (define dls	 (au:make-node "aumu" "dls " "appl"))
           (define *mixer* (au:make-node "aumx" "smxr" "appl"))
           (au:connect-node dls 0 *mixer* 1)
           (au:connect-node *mixer* 0 *au:output-node* 0)
           (au:update-graph)
           (au:print-graph)
           (define *metro* (make-metro 120))
           (define *metroclick* (mu:make-metrotick *metro*))
           (play-note (now) dls (random 40 100) 80 *second* 3)
           (print "Set up successfull: dls, *mixer*, *metro* and *metroclick* are now available. Use patch:load-au to load other instruments")))



;; shortcut
(define --setup setup:base)




;##################
; VOLUME CONTROLLER
;
;Description
;-----------------------------------------
; This object lets you create a UI for managing volume
; It allows you to set volume via drag and drop, and also returns the volume you set via the 'set-volume method on the object
; each object has its own color
; ..todo..


; Adding behaviour for specific instruments: 
;-----------------------------------------
; It works with DLS and ZEBRA for now; you can find out which are the volumes via
;(au:print-params dls *au:global-scope*)
;(au:set-param (now) dls 2 0 0 1)
;
;



; Example:
;-----------------------------------------

;(setup:base)
;(define c1 (setup:volume-object ++dls dls))
;(c1 'get) ;; gets the volume value (varies depending on instrument)
;(c1 'close) ;; closes the canvas
;(c1 'init)  ;; recreates it (the color is changed)
;(c1 'set (cosr 20 30 1/128)) ;; sets a value
;(c1 'up .1)  ;; up by .1 (till default max value)
;(c1 'up .1 60)  ;; up by .1 till 60
;(c1 'down .5 40) ;; down by .5 till 40

; if opening too many canvas that are not needed, reset the container: (define *setup:volsbuffer* '())
;
;##################



;a list of assocs composed by (++instname . (maxvolume volparam_number)) 
;eg: (cadr (assoc '++dls *setup:volumes-constants*)) ; => 70

(define *setup:volumes-constants* '((++dls . (70 1)) ;; max should be 120 but it clips! // 0=tuning; 1=vol; 2=reverb
                                  (++zebra . (200 0)) ; 0=volume
                                  ;; in kontakt define it manually via UI, then save the patch! allvalues vary between 1 and 0
                                  (++kontakt4 . (1.0 0)) ; 0=volume, 
                                  ))


;; container 
(define *setup:volsbuffer* '())


(define __volume-object
   (lambda (++instrument-type instance instance_name)
      (let* ((klassname '<volume_controller>)
             (super (oo:make-object))
             ;;variables
             (*canvas* #f)
             (*canvas_max_x* #f)
             (*canvas_max_y* #f)
             (*layer* #f)
             (*layer_height* #f)  
             (*layer_width* #f)  
             (*layer_x* #f)
             (*layer_y* #f)          
             (*maxvolume* #f) ;;  
             (*fill* #f)      
             (*stroke* #f)
             (*inst-type* ++instrument-type)
             (*instance* instance)
             (*instance-name* #f)
             (*instance-id* (gensym))

             ;; main methods
             (init (lambda () 
                      (set! *canvas* (gfx:make-canvas 140 300)) ;; default                                
                      (io:register-mouse-events *canvas*)
                      (gfx:clear-canvas (now) *canvas* color:black) 
                      (set! *canvas_max_x* (car (gfx:get-window-size *canvas*)))
                      (set! *canvas_max_y* (cdr (gfx:get-window-size *canvas*)))
                      (set! *layer_width* *canvas_max_x*)
                      (set! *layer_height* *canvas_max_y*) 
                      (set! *layer_x* 0)
                      (set! *layer_y* 0)
                      (set! *fill* (list 1.0 (random) (random) 1.0))
                      (set! *stroke* (list 0.5 0.9 1.0 1.0))
                      (set! *maxvolume* (if (assoc ++instrument-type *setup:volumes-constants*)
                                            (cadr (assoc ++instrument-type *setup:volumes-constants*))
                                            120)) ;;default   
                      (set! *instance-name* (string-append instance_name " (max=" (atom->string *maxvolume*) ")"))
                      ;(*instance-name* (string-append instance_name (string-replace (symbol->string *instance-id*) "gensym" "")))
                      (setup:window_title *canvas*  *instance-name*)   

                                          ;; create the layer
                      (set! *layer* (gfx:make-rectangle 0 0 *layer_width* *layer_height*))
                      (gfx:draw-path (now) *canvas* *layer* '(0 0 0 0) '(0 0 0 0) 2.0)

                      ;; Store in a container all the symbols that are needed to identify an object: 
                      ;; this is needed by io:mouse-drag, cause it needs to check for all existing canvases!
                      (set! *setup:volsbuffer* (append *setup:volsbuffer* (list (list *canvas* *maxvolume* *fill* ++instrument-type instance))))   
                      ;;
                      (set! io:mouse-drag (lambda (x y canvas)
                                             (for-each (lambda (mem)
                                                          (let* ((acanvas (cl:nth 0 mem))
                                                                 (maxvolume (cl:nth 1 mem))
                                                                 (fill (cl:nth 2 mem))
                                                                 (inst-type (cl:nth 3 mem))
                                                                 (inst-instance (cl:nth 4 mem))
                                                                 (vol (* y (/ maxvolume *canvas_max_y*))))                                                           
                                                             (if (equal? canvas (objc:get-address acanvas))                                                                
                                                                 (if (gfx:point-in-path? *layer* x y) 
                                                                     (begin (_apply-slider y acanvas fill)
                                                                            (if #f (print (objc:get-address acanvas) maxvolume fill vol))
                                                                            (_apply-volume vol inst-type inst-instance))))))
                                                       *setup:volsbuffer*))

                            )))                                                                              


             ;; INNER method: draws the slider position
             (_apply-slider (lambda (val acanvas fill)
                              (let ((rect (gfx:make-rectangle 0 0 *canvas_max_x* val)))
                                 (begin (gfx:clear-canvas (now) acanvas color:black)
                                        (gfx:draw-path (now) acanvas rect *stroke* fill 1.0)))))
             ;; INNER method: sets the vol depending on inst type
             (_apply-volume (lambda (vol inst-type inst-instance)
                              (if (assoc inst-type *setup:volumes-constants*)
                                  (au:set-param (now) inst-instance 
                                                (caddr (assoc inst-type *setup:volumes-constants*)) 
                                                0 0 vol)
                                  (print "Instrument type not recognized"))))

             ;; GETS the vol (no need to check explicitly the inst-type cause it's internal to the obj)
             (get (lambda ()
                            (if (assoc ++instrument-type *setup:volumes-constants*)
                                (au:get-param instance 
                                              (caddr (assoc ++instrument-type *setup:volumes-constants*)) 
                                              0 0)
                                (print "Instrument type not recognized"))))

             ;; SETS the vol manually, and updates the slider (no drag and drop)
             (set (lambda (vol)
                     (let ((val (floor (* vol (/ *canvas_max_y* *maxvolume* )))))
                        (if (<= vol *maxvolume*) 
                            (begin (_apply-slider val *canvas* *fill*)
                                   (_apply-volume vol *inst-type* *instance*))
                            (print "Value too high: maximum is " *maxvolume* " for this instrument")))))  
             ;; ADDs a value to the vol ; a tillvalue arg is optional, otherwise it the max volume available for this instrument
             (up (lambda (n . tillvalue)
                   (let ((current (get))
                         (maxvalue (if (not (null? tillvalue))
                                       (car tillvalue)
                                       *maxvolume*)))
                      (if (< (+ current n) maxvalue)
                          (set (+ current n))
                          (set maxvalue)))))
             ;; SUBTRACTS a value to the vol ; a tillvalue arg is optional, otherwise it's 0
             (down (lambda (n . tillvalue)
                   (let ((current (get))
                         (minvalue (if (not (null? tillvalue))
                                       (car tillvalue)
                                       0)))
                        (if (> (- current n) minvalue)
                            (set (- current n))
                            (set minvalue)))))     
             ;; CLOSES a canvas (doesn't destroy the object) TODO: destroy?               
             (close (lambda () 
                       (gfx:close-canvas *canvas*) (print 'volume 'canvas 'closed)))

             ;; add all the callable methods to this list:
             (methods (append (list (cons 'init init)  
                                    (cons 'get get)
                                    (cons 'set set)
                                    (cons 'up up)
                                    (cons 'down down)
                                    (cons 'close close)
                                    (cons '_apply-volume _apply-volume)
                                    (cons '_apply-slider _apply-slider))
                              (super 'get-methods)))

             (self (lambda (msg . args)
                      (apply sys:dynamic-call
                             ((cdr (assoc 'dispatch methods)) msg methods)
                             args))))
         (init)  ;; automatically init
         (let ((current (get)))  ;; set preexisting vol
               (set current))
		 self)))

;; signature workaround
(define setup:volume-object (lambda (++instrument-type au-instance)))
;; macro that lets me access the instance name as string, before evaluation
(define-macro (setup:volume-object ++instrument au-instance)
   `(__volume-object ,++instrument ,au-instance (sexpr->string (quote ,au-instance))))







;##################
; GRAPHICS SETUP FUNCTIONS
;
;##################




;Eg: (setup:window_title (gfx:make-canvas 640 480) "Nice title")

(define setup:window_title 
   (lambda (acanvas title)
      (let ((window (objc:call acanvas "window")))
         (objc:call window "setTitle:" title))))





; Inner function used by (setup:canvas) or (setup:capturecode)
(define _capture-code
   (lambda (time)
      (let ((code (gfx:get-code-image))
            (alphacanvas *alphacanvas*)   
            (alphalayer *alphalayer*))
         (gfx:image2image *layer* code alphalayer)
         (gfx:draw-image time *canvas* code alphacanvas)   ;;; reducing this value sloooows down everything...
         (objc:release (+ time 2000) code))
      (callback (+ time 2000) '_capture-code (+ time 5000))))




(define setup:canvas (lambda (x y) '()))  ;; trick for autocompletion

(define-macro (setup:canvas x y)
    `(begin  (define *canvas* (gfx:make-canvas ,x ,y))  
              (define *layer* (gfx:make-image ,x ,y)) 
              (define *alphacanvas* 1)
              (define *alphalayer* .8)
              (paste:drawexample)
              (print "\nSet up successfull: *canvas*, *layer*, *alphacanvas*[=1] and *alphalayer*[=.8] are now available. \nDraw on *layer* using gfx:path2image. \nPaste from clipboard to see an example")))



(define setup:capturecode (lambda (x y) '()))  ;; trick for autocompletion

(define-macro (setup:capturecode x y)
    `(begin  (define *canvas* (gfx:make-canvas ,x ,y))  
              (define *layer* (gfx:make-image ,x ,y)) 
              (define *alphacanvas* 1)
              (define *alphalayer* .8)
              (_capture-code (now))
			  (paste:capturecode)
              (print "\nSet up successfull: *canvas*, *layer*, *alphacanvas*[=1] and *alphalayer*[=.8] are now available. \nDraw on *layer* using gfx:path2image. \nCapture code functions are pasted in clipboard.. ")))



(define setup:start-screencast (lambda (x y) '()))  ;; trick for autocompletion

(define-macro (setup:start-screencast x y)
    `(begin  (define *canvas* (gfx:make-canvas ,x ,y))  
              (define *layer* (gfx:make-image ,x ,y)) 
              (define *alphacanvas* 1)
              (define *alphalayer* .8)
              (_capture-code (now))
              (gfx:start-movie-capture *canvas* (string-append  "~/tmp/impromptu"
                                                                (atom->string (gensym)) 
																".mov")  #t)
			  (paste:capturecode)
              (print "\nScreencast started. \nStop it using (gfx:stop-movie-capture *canvas*). \nSymbols *canvas*, *layer*, *alphacanvas*[=1] and *alphalayer*[=.8] are now available. \nDraw on *layer* using gfx:path2image. [Capture code functions are pasted in clipboard..] ")))



;; (setup:stop-screencast) [just a shortcut]
(define setup:stop-screencast 
   (lambda ()
      (gfx:stop-movie-capture *canvas*)))


