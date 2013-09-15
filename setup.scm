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


;; shortcuts 

(define --create setup:create-au) ;; just a different name for it


(define-macro (--createdls name bus)
   `(setup:create-au ,name ++dls ,bus))
(define-macro (--createzebra name bus)
   `(setup:create-au ,name ++zebra ,bus))
(define-macro (--createkontakt name bus)
   `(setup:create-au ,name ++kontakt4 ,bus))
(define-macro (--createabsynth name bus)
   `(setup:create-au ,name ++absynth4 ,bus))
(define-macro (--createfm8 name bus)
   `(setup:create-au ,name ++fm8 ,bus))
(define-macro (--createsampler name bus)
   `(setup:create-au ,name ++impromptusampler ,bus))


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






;;--------
;  LOADING SAMPLES FROM DIRECTORY 

;; ARGS
;; sampler: an impromptu sampler instance
;; location: a folder containing some stereo samples
;; idx: the position where samples should start, eg 1 or 50

; COMMENTS:
;; Impromptu sampler works ONLY with STEREO files - it'll crash if you pass it a mono one!
;; It also creates scheme symbols for each sample file eg "=sample1.wav=" (so to facilitate coding)
;; subs white spaces with '-' and lowercases the names

;TODO:
;; add macro so that index is 1 by default..
;; specify how samples are mapped over pitch range..
;; create the *my-audio-files* dynamically so that we can have more for more sampler instances...

;EXAMPLE
;(--setup)
;(--createsampler sam 3)
;(setup:loadsamples  sam "/my/_samples/Drum Machines/Moog Concertmate MG-1")
;(mu:test sam =sample1.wav=)
;;--------


;; GLOBAL variable: a vector to store audio data loaded via setup:loadsamples
(define *my-audio-files* (make-vector 127))

; note: crashes with MONO files
(define __loadsamples
   (lambda (sampler location idx)
      (let ((files (string-split (io:directory-list location) "\n"))
            (report ""))
         (for-each (lambda (filename)
                      (let ((x (string-replace (cl:string-lower filename) " " "-")))
                         (if (or (cl:string-find x ".wav") 
                                 (cl:string-find x ".aif") 
                                 (cl:string-find x ".aiff"))
                             (begin (vector-set! *my-audio-files* idx 
                                                 (au:load-audio-data (string-append location "/" filename)))
                                    (au:play:set-sample-data sampler idx 
                                                             (vector-ref *my-audio-files* idx))
                                    (let ((name (string->atom (string-append "=" x "="))))
                                       (eval `(define ,name ,idx)  (interaction-environment))
                                       (set! report 
                                             (string-append report ".. " 
                                                            (string-append "=" x "=")  
                                                            " [idx=" (number->string idx) "]"
                                                            " ==> sample '" filename "'\n")))
                                    (set! idx (+ idx 1))))))
                   files)
         (print report))))



(macro (setup:loadsamples args)  
   (cond ((equal? (length (cdr args)) 2)
          `(let ((sampler ,(cadr args))
                 (path ,(caddr args)))
              (__loadsamples sampler path 1)))
          ((equal? (length (cdr args)) 3)
	      `(let ((sampler ,(cadr args))
	             (path ,(caddr args))
				 (idx ,(cadddr args)))           
            (__loadsamples sampler path idx)))
          (#t (print "Error: the function only accepts 2 or 3 argument (instrument/path/&idx)"))))






; ================
;  VOLUME SLIDER
;
; eg: 
;(define *volslider* (setup:volumeslider '(piano1 piano2)))
;; or you could do simply
;(define vol (setup:volumeslider 2))

;; remember to release the object when closing!
; (objc:release *volslider*)
;
; ================
          




(define setup:volumeslider
 (lambda (params)
    (let* ((slidersTot (if (number? params) params (length params)))
           (labels (if (list? params) (map (lambda (x) (symbol->string x)) params) 
                       #f))
           (winWidth (* slidersTot 120))
           (window (objc:make "NSWindow" "initWithContentRect:styleMask:backing:defer:"
                        (list 200 200 winWidth 300)
                        1 2 0)) 
           (sliders (make-list-with-proc slidersTot (lambda (i)
                           (let ((slider (objc:make "NSSlider" "initWithFrame:"  ;; x y width height
                                                    (list (+ 40 (* i 120)) 60 40 200))))
                              (objc:call slider "setTarget:" *objc:bridge*)
                              (objc:call slider "setAction:" "floatAction:")
                              (objc:call slider "setTag:" i)
                              (objc:call slider "setMaxValue:" 3.0)
                              (objc:call (objc:call window "contentView") "addSubview:" slider)
                              slider))))
           ;; text fields are updated as slider moves
           (text-fields (make-list-with-proc slidersTot (lambda (i)
                           (let ((tf (objc:make "NSTextField" "initWithFrame:"
                                                (list (+ 28 (* i 120)) 20 65 20))))
                              (objc:call tf "setEditable:" #f)
                              (objc:call (objc:call window "contentView") "addSubview:" tf)
                              tf))))
           ;; text labels display au name (if passed as list) or its bus number
           (text-labels  (make-list-with-proc slidersTot (lambda (i)
                           (let ((tf (objc:make "NSTextField" "initWithFrame:"
                                                (list (+ 33 (* i 120)) 270 50 20))))
                              (objc:call tf "setEditable:" #f)
                              (objc:call (objc:call window "contentView") "addSubview:" tf)
                              (if labels
                                  (objc:call tf "setStringValue:" (list-ref labels i))
                                  (objc:call tf "setStringValue:" (string-append "ch:" (number->string i))))
                              tf))))
           )
       (objc:call window "orderFront:" 0)
       (objc:call window "setTitle:" "Mixer Volumes")
       (set! objc:action
             (lambda (id val)
                (let* ((tf (list-ref text-fields id))
                       (slider (list-ref sliders id))
                       (idchannel (+ id 1)) ;; channels start at 1, slider id at 0
                       (value (objc:call slider "floatValue")))
                   (objc:call tf "setStringValue:"
                              (number->string value))
                   (au:set-param (now) *mixer* 0 *au:input-scope* idchannel value))))
       window)))












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













