;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  PASTE:
;;  functions for copying code templates into the clipboard
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;






;;; pastebin functions for livecoding: create an 'empty' template of a musical loop


;;--------
;  Sun Jul 17 18:47:14 BST 2011
;  ~ [used to be pb:cb] 
;;--------

(define ~ (lambda (name beat) '()))  ;; trick for autocompletion
;; macro wrapper for pb-base: produces a acallback skeleton with let ((dur .....
(define-macro (~ name beat . args)
   `(inner:pb-base (sexpr->string (quote ,name)) 
              (sexpr->string (quote ,beat))
              ,@(map (lambda (expr)
                        (sexpr->string expr))
                     args)))
(define inner:pb-base
   (lambda (name beat . args)
      (let ((a (apply string-append (map (lambda (e) (string-append "\n      " e)) args))))
         (sys:set-pasteboard
         ( string-append 
"(define " name "
   (lambda (beat) " a "
     (callback (*metro* (+ beat (* 1/2 " beat "))) '" name " (+ beat " beat "))))\n\n(" name " (*metro* 'get-beat 4))")))))






;;--------
;  Sun Jul 17 18:47:14 BST 2011
;  ~DUR 
;;--------

(define ~dur (lambda (name beat) '()))  ;; trick for autocompletion
;; macro wrapper for pb-dur: produces a acallback skeleton with let ((dur .....
(define-macro (~dur name beat . args)
   `(inner:pb-dur (sexpr->string (quote ,name)) 
              (sexpr->string (quote ,beat))
              ,@(map (lambda (expr)
                        (sexpr->string expr))
                     args)))
(define inner:pb-dur
   (lambda (name beat . args)
      (let ((a (apply string-append (map (lambda (e) (string-append "\n      " e)) args))))
         (sys:set-pasteboard
         (string-append 
"(define " name "
   (lambda (beat) 
      (let ((dur " beat "))
          " a "
     (callback (*metro* (+ beat (* 1/2 dur))) '" name " (+ beat dur)))))\n\n(" name " (*metro* 'get-beat 4))")))))




;;--------
;  Sun Jul 17 18:47:14 BST 2011
;  ~CHORD 
;;--------

(define ~chord (lambda (name beat chord) '()))  ;; trick for autocompletion
;; EG
;; (pb:chord t 1/8 (pc:make-chord 50 70 4 '(0 4 5 6 7)))
;; (pb:chord t 1/8 my_chord)
(define-macro (~chord name beat chord . args)
   `(inner:pb-chord (sexpr->string (quote ,name)) 
              (sexpr->string (quote ,beat))
              (sexpr->string (quote ,chord))
              ,@(map (lambda (expr)
                        (sexpr->string expr))
                     args)))
(define inner:pb-chord
   (lambda (name beat chord . args)
      (let ((a (apply string-append (map (lambda (e) (string-append "\n      " e)) args))))
         (sys:set-pasteboard
         (string-append 
"(define " name "
   (lambda (beat chord) 
      (let ((dur " beat "))
         (for-each (lambda (x)
                      (play ??? x 1 dur))
                   chord)
         " a "
         (callback (*metro* (+ beat (* 1/2 dur))) '" name " (+ beat dur) chord))))\n\n(" name " (*metro* 'get-beat 4) " chord ")")))))










;;--------
;  Sun Jul 17 18:47:14 BST 2011
;  ~MEL 
;;--------

(define ~mel (lambda (name beat melody) '()))  ;; trick for autocompletion
;; EG
;; (pb:mel loop 1/8 (make-list-with-proc 8 (lambda (x) (pc:random 65 75 '(0)))))
(define-macro (~mel name beat melody . args)
   `(inner:pb-mel (sexpr->string (quote ,name)) 
              (sexpr->string (quote ,beat))
              (sexpr->string (quote ,melody))
              ,@(map (lambda (expr)
                        (sexpr->string expr))
                     args)))

(define inner:pb-mel
   (lambda (name beat melody . args)
      (let ((a (apply string-append (map (lambda (e) (string-append "\n      " e)) args))))
         (sys:set-pasteboard
         (string-append 
"(define " name "
   (lambda (beat melody) 
      (let ((dur " beat ")) 
         (play ???? (car melody) 1 dur)        
         " a "
         (callback (*metro* (+ beat (* 1/2 dur))) '" name " (+ beat dur)
                   (if-cdr-notnull melody " melody ")))))\n\n(" name " (*metro* 'get-beat 4) " melody ")")))))









;;--------
;  Sun Jul 17 18:47:14 BST 2011
;  ~MEL+DURS 
;  same as ~MEL, but you can pass some durations too, which will be looped over. It can create interesting sfasature tra il numero di note e quello delle durations
;;--------

;; EG
;; (~mel+durs loop (make-list-with-proc 8 (lambda (x) (pc:random 65 75 '(0)))) (list 2 3 4))
(define ~mel+durs (lambda (name melody durs) '()))  ;; trick for autocompletion

(define-macro (~mel+durs name melody durs . args)
   `(inner:pb-mel2 (sexpr->string (quote ,name)) 
              (sexpr->string (quote ,melody))
              (sexpr->string (quote ,durs))
              ,@(map (lambda (expr)
                        (sexpr->string expr))
                     args)))
(define inner:pb-mel2
   (lambda (name melody durs . args)
      (let ((a (apply string-append (map (lambda (e) (string-append "\n      " e)) args))))
         (sys:set-pasteboard
         (string-append 
"(define " name "
   (lambda (beat melody durs) 
      (let ((dur (car durs))) 
         (play ???? (car melody) 1 dur)        
         " a "
         (callback (*metro* (+ beat (* 1/2 dur))) '" name " (+ beat dur)
                   (if-cdr-notnull melody " melody ")
                   (if-cdr-notnull durs " durs ")))))\n\n(" name " (*metro* 'get-beat 1) " melody " " durs ")")))))








;;--------
;  2012-04-15
;  ~playau pastebin function
;;--------



(define ~playau (lambda (name dur inst) '())) 
;; macro wrapper for pb-iplay
(define-macro (~playau name dur inst . args)
   `(pb-playau (sexpr->string (quote ,name)) 
              (sexpr->string (quote ,dur))
              (sexpr->string (quote ,inst))
              ,@(map (lambda (expr)
                        (sexpr->string expr))
                     args)))
(define pb-playau
   (lambda (name dur inst . args)
      (let ((a (apply string-append (map (lambda (e) (string-append "\n      " e)) args))))
         (sys:set-pasteboard
         (string-append 
"(define " name "
   (lambda (beat) 
      (mu:with-au " inst "
          (mu:play-au 50 90 1))" a "
     (callback (*metro* (+ beat (* 1/2 " dur "))) '" name " (+ beat " dur "))))\n\n(" name " (*metro* 'get-beat 4))")))))








;;--------
;  Sun Jul 17 18:48:26 BST 2011

; Pastebin functions that return code for MUSICAL SETUPs
;  [not used much anymore]
;;--------



(define paste:simple_setup
	(lambda ()
		(sys:set-pasteboard (string-append "(au:clear-graph)\n"
													  "(define dls	 (au:make-node \"aumu\" \"dls \" \"appl\"))\n"
													  "(define *mixer* (au:make-node \"aumx\" \"smxr\" \"appl\"))\n"
													  "(au:connect-node dls 0 *mixer* 1)\n"
													  "(au:connect-node *mixer* 0 *au:output-node* 0)\n"

													  "(au:update-graph)\n"
													  "(au:print-graph)\n"
													  "(define *metro* (make-metro 100))\n"
													  "(*metro* 'set-tempo 120)\n"
													  
													  ";;test\n"
													  "(play-note (now) dls 60 80 *second* 3)\n"
													  ))))
													

;;;;;;;;;;;;
;;;; setup with Battery

(define paste:setup_bat
	(lambda ()
		(sys:set-pasteboard (string-append "(au:clear-graph)\n"
													  "(define dls	 (au:make-node \"aumu\" \"dls \" \"appl\"))\n"
													  "(define bat (au:make-node	\"aumu\" \"NBa3\" \"-NI-\" ))\n"
													  "(define *mixer* (au:make-node \"aumx\" \"smxr\" \"appl\"))\n"
													  "(au:connect-node dls 0 *mixer* 1)\n"
													  "(au:connect-node bat 0 *mixer* 2)\n"
													  "(au:connect-node *mixer* 0 *au:output-node* 0)\n"

													  "(au:update-graph)\n"
													  "(au:print-graph)\n"
													  "(define *metro* (make-metro 100))\n"
													  "(*metro* 'set-tempo 120)\n"
													  
													  ))))


;;;;;;;;;;;;
;;;; setup with Zebra

(define paste:setup_zebras
	(lambda ()
		(sys:set-pasteboard (string-append "(au:clear-graph)\n"
													  "(define dls	 (au:make-node \"aumu\" \"dls \" \"appl\"))\n"
													  "(define zebra1 (au:make-node	\"aumu\" \"SMD2\" \"UHfX\" ))\n"
													  "(define zebra2 (au:make-node	\"aumu\" \"SMD2\" \"UHfX\" ))\n"
													  "(define zebra3 (au:make-node	\"aumu\" \"SMD2\" \"UHfX\" ))\n"
													  "(define zebra4 (au:make-node	\"aumu\" \"SMD2\" \"UHfX\" ))\n"
													  "(define *mixer* (au:make-node \"aumx\" \"smxr\" \"appl\"))\n"
													  "(au:connect-node dls 0 *mixer* 1)\n"
													  "(au:connect-node zebra1 0 *mixer* 2)\n"
													  "(au:connect-node zebra2 0 *mixer* 3)\n"
													  "(au:connect-node zebra3 0 *mixer* 4)\n"
													  "(au:connect-node zebra4 0 *mixer* 5)\n"
													  "(au:connect-node *mixer* 0 *au:output-node* 0)\n"

													  "(au:update-graph)\n"
													  "(au:print-graph)\n"
													  "(define *metro* (make-metro 100))\n"
													  "(*metro* 'set-tempo 120)\n"

													  ))))





;;;;;;;;;;;;
;;;; setup with everything

(define paste:setup_all
	(lambda ()
		(sys:set-pasteboard (string-append "(au:clear-graph)\n"
													  "(define dls	 (au:make-node \"aumu\" \"dls \" \"appl\"))\n"
													  "(define zebra1 (au:make-node	\"aumu\" \"SMD2\" \"UHfX\" ))\n"
													  "(define zebra2 (au:make-node	\"aumu\" \"SMD2\" \"UHfX\" ))\n"
													  "(define zebra3 (au:make-node	\"aumu\" \"SMD2\" \"UHfX\" ))\n"
													  "(define zebra4 (au:make-node	\"aumu\" \"SMD2\" \"UHfX\" ))\n"
													  "(define bat (au:make-node	\"aumu\" \"NBa3\" \"-NI-\" ))\n"
													  "(define *mixer* (au:make-node \"aumx\" \"smxr\" \"appl\"))\n"
													  "(au:connect-node dls 0 *mixer* 1)\n"
													  "(au:connect-node zebra1 0 *mixer* 2)\n"
													  "(au:connect-node zebra2 0 *mixer* 3)\n"
													  "(au:connect-node zebra3 0 *mixer* 4)\n"
													  "(au:connect-node zebra4 0 *mixer* 5)\n"
													  "(au:connect-node bat 0 *mixer* 6)\n"
													  "(au:connect-node *mixer* 0 *au:output-node* 0)\n"

													  "(au:update-graph)\n"
													  "(au:print-graph)\n"
													  "(define *metro* (make-metro 100))\n"
													  "(*metro* 'set-tempo 120)\n"

													  ))))





















;;;;;;;;;;;;;;
;;    stub for drumkit
;;    not used much anymore.....
;;;;;;;;;;;;;

;;;drumkit in 4/4 with levare
;; eg (paste:drumkit 'f 1/8)
(define paste:drumkit
	(lambda (name dur)
		(sys:set-pasteboard (string-append "(define "
                                     (atom->string name)
                                     "\n  (lambda (beat)\n" 
                                     "     (let ((staccati1 '(.25 .5 .75))\n"
                                     "           (staccati2 '(.25 .5 .725 ))\n"
                                     "           (vol 0)\n"
                                     "           (drkit inst))\n"
                                     "        (when-mod 1 '(0) (play drkit 6 vol 1))\n"
                                     "        (when-mod 4 '(0) (play drkit 13 vol 1))\n"
                                     "        (when-mod 4 '(2) (play drkit 14 vol 1))\n"
                                     "        (when-mod 8 '(6) (play (random staccati1) drkit 14 vol 1))\n"
                                     "        (when-mod 8 '(7) (play (random staccati2) drkit 13 vol 1))\n\n"
                                     
                                     "        (callback (*metro* (+ beat (* .5 " (atom->string dur) "))) '"(atom->string name) " (+ beat " (atom->string dur) ")))))\n\n\n"

                                     "(" (atom->string name) " (*metro* 'get-beat 4))"

													  ))))




;;;;;;;;;;;;
;;;; oscillating volume
;;;;;;;;;;;;
(define paste:vol-osc
	(lambda ()
		(sys:set-pasteboard (string-append "(define vol_osc\n"
													  "	(lambda (freq beat)\n"
													  "		(abs (* 100 (sin (* freq pi (*metro* beat)))))))\n"
													  ";;test\n"
													  "(vol_osc .319 (*metro* 'get-beat 4))\n"
													  ))))






(define paste:audio-capture
   (lambda ()
		(sys:set-pasteboard (string-append 
													  "(au:start-audio-capture \"~/tmp/impromptu.aiff\")\n"
													  "(au:stop-audio-capture)"

													  ))))





;;;;;;;;; 
;  functions that paste the code needed for CREATING INSTRUMENTS
;;
;     [this is normally done via setup:create-au. .....]
;;;;;;;;


;; e.g. (paste:createnode crystal ++crystal 3)
;; eg we are pasting this into the system clipboard:
	;;(define sampler (au:make-node "aumu" "play" "moso"))
	;;(au:connect-node dls 0 *mixer* 1)
	;;(au:update-graph)
(define paste:createnode (lambda (name ++instrument outputbus) '()))  
;;; macro that wraps the following function
(define-macro (paste:createnode name ++instrument outputbus)
   `(inner:pastecreatenode (sexpr->string (quote ,name))  ,++instrument ,outputbus))

(define inner:pastecreatenode
	(lambda (name ++instrument outputbus)
		(let ((instr (assoc ++instrument *setup:instruments*))
			(nname (sexpr->string name)))
			(sys:set-pasteboard (string-append "(define " (atom->string name) 
														  " (au:make-node	 \""																				
														  (atom->string (cl:nth 1 instr))  "\" \""
														  (atom->string (cl:nth 2 instr))  "\" \""
														  (atom->string (cl:nth 3 instr))  "\" "
														  "))\n(au:connect-node " 
														  (atom->string name) 
														  " 0 *mixer* " 
														  (atom->string outputbus) ")\n" 
														  "(au:update-graph)"
														  ))) ))








;
;##################
;#  Sat Dec  4 19:28:08 GMT 2010
;#  GRAPHICAL PASTEBINs 
;#
;##################


;; funs to capture code, canvas + layer and fun that writes on layer

(define paste:capturecode
   (lambda ()
		(sys:set-pasteboard (string-append 
";; make the canvas the exact size of your screen so to avoid empty spaces 
(define *canvas* (gfx:make-canvas 800 600))
(define *layer* (gfx:make-image 800 600)) 	
(define *alphacanvas* 1)
(define *alphalayer* .8)

(define _capture-code
   (lambda (time)
      (let ((code (gfx:get-code-image))
            (alphacanvas *alphacanvas*)   
            (alphalayer *alphalayer*))
         (gfx:image2image *layer* code alphalayer)
         (gfx:draw-image time *canvas* code alphacanvas)   ;;; reducing this value sloooows down everything...
         (objc:release (+ time 2000) code))
      (callback (+ time 2000) '_capture-code (+ time 5000))))
(_capture-code (now))

(gfx:start-movie-capture *canvas* \"~/tmp/my.mov\" #t)
;;(gfx:stop-movie-capture canvas)

;; eg: draw on the layer
(define update-layer
   (lambda (beat) 
      (let ((dur 1/8)
            (square (gfx:make-rectangle (cosr 60 30 1/4) (cosr 160 130 1/32) 50 60))
            (line (gfx:make-line 0 0 (random 200) (random 200))))
         ;; we can draw directly on the canvas
         (onbeat 2 0 (gfx:draw-path (*metro* beat) *canvas* line color:black color:white (random 1 5)))
         ;; or we draw on the layer...
         (gfx:clear-image *layer*)
         (gfx:path2image square *layer* '(1 0 1 .1) '(0 0 1 .1)) 
     (callback (*metro* (+ beat (* 1/2 dur))) 'update-layer (+ beat dur)))))

(update-layer (*metro* 'get-beat 4))"))))





(define paste:drawexample
   (lambda ()
		(sys:set-pasteboard (string-append 
"
(gfx:clear-canvas (now) *canvas* color:white)
;; eg: draw on the layer
(define update-layer
   (lambda (beat) 
      (let ((dur 1/8)
            (square (gfx:make-rectangle (cosr 60 30 1/4) (cosr 160 130 1/32) 50 60))
            (line (gfx:make-line 0 0 (random 200) (random 200))))
         ;; draw layer on canvas
         (gfx:draw-image (*metro* beat) *canvas* *layer* *alphalayer*)
         ;; we can draw directly on the canvas
         (onbeat 2 0 (gfx:draw-path (*metro* beat) *canvas* line color:black color:white (random 1 5)))
         ;; or we draw on the layer...
         (gfx:clear-image *layer*)
         (gfx:path2image square *layer* '(1 0 1 .1) '(0 0 1 .1))
     (callback (*metro* (+ beat (* 1/2 dur))) 'update-layer (+ beat dur)))))

(update-layer (*metro* 'get-beat))"))))






(define paste:screencast
   (lambda ()
		(sys:set-pasteboard (string-append 
";; make the canvas the exact size of your screen so to avoid empty spaces 
(define *canvas* (gfx:make-canvas 800 600))			  
(define *layer* (gfx:make-image 800 600)) 	
(define *alphacanvas* 1)
(define *alphalayer* .8)
(define _capture-code
   (lambda (time)
      (let ((code (gfx:get-code-image))
            (alphacanvas *alphacanvas*)   
            (alphalayer *alphalayer*))
         (gfx:image2image *layer* code alphalayer)
         (gfx:draw-image time *canvas* code alphacanvas)   ;;; reducing this value sloooows down everything...
         (objc:release (+ time 2000) code))
      (callback (+ time 2000) '_capture-code (+ time 5000))))
(_capture-code (now))

(gfx:start-movie-capture *canvas* \"~/tmp/my.mov\" #t)
;;(gfx:stop-movie-capture canvas)"))))







