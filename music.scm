;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  mu:
;;  functions for making music
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple functions usable for *testing* sounds/instruments
;; both with/without time abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mu:test (lambda (instrument note)
                              (play-note (now) instrument note 80 *second*)))
     


; (mu:testchord dls '(60 70 90))
(define mu:testchord
   (lambda (instrument notes)
      (for-each (lambda (x)
                   (play-note (now) instrument x 80 *second*))
                notes)))






;PROBLEM: if the chordsym doesnt exist the whole thing breaks!!!!! WHYWHY?
; wrapper on pc:make-chord => it doesn't distribute the chord notes but it just adds them up to a root
; the new pc:lib now has :  (pc:make-chord-fixed 60 3 '(0 3 7))      => (60 63 67)
(define mu:chord
   (lambda (root chord-sym)
      (map (lambda (x)
              (+ root x))
           (pc:chord root chord-sym))))

;;(mu:chord 60 '^)


;; wrapping the pc:scale so that we can pass any note and obtain the relative scale
(define mu:scale 
   (lambda (base mode)
      (map (lambda (x)
              (+ (- base (modulo base 12)) x))
           (pc:scale (modulo base 12) mode))))

(when #f
      (pc:scale 2 'ionian)
      (mu:scale 62 'ionian))






;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VOLUMEFROMBEAT
;;;;;;;;;;;;;;;;;;;;;;;;;;


;2012-03-25: changed name: 'amodulo' 

;; mi serve una funzione cui passo a) quanto dura il loop b) a ogni posizione che volume associare
;; i valori di posizione in b) ovviamente devono essere compresi entro la dur max del loop: eg se il loop e' 2 beats, non posso avere 5/2. Se valori non vengono dati, assumo che sia zero, a meno che non passo un default value. 


;; note that by using volume from beat we don't have to use onebeat anymore!!!! 

;; inner function called by the macro below
(define _volume_from_beat 
   (lambda (beat vols-list amodulo)
      (if (> amodulo 0)
          (if (assoc (modulo beat amodulo) vols-list)
              (cadr (assoc (modulo beat amodulo) vols-list))
              0)
          0)))
;; macro and macro-signature trick
(define mu:volumefrombeat (lambda (vols_list amodulo) '()))
(define-macro (mu:volumefrombeat vols_list amodulo)
   `(_volume_from_beat beat ,vols_list ,amodulo))

; example
(if #f 
    (let ((beat (*metro* 'get-beat))
          (temp '((0 50) (1/2 80))))
       (mu:volumefrombeat temp 0))) ;; => 50 at each beat cause it's modulo 0











;; nice macro: intervals are either specified or inferred!
;(let ((beat (*metro* 'get-beat)))
;   (mu:playsequence dls '(60 62) 60 1/2 ))

(define mu:playsequence (lambda (inst plist volumes durations opt_intervals) '()))

(define-macro (mu:playsequence inst plist volumes durations . intervals)
   `(let* ((n (length ,plist))
           (vols (cl:expand-list2 ,volumes n))
           (durs (cl:expand-list2 ,durations n))
           (ints (if (null? (list ,@intervals))
                     (cl:expand-list2 ,durations n)
                     (cl:expand-list2 ,@intervals n))))

       (dotimes (i (length ,plist))
          (play (* (+ i 1) (cl:nth i ints))
                ,inst 
                (cl:nth i ,plist) 
                (cl:nth i vols)
                (cl:nth i durs)))))








;; like mu:playsequence, but much more intuitive
;; eg
;(let ((beat (*metro* 'get-beat)))
;   (mu:playseq dls '(60 62 64 65) '(0 1/2 3/2 3) 60 1/2) 
;   (mu:playseq bat '(10 10 10 10) '(0 1/2 3/2 3) 60 1/2))


(define mu:playseq (lambda (inst plist beatlist volumes a_duration)
                   '()))
(define-macro (mu:playseq inst plist times volumes aduration)
   `(let* ((n (length ,plist))
           (vols (cl:expand-list2 ,volumes n))
           (real_times (cl:expand-list2 ,times n)))         

       (dotimes (i (length ,plist))
          (play (cl:nth i real_times)
                ,inst 
                (cl:nth i ,plist) 
                (cl:nth i vols)
                ,aduration))))






;; same as above, but the beats do not need to be specified; they are inferred from the durations
;  eg: if we have 3 notes with dur 1, the intervals will be: '(0 1 2)

(define mu:playseq2 (lambda (inst plist beat_or_beatlist volumes a_duration)
                   '()))
(define-macro (mu:playseq2 inst plist times volumes aduration)
   `(let* ((n (length ,plist))
           (vols (cl:expand-list2 ,volumes n))
           (real_times (if (list? ,times)
                           (cl:expand-list2 ,times n)
                           (let inner ((i 0)  ;; generates sequential intervals based on the duration value
                                       (out '()))
                              (if (= i (length ,plist))
                                  out
                                  (inner (+ i 1) (append out (list (* i ,aduration)) )))
                             ))))  
                ;(print real_times)
       (dotimes (i (length ,plist))
          (play (cl:nth i real_times)
                ,inst 
                (cl:nth i ,plist) 
                (cl:nth i vols)
                ,aduration))))




; wrapper for facilitating playing chords
; you can pass a fixed volume or as many vols as the chord notes are
;(let ((beat (*metro* 'get-beat)))
;   (mu:playchord zeb1 (mu:chord 60 '-) '(50 10 60) 4))
				

(define mu:playchord (lambda (inst plist vol len)
                   '()))

(define-macro (mu:playchord inst plist vol len)
   `(if (list? ,vol)
        (let ((volumes (cl:expand-list ,vol (- (length ,plist) (length ,vol)))))
           (for-each (lambda (p v)
                        (play ,inst p v ,len))
                     ,plist volumes))
        (for-each (lambda (p)
                     (play ,inst p ,vol ,len))
                  ,plist)))









;;;;;;;;;;;;;
; RULLATE
;
;(let ((beat (*metro* 'get-beat)))
;   (mu:roll bat 13 9 1/4))
;;;;;;;;;;;;;

(define mu:rollup (lambda (inst pitch) '()))
(define-macro (mu:rollup inst pitch)
   `(let loop ((i 0))
         (play (* i 1/10) ,inst ,pitch (+ 50 (expt 4 i)) 1 9)
         (if (< i 3) (loop (+ i 1)))))


(define mu:rolldown (lambda (inst pitch) '()))
(define-macro (mu:rolldown inst pitch)
   `(let loop ((i 0))
         (play (* i 1/10) ,inst ,pitch (- 110 (expt 4 i)) 1 9)
         (if (< i 3) (loop (+ i 1)))))

(define mu:rolldelay (lambda (inst pitch hits spacing) '()))
;; warning: this may give problems!
(define-macro (mu:rolldelay inst pitch hits spacing)
      `(let loop ((i 0))
         (play (* (/ i 3) (* i ,spacing)) ,inst ,pitch (+ 50 (/ (expt 2 i) 2)) 1 9)
         (if (< i (- ,hits 1) (loop (+ i 1))))))

(define mu:roll (lambda (inst pitch hits spacing) '()))
(define-macro (mu:roll inst pitch hits spacing)
      `(let loop ((i 0))
         (play (* i ,spacing) ,inst ,pitch (+ 60 (expt 2 i)) 1 9)
         (if (< i (- ,hits 1)) (loop (+ i 1)))))









;2012-04-15: SHORTCUT FOR PLAYING AN AUDIO INSTRUMENT INSTANCE
; note that 'macro' and 'define-macro' are not the same macro-definition procedures
; if we use define-macro the functions below won't work

;example:
;(define t
;   (lambda (beat) 
;      (mu:with-au dls
;        (mu:play-au 47 90 1/2))
;     (callback (*metro* (+ beat (* 1/2 1))) 't (+ beat 1))))


(define mu:with-au (lambda (audio-instrument-instance) '()))
(macro (mu:with-au args)
   `(let ((thisau ,(cadr args)))
       ,@(cddr args)))


;; macro that expects an 'inst' variable defined, so that we can omit the instrument when using play
(define mu:play-au (lambda (&offset note vol dur) '()))
(macro (mu:play-au args)  
   (cond ((equal? (length (cdr args)) 3)
          `(let ((note ,(cadr args))
                 (vol ,(caddr args))
                 (dur ,(cadddr args))
                 )
              ;(print thisau beat note vol dur)
              (play thisau note vol dur)))
         ((equal? (length (cdr args)) 4)
          `(let ((offset ,(cadr args))
                 (note ,(caddr args))
                 (vol ,(cadddr args))
                 (dur ,(cadddr (cdr args))))               
           ;(print thisau beat note vol dur)
           (play (eval offset) thisau note vol dur)))
         (#t (print "Error: the function only accepts 3 or 4 argument"))))








;;; metrotick object - relies on a metronome instance being previously defined, and on the 
; cl:ratio_is_perfect util

; (define *metro* (make-metro 120))
; (define metroclick (mu:make-metrotick *metro*))
; (metroclick 4)  ;; start beating a 4/4
; (metroclick 'stop) 


(define mu:make-metrotick
   (lambda (start-metro)
      (let* ((*loopruns* #f)
             (*beats* 1)
             (*vol* 60)
             (metro start-metro)
             (musicloop 
               (lambda (beat) 
                  (let* ((mod (modulo beat *beats*)))
                     ;(print mod)
                    (if (= 0 mod)
                        (play dls *gm:hi-bongo*  *vol* 1/8 9)
                        (if (cl:ratio_is_perfect mod)
                            (begin (play dls *gm:low-bongo* *vol* 1/8 9))  ;;(print 'here mod) 
                              ))  ;;(print mod)
                    (if *loopruns* 
                      (callback (metro (+ beat 1/16)) musicloop (+ beat 1/8)))))))
         (lambda (sym . args)
            (cond ((number? sym)
                   (print (string-append "Metronome: started with " (number->string sym) "/4 (use 'stop to end)"))
                   (set! *beats* sym)
                   (when (not *loopruns*)
                         (set! *loopruns* #t)                                                                                                            
                         (musicloop (metro 'get-beat 1))))
                  
                  ((equal? sym 'stop)                   
                    (print "Metronome stopped")
                    (set! *loopruns* #f))
   
                  (else 'bad-method-name))))))














;
;
;SOME MUSICAL FUNCTIONS WITHOUT THE MU: PREFIX 
;
;



;; intervals



(define-macro (octave plist)
   `(if (list? ,plist)
        (map (lambda (p)
                (+ 12 p))
             ,plist)
        (+ 12 ,plist)))

(define-macro (fourth plist)
   `(if (list? ,plist)
        (map (lambda (p)
                (+ 5 p))
             ,plist)
        (+ 5 ,plist)))


(define-macro (fifth plist)
   `(if (list? ,plist)
        (map (lambda (p)
                (+ 7 p))
             ,plist)
        (+ 7 ,plist)))

(define-macro (thirdmaj plist)
   `(if (list? ,plist)
        (map (lambda (p)
                (+ 4 p))
             ,plist)
        (+ 4 ,plist)))


(define-macro (thirdmin plist)
   `(if (list? ,plist)
        (map (lambda (p)
                (+ 3 p))
             ,plist)
        (+ 3 ,plist)))


(define-macro (sixth plist)
   `(if (list? ,plist)
        (map (lambda (p)
                (+ 9 p))
             ,plist)
        (+ 9 ,plist)))

(define-macro (seventhdom plist)
   `(if (list? ,plist)
        (map (lambda (p)
                (+ 10 p))
             ,plist)
        (+ 10 ,plist)))

(define-macro (seventhmaj plist)
   `(if (list? ,plist)
        (map (lambda (p)
                (+ 11 p))
             ,plist)
        (+ 11 ,plist)))














