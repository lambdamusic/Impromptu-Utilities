;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  gmidi:
;;  helpers for working with DlS and gmidi mappings
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;







;
; HELPERS FOR DLS INSTRUMENT
;

;; function that gives back number mappings from the gm:names I've defined
;; e.g. (gm:numbers_from_names '(*gm:kick* *gm:kick* *gm:kick* *gm:kick*))
(define dls:numbers_from_names 
   (lambda (listnames)
      (make-list-with-proc (length listnames) (lambda (x)
                                                 (eval (cl:nth (+ 1 x) listnames))))))


;;(mk:change-bank piano *gmi:guitar-fret-noise*)
(define dls:change-bank 
   (lambda  (gm_inst *gmi:newbank*)
      (au:midi-out (now) gm_inst *io:midi-cc* 16 *gmi:newbank* 0)))  


;; channel 0 10 XX changes the PAN (0-180)
(define dls:change-pan
   (lambda (time inst pos)
      (au:midi-out time inst *io:midi-cc* 0 10 pos) 
      (print-notification "Pan changed to: " pos)))







;;;;;;;;;
;;; midi mappings
;;;;;;;;;



; gm-> drums
(define *gm:kick* 35)
(define *gm:kick-2* 36)
(define *gm:side-stick* 37)
(define *gm:snare* 38)
(define *gm:hand-clap* 39)
(define *gm:snare-2* 40)
(define *gm:low-floor-tom* 41)
(define *gm:closed-hi-hat* 42)
(define *gm:hi-floor-tom* 43)
(define *gm:pedal-hi-hat* 44)
(define *gm:low-tom* 45)
(define *gm:open-hi-hat* 46)
(define *gm:low-mid-tom* 47)
(define *gm:hi-mid-tom* 48)
(define *gm:crash* 49)
(define *gm:hi-tom* 50)
(define *gm:ride* 51)
(define *gm:chinese* 52)
(define *gm:ride-bell* 53)
(define *gm:tambourine* 54)
(define *gm:splash* 55)
(define *gm:cowbell* 56)
(define *gm:crash-2* 57)
(define *gm:vibraslap* 58)
(define *gm:ride-2* 59)
(define *gm:hi-bongo* 60)
(define *gm:low-bongo* 61)
(define *gm:mute-hi-conga* 62)
(define *gm:hi-conga* 63)
(define *gm:low-conga* 64)
(define *gm:hi-timbale* 65)
(define *gm:low-timbale* 66)
(define *gm:hi-agogo* 67)
(define *gm:low-agogo* 68)
(define *gm:cabasa* 69)
(define *gm:maracas* 70)
(define *gm:short-whistle* 71)
(define *gm:long-whistle* 72)
(define *gm:short-guiro* 73)
(define *gm:long-guiro* 74)
(define *gm:claves* 75)
(define *gm:hi-wood-block* 76)
(define *gm:low-wood-block* 77)
(define *gm:mute-cuica* 78)
(define *gm:open-cuica* 79)
(define *gm:mute-triangle* 80)
(define *gm:open-triangle* 81)
(define *gm:mute-surdo* 86)
(define *gm:open-surdo* 87)



;; gmi->instruments
(define *gmi:acoustic-grand-piano*  1) 
(define *gmi:bright-acoustic-piano* 2) 
(define *gmi:electric-grand-piano*  3) 
(define *gmi:honky-tonk-piano*      4) 
(define *gmi:electric-piano-1*      5) 
(define *gmi:electric-piano-2*      6) 
(define *gmi:harpsichord*           7) 
(define *gmi:clavi*                 8) 
(define *gmi:celesta*                9) 
(define *gmi:glockenspiel*           10)     
(define *gmi:music-box*              11)     
(define *gmi:vibraphone*             12)     
(define *gmi:marimba*                13)     
(define *gmi:xylophone*              14)     
(define *gmi:tubular-bells*          15)     
(define *gmi:dulcimer*               16)     
(define *gmi:drawbar-organ*          17)     
(define *gmi:percussive-organ*       18)     
(define *gmi:rock-organ*             19)     
(define *gmi:church-organ*           20)     
(define *gmi:reed-organ             21)     
(define *gmi:accordion*              22)     
(define *gmi:harmonica*              23)     
(define *gmi:tango-accordion*        24)     
(define *gmi:acoustic-guitar-nylon* 25)     
(define *gmi:acoustic-guitar-steel* 26)     
(define *gmi:electric-guitar-jazz* 27)     
(define *gmi:electric-guitar-clean*  28)     
(define *gmi:electric-guitar-muted* 29)     
(define *gmi:overdriven-guitar*      30)     
(define *gmi:distortion-guitar*      31)     
(define *gmi:guitar-harmonics*       32)     
(define *gmi:acoustic-bass*          33)     
(define *gmi:electric-bass-finger* 34)     
(define *gmi:electric-bass-pick*   35)     
(define *gmi:fretless-bass*          36)     
(define *gmi:slap-bass-1*            37)     
(define *gmi:slap-bass-2*            38)     
(define *gmi:synth-bass-1*           39)     
(define *gmi:synth-bass-2*           40)     
(define *gmi:violin*                 41)     
(define *gmi:viola*                  42)     
(define *gmi:cello*                  43)     
(define *gmi:contrabass*             44)     
(define *gmi:tremolo-strings*        45)     
(define *gmi:pizzicato-strings*      46)     
(define *gmi:orchestral-harp*        47)     
(define *gmi:timpani*                48)     
(define *gmi:string-ensemble-1*      49)     
(define *gmi:string-ensemble-2*      50)     
(define *gmi:synthstrings-1*         51)     
(define *gmi:synthstrings-2*         52)     
(define *gmi:choir-aahs*     53)     
(define *gmi:voice-oohs*     54)     
(define *gmi:synth-voice*    55)     
(define *gmi:orchestra-hit*  56)     
(define *gmi:trumpet*        57)     
(define *gmi:trombone*       58)     
(define *gmi:tuba*           59)     
(define *gmi:muted-trumpet*  60)     
(define *gmi:french-horn*    61)     
(define *gmi:brass-section*  62)     
(define *gmi:synthbrass-1*   63)     
(define *gmi:synthbrass-2*   64)     
(define *gmi:soprano-sax*    65) 
(define *gmi:alto-sax*       66) 
(define *gmi:tenor-sax*      67) 
(define *gmi:baritone-sax*   68) 
(define *gmi:oboe*           69) 
(define *gmi:english-horn*   70) 
(define *gmi:bassoon*        71) 
(define *gmi:clarinet*       72) 
(define *gmi:piccolo*        73) 
(define *gmi:flute*          74) 
(define *gmi:recorder*       75) 
(define *gmi:pan-flute*      76) 
(define *gmi:blown-bottle*   77) 
(define *gmi:shakuhachi*     78) 
(define *gmi:whistle*        79) 
(define *gmi:ocarina*        80) 
(define *gmi:lead-1-square*        81) 
(define *gmi:lead-2-sawtooth*    82) 
(define *gmi:lead-3-calliope*     83) 
(define *gmi:lead-4-chiff*        84) 
(define *gmi:lead-5-charang*      85) 
(define *gmi:lead-6-voice*         86) 
(define *gmi:lead-7-fifths*        87) 
(define *gmi:lead-8-bass-lead*   88) 
(define *gmi:pad-1-newage*        89) 
(define *gmi:pad-2-warm*           90) 
(define *gmi:pad-3-polysynth*      91) 
(define *gmi:pad-4-choir*          92) 
(define *gmi:pad-5-bowed*          93) 
(define *gmi:pad-6-metallic*       94) 
(define *gmi:pad-7-halo*           95) 
(define *gmi:pad-8-sweep*          96) 
(define *gmi:fx-1-rain*            97) 
(define *gmi:fx-2-soundtrack*      98) 
(define *gmi:fx-3-crystal*         99) 
(define *gmi:fx-4-atmosphere*      100)
(define *gmi:fx-5-brightness*      101)
(define *gmi:fx-6-goblins*         102)
(define *gmi:fx-7-echoes*          103)
(define *gmi:fx-8-sci-fi*          104)
(define *gmi:sitar*        105)
(define *gmi:banjo*        106)
(define *gmi:shamisen*     107)
(define *gmi:koto*         108)
(define *gmi:kalimba*      109)
(define *gmi:bag-pipe*     110)
(define *gmi:fiddle*       111)
(define *gmi:shanai*       112)
(define *gmi:tinkle-bell*     113)
(define *gmi:agogo*           114)
(define *gmi:steel-drums*     115)
(define *gmi:woodblock*       116)
(define *gmi:taiko-drum*      117)
(define *gmi:melodic-tom*     118)
(define *gmi:synth-drum*      119)
(define *gmi:reverse-cymbal*         120)
(define *gmi:guitar-fret-noise*      121)
(define *gmi:breath-noise*     122)
(define *gmi:seashore*         123)
(define *gmi:bird-tweet*       124)
(define *gmi:telephone-ring*         125)
(define *gmi:helicopter*     126)
(define *gmi:applause*       127)
(define *gmi:gunshot*        128)







