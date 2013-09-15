;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  sys:
;;  functions for system operations
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;



(define *test* #f)

(sys:livecoding-error-hook? #t)


; installation-specific locations

(define *sys:home_dir* "/Users/michele.pasin/Dropbox/code/impromptu/")
(define *sys:bat_dir* "/Users/michele.pasin/SugarSync/music/Battery/")
(define *sys:patches_dir* "/Users/michele.pasin/Dropbox/code/impromptu/_patches/")
(define *sys:tmp* "/Users/michele.pasin/tmp/")






;;;;;;;;;;;
;;;;;;; applescript bridge
;;;;;;;;;;;


;; eg (sys:open_finder_at2 *sys:patchesabsynth_dir*) :: this version doesn't always work
(define sys:open_finder_at2 
   (lambda (location)
      (let* ((llocation (string-replace location "/" ":"))
              (script (string-append "tell application \"Finder\" to activate open folder \"Macintosh HD" llocation "\"")))
         (objc:call (objc:call (objc:call "NSAppleScript" "alloc") 
                               "initWithSource:" 
                               script) 
                    "executeAndReturnError:" ))))

;; this one uses the terminal and it seems to work always
(define sys:open_finder_at
   (lambda (location)
      (let ((script (string-append "tell application \"Terminal\" to do script \"cd " location "; open .; exit\"")))
         (objc:call (objc:call (objc:call "NSAppleScript" "alloc") 
                               "initWithSource:" 
                               script) 
                    "executeAndReturnError:" ))))



(define sys:open_terminal_at
   (lambda (location)
      (let ((script (string-append "tell application \"Terminal\" to do script \"cd " location ";\"")))
         (objc:call (objc:call (objc:call "NSAppleScript" "alloc") 
                               "initWithSource:" 
                               script) 
                    "executeAndReturnError:" ))))







;;;;;;;;;;;;;;;;;;;
;WEBBY STUFF
;;;;;;;;;;;;;;;;;;;


;; open-url: calls the default mac browser with a url argument 
;; disclaimer: I'm not an objc programmer... found an example at 
;; http://macosx.com/forums/software-programming-web-scripting/18422-how-do-i-launch-url-using-cocoa-objective-c.html

(define sys:open-url 
   (lambda (urlstring)
      (let ((urlobj (objc:call "NSURL" "URLWithString:" urlstring))
            (workspace (objc:call "NSWorkspace" "sharedWorkspace")))
         (objc:call workspace "openURL:" urlobj))))



;;;;;;;;;;
;; impromptu wiki url caller
; eg: (wiki gfx:scale-path) ;; looks up the gfx:scale-path documentation online
;
; More info on: http://www.michelepasin.org/musicblog/2010/02/15/impromptu-function-to-access-wiki-docs-from-the-editor/


(define _wikiescape 
   (lambda (funname)
      (for-each (lambda (x)
                   (set! funname (cl:string-replace funname (car x) (cadr x))))
                '(("+" "%2B") 
                  ("=" "%3D") 
                  ("<" "lessthan") 
                  (">" "greaterthan") 
                  ("*" "%2A") 
                  ("?" "%3F") 
                  ("!" "%21")                 
                  ))
      (cl:string-capitalize funname)))


(define _wikiurl
   (lambda (funname)
      (let* ((urlbase "http://moso.com.au/wiki/index.php?title=")
             (newname (_wikiescape funname))
             (url (string-append urlbase newname)))
         (sys:open-url url))))


;; macro wrapper 
(define-macro (wiki name)
   `(_wikiurl (sexpr->string (quote ,name)))) 






