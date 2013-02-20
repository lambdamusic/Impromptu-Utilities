;;;;;;;;;;;;;;;;;;;;;;;;;; michele pasin
;;
;;
;;  patch:
;;  functions for working with audio instrument patches
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;




;; eg: (patch:load ++kontakt4 kit "Elab_files_house")
;; if available, it looks for scheme mappings on the same directory, in a file "Elab_files_house.scm"
(define patch:load
   (lambda (++instrumentType instance patch-name)
      (let* ((name (symbol->string ++instrumentType))
             (location (string-append *sys:patches_dir* name "/" patch-name))
             (mappingsfile (string-append location ".scm")))
         (if (io:file-exists? location)  
             (begin (au:load-preset instance location)
                    (print "Patch successfully loaded")
                    (if (io:file-exists? mappingsfile)
                        (begin (load mappingsfile) 
                               (print "Scheme Mappings successfully loaded"))
                        (print "No scheme Mappings available")))           
             (print (string-append "===========\nCould not find: " 
                                   location
                                   " \n===========\n"))))))


;; from an instrument ++name, shows the correlated patch dir if existing
(define patch:printdir 
   (lambda (++instrument)
      (let* ((name (symbol->string ++instrument))
             (location (string-append *sys:patches_dir* name)))
         (if (io:file-exists? location)            
             (print (string-append "===========\nAvailable patches for " 
                                   name
                                   ":\n===========\n" 
                                   (io:directory-list location)))
             (print (string-append "===========\nTrying to show available patches... directory for " 
                                   name
                                   " does not exist yet\n===========\n"))))))



(define patch:save
   (lambda (++instrumentType instance patch-name)
      (let* ((name (symbol->string ++instrumentType))
             (location (string-append *sys:patches_dir* name "/" patch-name)))
         (au:save-preset instance location)
         (print "Patch successfully saved"))))













