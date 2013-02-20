;; put this file in ~/Library/Application Support/Impromptu


(define *sys:home_libs* "/Users/code/impromptu/_libs/")

;; eg (util:loadlib "xml_lib.scm") // depends on *sys:home_libs*

(define util:loadlib
   (lambda (libname)
      (let ((location (string-append *sys:home_libs* libname)))
         (if (io:file-exists? location)  
             (begin (load location)
                    (print (string-append "Library file successfully loaded: " location)))      
             (print (string-append "===========\nCould not find: " 
                                   location
                                   " \n===========\n"))))))


; load every .scm file in the library folder - don't inspect directories..

(for-each (lambda (filename)
             (if (= (length (string-split filename ".scm")) 2)
                 (util:loadlib filename)))
	 (string-split (io:directory-list *sys:home_libs*) "\n"))

