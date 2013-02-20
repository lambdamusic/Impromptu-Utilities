;
;; library for working with XML - adapted (mostly unchanged) from the example from Andrew Sorensen
;

(define *xml:invalid-kind* 0)
(define *xml:document-kind* 1)
(define *xml:element-kind* 2)
(define *xml:attribute-kind* 3)
(define *xml:comment-kind* 6)
(define *xml:text-kind* 7)

; where dtd is an optional url string
(define xml:document-create
   (lambda dtd
      (let ((xmldoc (objc:make "NSXMLDocument" "init")))
         (if (not (null? dtd))
             (let* ((url (objc:call "NSURL" "URLWithString:" (car dtd)))
                    (dtdref (objc:make "NSXMLDTD" "initWithContentsOfURL:options:error:" url 0 0)))
                (objc:call xmldoc "setDTD:" dtdref)
                (objc:call xmldoc "setStandalone:" #f)))
         (objc:call xmldoc "setVersion:" "1.1")
         (objc:call xmldoc "setCharacterEncoding:" "UTF-8")
         xmldoc)))

; load xml from an existing document located at path
(define xml:load-document
   (lambda (path)
      (objc:make "NSXMLDocument" "initWithContentsOfURL:options:error:" 
                   (objc:call "NSURL" "fileURLWithPath:" path) 0 0)))


; set document root node
(define xml:set-root-node
   (lambda (xmldoc xmlnode)
      (objc:call xmldoc "setRootElement:" xmlnode)))

; gets the documents root node
(define xml:get-root-node
   (lambda (xmldoc)
      (objc:call xmldoc "rootElement")))

; returns the type of the node
(define xml:get-node-type
   (lambda (xmlnode)
      (objc:call xmlnode "kind")))

; returns the name of the node
(define xml:get-node-name
   (lambda (xmlnode)
      (objc:nsstring->string (objc:call xmlnode "name"))))

; returns the value associated with a text node or attribute node
(define xml:get-node-value
   (lambda (xmlnode)
      (objc:nsstring->string (objc:call xmlnode "stringValue"))))

;where name is a string and text if provided is a string
(define xml:element-create
   (lambda (name . text)
      (if (null? text)
          (objc:call "NSXMLNode" "elementWithName:" name)
          (objc:call "NSXMLNode" "elementWithName:stringValue:" name (car text)))))

; where parent and child are both elements
(define xml:add-child
   (lambda (parent child)
      (objc:call parent "addChild:" child)))

; add text node to element (can be either a number or a string)
(define xml:gfx:add-text
   (lambda (element text)
      (objc:call element "setStringValue:" text)))

; get all child nodes
(define xml:get-children
   (lambda (element)
      (objc:call element "children")))

;where name and value are both strings
(define xml:attribute-create
   (lambda (name value)
      (objc:call "NSXMLNode" "attributeWithName:stringValue:" name value)))

; add attribute to element
(define xml:add-attribute
   (lambda (element attribute)
      (objc:call element "addAttribute:" attribute)))

; get all attribute nodes
(define xml:get-attributes
   (lambda (element)
      (objc:call element "attributes")))

; get parent node
(define xml:get-parent
   (lambda (element)
      (objc:call element "parent")))

; return the xmldocument as a text string
(define xml:document->string
   (lambda (xmldoc)
      (let* ((str (objc:call xmldoc "XMLStringWithOptions:" 131072))
             (ret (objc:nsstring->string str)))
         ret)))

; return a node set from xpath query
(define xml:xpath
   (lambda (xmlnode xpath-exp)
      (objc:call xmlnode "nodesForXPath:error:" xpath-exp 0)))

;; End of OBJC library section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS

; utility to read in an xml file and convert to custom nested list structure
(define xml:xml->tree
   (lambda (xmldoc)
      (let loop ((node (xml:get-root-node xmldoc)))
         ;(print 'top node)
         (cond ((= (xml:get-node-type node) *xml:element-kind*)
                ;(print 'element node)
                (append (list (string->symbol (xml:get-node-name node)))
                        (map (lambda (node)
                                (loop node))
                             (append (objc:nsarray->list (xml:get-attributes node))
                                     (objc:nsarray->list (xml:get-children node))))))
               ((= (xml:get-node-type node) *xml:text-kind*)
                ;(print 'text node)
                (xml:get-node-value node))
               ((= (xml:get-node-type node) *xml:attribute-kind*)
                ;(print 'attr node)
                (cons (string->symbol (xml:get-node-name node))
                      (xml:get-node-value node)))
               (else (print-notification "ignoring node" node))))))

; utility function to walk over a custom nested list structure xml representation (see below)
(define xml:tree->xml
   (lambda (xmltree)
      (let ((xmldoc (xml:document-create))
            (root (xml:element-create (symbol->string (car xmltree)))))
         (xml:set-root-node xmldoc root)
         (let loop ((tree (cdr xmltree))
                    (parent root))
            (for-each (lambda (node)
                         (cond ((list? node)
                                (let ((element (xml:element-create (symbol->string (car node)))))
                                   (xml:add-child parent element)
                                   (loop (cdr node) element)))
                               ((pair? node)
                                (xml:add-attribute parent (xml:attribute-create (symbol->string (car node))
                                                                                (if (number? (cdr node))
                                                                                    (number->string (cdr node))
                                                                                    (cdr node)))))
                               ((string? node)
                                (xml:gfx:add-text parent node))
                               ((number? node)
                                (xml:gfx:add-text parent (number->string node)))))      
                      tree))
         xmldoc)))






;;;;;;;;
;;;;;;;;  mikele 
;;;;;;;;

; load xml from an existing url
(define xml:load-url
   (lambda (path)
      (objc:make "NSXMLDocument" "initWithContentsOfURL:options:error:" 
                   (objc:call "NSURL" "URLWithString:" path) 0 0)))



