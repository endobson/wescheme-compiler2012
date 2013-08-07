#!/usr/bin/env racket
#lang racket/base

;; This script is almost exactly like compiler-service, but is only meant
;; to be used to run the test suite.



(require web-server/servlet
         web-server/servlet-env
         scheme/runtime-path
         racket/path
         racket/port
         scheme/match
         scheme/list
         racket/cmdline
         "this-runtime-version.rkt"
         "places-compiler.rkt"
         ;; profile
         "find-paren-loc.rkt"
         "src/compiler/mzscheme-vm/write-support.ss"
         "src/compiler/mzscheme-vm/compile.ss"
         "src/compiler/mzscheme-vm/private/json.ss"
         "src/compiler/moby-failure.ss"
         "src/compiler/pinfo.ss"
         "src/collects/moby/runtime/permission-struct.ss"
         "src/collects/moby/runtime/error-struct.ss"
         "src/collects/moby/runtime/error-struct-to-dom.ss"
         "src/collects/moby/runtime/stx.ss"
         "js-runtime/src/sexp.ss"
         "src/collects/moby/runtime/dom-helpers.ss")

(define-runtime-path test-htdocs "tests/test-htdocs")
(define-runtime-path misc-runtime "js-runtime/lib")
(define-runtime-path htdocs "servlet-htdocs")
(define-runtime-path compat 
  "js-runtime/lib/compat")
(define-runtime-path easyxdm "support/easyXDM")




;; make-port-response: (values response/incremental output-port)
;; Creates a response that's coupled to an output-port: whatever you
;; write into the output will be pushed into the response.
(define (make-port-response #:mime-type (mime-type #"application/octet-stream")
                            #:headers (headers '()))
  (let-values ([(in out) (make-pipe)]
               [(CHUNK-SIZE) 1024])
    (values (response
             200 #"OK"
             (current-seconds)
             mime-type
             headers
             (lambda (op)
               (let loop ()
                 (let ([some-bytes (read-bytes CHUNK-SIZE in)])
                   (unless (eof-object? some-bytes)
                     (write-bytes some-bytes op)
                     (loop))))))
            out)))



(define (url-matches? a-url text)
  (for/or ([chunk (url-path a-url)])
     (and (string? (path/param-path chunk))
          (string=? (path/param-path chunk) text))))


;; has-program-text?: request -> boolean
;; returns true if the request includes program text.
(define (has-program-text? request)
  (exists-binding? 'program (request-bindings request)))

;; get-program-text: request -> string
;; Returns the textual content of the program.
(define (get-program-text request)
  (extract-binding/single 'program (request-bindings request)))


;; Web service consuming programs and producing bytecode.
(define (start request)
  (with-handlers ([void 
                   (lambda (exn-or-moby-error)
                     (define the-exn (if (exn? exn-or-moby-error)
                                         exn-or-moby-error
                                         (make-moby-failure
                                          (format "~a"
                                                  (string-append
                                                   (dom-string-content
                                                    (error-struct->dom-sexp exn-or-moby-error #f))
                                                   "\n"
                                                   (Loc->string (moby-error-location exn-or-moby-error))))
                                          (current-continuation-marks)
                                          exn-or-moby-error)))
                     (cond
                       [(jsonp-request? request)
                        (handle-json-exception-response request the-exn)]
                       [else 
                        (handle-exception-response request the-exn)]))])
    (cond
     [(url-matches? (request-uri request) "listTestPrograms")
      (list-test-programs request)]
     [(url-matches? (request-uri request) "getTestProgram")
      (get-test-program request)]
     [else
      (let*-values ([(program-name)
                     (string->symbol
                      (extract-binding/single 'name (request-bindings request)))]
                    [(program-text) 
                     (extract-binding/single 'program (request-bindings request))]
                    [(program-input-port) (open-input-string program-text)])
        ;; To support JSONP:
        (cond [(jsonp-request? request)
               (handle-json-response request program-name program-input-port)]
              [else
               (handle-response/place request program-name program-text)]))])))




(define (list-test-programs request)
  (let-values  ([(response output-port) (make-port-response #:mime-type #"text/plain")])
    (define test-programs (for/list ([p (directory-list test-htdocs)]
                                    #:when (regexp-match #px".scm$" (path->string p)))
                             (path->string p)))
    (write-json test-programs output-port)
    (close-output-port output-port)
    response))



;; get-test-program: request -> response
;; Write out the content of the test file as the response.
(define (get-test-program request)
  (let-values  ([(response output-port) 
                 (make-port-response #:mime-type #"text/plain")])
    (define base-name 
      (file-name-from-path
       (extract-binding/single 'name (request-bindings request))))
    (define filename (build-path test-htdocs base-name))
    (when (file-exists? filename)
      (call-with-input-file filename 
        (lambda (ip)
          (copy-port ip output-port))))
    (close-output-port output-port)
    response))








;; jsonp-request?: request -> boolean
;; Does the request look like a jsonp request?
(define (jsonp-request? request)
  (exists-binding? 'callback (request-bindings request)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsonp stuff

;; handle-json-response: -> response
(define (handle-json-response request program-name program-input-port)
  (let-values ([(response output-port) (make-port-response #:mime-type #"text/javascript")]
               [(compiled-program-port) (open-output-bytes)])
    (let ([pinfo (compile/port program-input-port compiled-program-port
                               #:name program-name
                               #:runtime-version THIS-RUNTIME-VERSION)])
      (fprintf output-port "~a(~a);" 
               (extract-binding/single 'callback (request-bindings request))
               (format-output (get-output-bytes compiled-program-port)
                              pinfo
                              request))
      (close-output-port output-port)
      response)))


;; format-output: bytes pinfo request -> bytes
;; Print out output.  If in json format, output is in the form:
;; { 'bytecode' : string,
;;   'permissions' : arrayof string }
;;
;; Otherwise, just returns the bytecode.
(define (format-output output-bytecode pinfo request)
  (cond
    [(wants-json-output? request)
     (let ([op (open-output-bytes)])
       (write-json (make-hash `((bytecode . ,(bytes->string/utf-8 output-bytecode))
                                (permissions . ,(get-android-permissions pinfo))))
                   op)
       (get-output-bytes op))]

    [else
     (format "(~a)"
	     ;; Raw version just spits the output bytecode.
	     output-bytecode)]))


(define (get-android-permissions pinfo)
  null
  #;
 (apply append 
        (map permission->android-permissions
             (pinfo-permissions pinfo))))
              
             


;; handle-json-exception-response: exn -> response
(define (handle-json-exception-response request exn)
  (case (compiler-version request)
    [(0)
     (let-values ([(response output-port) (make-port-response #:mime-type #"text/javascript")])
       (let ([payload
              (format "~a(~a);\n" (extract-binding/single 'on-error (request-bindings request))
                      (sexp->js (exn-message exn)))])
         (fprintf output-port "~a" payload)
         (close-output-port output-port)
         response))]
    [(1)
     (let-values ([(response output-port) (make-port-response #:mime-type #"text/javascript")])
       (let ([payload
              (format "~a(~a);\n" (extract-binding/single 'on-error (request-bindings request))
                      (jsexpr->json (exn->json-structured-output request exn)))])
         (fprintf output-port "~a" payload)
         (close-output-port output-port)
         response))]))
     

;;paren->loc: paren -> loc
;;converts a paren to a loc
(define (paren->loc p)
  (match p
    [(struct paren (text type paren-type p-start p-end))
     (make-Loc p-start 0 0 (- p-end p-start) "<definitions>")]))


;;paren->oppParen: paren -> string
;;takes in a paren and outputs the opposite paren as a string
(define (paren->oppParen p)
  (match p
    [(struct paren (text type paren-type p-start p-end))
    (get-match text)]))
     
;;parenCheck takes as input a string, and outputs a boolean.
;;The string that is input should be either ) } or ], which will return true. Else, false.
(define (parenCheck paren)
  (or (string=? paren ")")  
      (string=? paren "}") 
      (string=? paren "]")))



;; exn->structured-output: exception -> jsexpr
;; Given an exception, tries to get back a jsexpr-structured value that can be passed back to
;; the user.
;; exn->structured-output: exception -> jsexpr
;; Given an exception, tries to get back a jsexpr-structured value that can be passed back to
;; the user.
(define (exn->json-structured-output request an-exn)
  (define (on-moby-failure-val failure-val)
    (make-hash `(("type" . "moby-failure")
                 ("dom-message" . 
                                ,(dom->jsexpr 
                                  (error-struct->dom-sexp failure-val #f)))
                 ("structured-error" .
                  ,(jsexpr->json (make-hash `(("location" . ,(loc->jsexpr (moby-error-location failure-val)))
                                              ("message" . ,(error-struct->jsexpr failure-val)))))))))
  (cond
    [(exn:fail:read? an-exn)
     (define program (get-program-text request))
     (define input-port (open-input-string program))
     (define parens (paren-problem input-port))       ;;assuming that it is a paren problem 
     
     (let ([translated-srclocs 
            (map srcloc->Loc (exn:fail:read-srclocs an-exn))])
       (on-moby-failure-val
        (make-moby-error (if (empty? translated-srclocs)
                             ;; Defensive: translated-srclocs should not be empty, but
                             ;; if read doesn't give us any useful location to point to,
                             ;; we'd better not die here.
                             (make-Loc 0 1 0 0 "")
                             (first translated-srclocs))
                         (cond [(empty? parens)
                                
                                (make-moby-error-type:generic-read-error
                                 (exn-message an-exn)
                                 (if (empty? translated-srclocs) 
                                     empty
                                     (rest translated-srclocs)))]
                               [else
                                (make-Message "read: expected a "
                                              (get-match (paren-text (first parens)))
                                      
                                              (if (parenCheck (get-match (paren-text (first parens))))    
                                                  " to close "
                                                  " to open "
                                                  )

                                              (make-ColoredPart (paren-text (first parens)) (paren->loc (first parens)))
                                              (if (not (empty? (rest parens))) " but found a " "")
                                              (if (not (empty? (rest parens))) (make-ColoredPart (paren-text (second parens)) (paren->loc (second parens))) "")
                                              )]))))]

    [(moby-failure? an-exn)
     (on-moby-failure-val (moby-failure-val an-exn))]
    
    [else
     (exn-message an-exn)]))



;; dom->jsexpr: dom -> jsexpr
;; Translate a dom structure to one that can pass through.  The dom is treated as a nested list.
(define (dom->jsexpr a-dom)
  (match a-dom
    [(list head-name attribs body ...)
     `(,(symbol->string head-name)
       ,(map (lambda (k+v)
               (list (symbol->string (first k+v))
                     (second k+v))) 
             attribs)
       ,@(map dom->jsexpr body))]
    [else
     a-dom]))



;; srcloc->Loc: srcloc -> jsexp
;; Converts a source location (as stored in exceptions) into one that we can
;; store in error structure values.
(define (srcloc->Loc a-srcloc)
  (make-Loc (srcloc-position a-srcloc)
            (srcloc-line a-srcloc)
            (srcloc-column a-srcloc)
            (srcloc-span a-srcloc)
            (format "~a" (srcloc-source a-srcloc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non jsonp stuff: use with xmlhttprequest
(define (handle-response request program-name program-input-port)
  (let-values  ([(response output-port) (make-port-response #:mime-type #"text/plain")]
                [(program-output-port) (open-output-bytes)])
    (let ([pinfo (compile/port program-input-port program-output-port
                               #:name program-name
                               #:runtime-version THIS-RUNTIME-VERSION)])    
      (display (format-output (get-output-bytes program-output-port) pinfo request) output-port)
      (close-output-port output-port)
      response)))

(define (handle-response/place request program-name program-text)
  (let-values  ([(response output-port)
                 (make-port-response #:mime-type #"text/plain")])
    (let-values ([(pinfo program-output)
                  (compile/port/place program-text
                                      #:name program-name
                                      #:runtime-version THIS-RUNTIME-VERSION)])
      (display (format-output program-output pinfo request) output-port)
      (close-output-port output-port)
      response)))


;; wants-json-output?: request -> boolean
;; Produces true if the client wants json output.
;; json output is allowed to generate both the bytecode
;; and permissions strings
(define (wants-json-output? req)
  (let ([bindings (request-bindings req)])
    (and (exists-binding? 'format bindings)
         (string=? (extract-binding/single 'format bindings)
                   "json"))))
        
        
;; handle-exception-response: exn -> response
(define (handle-exception-response request exn)
  (case (compiler-version request)
    [(0)
     (response/full 500 
                    #"Internal Server Error"
                    (current-seconds)
                    #"application/octet-stream"
                    (list)
                    (list (string->bytes/utf-8 (exn-message exn))))]
    [(1)
     (response/full 500 
                    #"Internal Server Error"
                    (current-seconds)
                    #"application/octet-stream"
                    (list)
                    (list (string->bytes/utf-8 
                           (jsexpr->json (exn->json-structured-output request exn)))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers


;; compiler-version: request -> number
;; versions should be interpreted as
;;
;; 0: no support for structured error messages.
;; 1: support for structured error messages.
(define (compiler-version request)
  (cond
    [(exists-binding? 'compiler-version (request-bindings request))
     (string->number (extract-binding/single 'compiler-version (request-bindings request)))]
    [else
     0]))



(define (Loc->string a-loc)
  (format "Location: line ~a, column ~a, span ~a, offset ~a, id ~s" 
          (Loc-line a-loc)
          (Loc-column a-loc)
          (Loc-span a-loc)
          (Loc-offset a-loc)
          (Loc-id a-loc)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Write out a fresh copy of the support library.
(call-with-output-file (build-path htdocs "support.js")
  (lambda (op)
    (write-support "browser" op))
  #:exists 'replace)


;; Also, write out the collections
(unless (directory-exists? (build-path htdocs "collects"))
  (make-directory (build-path htdocs "collects")))
(write-collections/dir (build-path htdocs "collects"))
;;(call-with-output-file (build-path htdocs "collections.js")
;;  (lambda (op)
;;    (write-collections op))
;;  #:exists 'replace)


(define port 8000)
(void (command-line #:program "servlet"
                    #:once-each
                    [("-p" "--port") p "Port (default 8000)" (set! port (string->number p))]))


(serve/servlet start 
               #:port port
               #:servlet-path "/"
               #:servlet-regexp #px"^/(servlets/standalone.ss|listTestPrograms|getTestProgram)"
               #:extra-files-paths (list test-htdocs misc-runtime htdocs compat easyxdm)
               #:launch-browser? #f
               #:listen-ip #f)
