#lang racket/base

(require racket/place racket/match racket/port
         "src/compiler/mzscheme-vm/compile.ss")

(provide compile/port/place)

(define the-place #f)

(define (get-the-place)
  (or the-place
      (begin
        (set! the-place (init-the-place))
        the-place)))


(define (init-the-place)
  (place chan
    (let loop ()
      (match (sync chan)
        [(list new-chan
               program-text #;pinfo
               program-name runtime)
         (call-with-input-string program-text
           (lambda (program-input-port)
             (place-channel-put new-chan
               (list
                 #f
               (call-with-output-bytes 
                 (lambda (program-output-port)
                   (begin
                     (displayln "Before compile")
                     (begin0 
                     (compile/port program-input-port
                                   program-output-port
      ;                             #:pinfo pinfo
                                   #:name program-name
                                   #:runtime-version runtime))
                     (displayln "after compile"))))))))])
      (loop))))

(define (compile/port/place program-text
            ;                #:pinfo pinfo
                            #:name program-name
                            #:runtime-version runtime)
  (define-values (pc1 pc2) (place-channel))
  (place-channel-put (get-the-place) (list pc2 program-text program-name runtime))
  (apply values (place-channel-get pc1)))
