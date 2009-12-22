;;; parsec.ss
;; A parser combinators library.

#!r6rs

(library (parsec)
  (export parse catch fail eof? bof? pop push input-of

          define-parser parser-body sequence

          prev next try choice many many-1 sep-by sep-by-1 satisfy lexeme
          literal symbol

          input? make-input input-value input-state input-position input-fail
          result? make-result result-value result-input
          failure? make-failure failure-message failure-input)
  (import (rnrs)
          (only (scheme) printf define-for-syntax syntax-case*))


  (define-record-type input
    (fields value state position fail))

  (define-record-type result
    (fields value input))

  (define-record-type failure
    (fields message input))


  (define (parse p in)
    (call/cc
     (lambda (cc)
       (if (input? in)
           (p (make-input (input-value in) (input-state in) (input-position in) cc))
           (p (make-input in '() 0 cc))))))

  (define (catch p)
    (lambda (in)
      (parse p in)))

  (define (fail in msg)
    (printf "fail: ~A\n" msg)           ; debug
    ((input-fail in) (make-failure msg in)))

  (define (eof? in)
    (= (input-position in) (string-length (input-value in))))

  (define (bof? in)
    (zero? (input-position in)))

  (define (pop in)
    (make-input (input-value in) (input-state in) (+ (input-position in) 1) (input-fail in)))

  (define (push in)
    (make-input (input-value in) (input-state in) (- (input-position in) 1) (input-fail in)))

  (define (input-of r default)
    (cond
     ((result? r) (result-input r))
     ((failure? r) (failure-input r))
     (else default)))


  ;; Parser creation macros
  (define-for-syntax (symbolic-identifier=? id1 id2)
    (eq? (syntax->datum id1)
         (syntax->datum id2)))

  (define-syntax define-parser
    (lambda (stx)
      (syntax-case stx ()
        ((_ name body ...)
         #'(define name
             (lambda (in)
               (parser-body in body ...)))))))

  (define-syntax sequence
    (lambda (stx)
      (syntax-case stx ()
        ((_ body ...)
         #'(lambda (in)
             (parser-body in body ...))))))

  (define-syntax parser-body
    (lambda (stx)
      (syntax-case* stx (return bind fail eof? bof? if cond begin let let* letrec)
        symbolic-identifier=?
        ((_ in (if x a b))           #'(if (parser-body in x) (parser-body in a) (parser-body in b)))
        ((_ in (begin body ...))     #'(begin (parser-body in body ...)))
        ((_ in (let bs body ...))    #'(let bs (parser-body in body ...)))
        ((_ in (let* bs body ...))   #'(let* bs (parser-body in body ...)))
        ((_ in (letrec bs body ...)) #'(letrec bs (parser-body in body ...)))
        ((_ in (bind a b) rest ...)  #'(let* ((a (b in))
                                              (in (make-input
                                                   (input-value (input-of a in))
                                                   (input-state (input-of a in))
                                                   (input-position (input-of a in))
                                                   (input-fail in))))
                                         (parser-body in rest ...)))
        ((_ in (fail msg))           #'(fail in msg))
        ((_ in (return x))           #'(let ((val x))
                                         (if (procedure? val)
                                             (begin
                                               (let* ((r (val in))
                                                      (in (input-of r in)))
                                                 (make-result r in)))
                                             (make-result val in))))
        ((_ in (eof?))               #'(eof? in))
        ((_ in (bof?))               #'(bof? in))
        ((_ in x)                    #'(let ((val x))
                                         (cond
                                          ((result? val) (make-result (result-value val) in))
                                          ((procedure? val) (val in))
                                          (else val))))
        ((_ in x rest ...)           #'(let ((val x))
                                         (if (procedure? val)
                                             (let ((in (input-of (val in) in)))
                                               (parser-body in rest ...))
                                             (begin val (parser-body in rest ...))))))))

  ;; Combinators
  (define (next)
    (lambda (in)
      (if (eof? in)
          (fail in "Unexpected EOF.")
          (make-result (string-ref (input-value in) (input-position in)) (pop in)))))

  (define (prev)
    (lambda (in)
      (if (bof? in)
          (fail in "Unexpected BOF.")
          (make-result (string-ref (input-value in) (- (input-position in) 1)) (push in)))))

  (define-parser (choice . ps)
    (if (null? ps)
        (fail "No matches in choice.")
        (begin
          (bind pos input-position)
          (bind r (catch (car ps)))
          (bind pos* input-position)

          (if (result? r)
              r
              (if (= pos pos*)
                  (apply choice (cdr ps))
                  (fail "choice: Parse failed and consumed input."))))))

  (define-parser (many p)
    (bind pos input-position)
    (bind r (catch p))
    (bind pos* input-position)

    (if (result? r)
        (if (= pos pos*)
            (error 'many "Applied to parser that accepts an empty string.")
            (begin
              (bind rs (many p))
              (return (cons r (result-value rs)))))
        (return '())))

  (define-parser (many-1 p)
    (bind r p)
    (bind rs (many p))
    (return (cons r (result-value rs))))

  (define-parser (satisfy . fs)
    (if (null? fs)
        (fail "Token did not satisfy predicate.")
        (begin
          (bind tok (next))
          (if ((car fs) (result-value tok))
              tok
              (begin
                (prev)
                (apply satisfy (cdr fs)))))))

  (define-parser (lexeme p)
    (bind r p)
    (many (satisfy char-whitespace?))
    r)

  (define-parser (literal s)
    (letrec ((char-seq
              (lambda (chars)
                (sequence
                  (if (null? chars)
                      (return '())
                      (begin
                        (bind c (satisfy (lambda (c) (char=? c (car chars)))))
                        (bind cs (char-seq (cdr chars)))
                        (return (cons (result-value c) (result-value cs)))))))))

      (bind seq (char-seq (string->list s)))
      (make-result
       (list->string (result-value seq))
       (result-input seq))))

  (define-parser (symbol s)
    (lexeme (literal s)))

  (define-parser (try p)
    (bind pos input-position)
    (bind r (catch p))
    (if (failure? r)
        (make-failure
         (failure-message r)
         (make-input
          (input-value (failure-input r))
          (input-state (failure-input r))
          pos
          (input-fail (failure-input r))))
        r))

  (define-parser (sep-by delim p)
    (bind r (catch p))
    (if (result? r)
        (begin
          (bind rs (many (sequence delim p)))
          (return (cons r (result-value rs))))
        (return '())))

  (define-parser (sep-by-1 delim p)
    (bind r p)
    (bind rs (many (sequence delim p)))
    (return (cons r (result-value rs)))))
