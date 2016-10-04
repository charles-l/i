(use tcp srfi-13)

(define pw #f)
(define username "ncharlie")
(define realuser "ncharlie")
(define nick "ncharlie")

(define (%connection-in)
  (car *connection*))

(define (%connection-out)
  (cdr *connection*))

(define (%i-send str . args)
  (write-line (apply string-append str args)
              (%connection-out)))

(define (%i-read)
  (read-line (%connection-in)))

(define-syntax with-connection
  (syntax-rules ()
                ((with-connection con ...)
                 (fluid-let ((*connection* con))
                            ...))))

(define (i-connect server)
  (define-values (i o) (tcp-connect server 6667))
  (set! *connection* `(,i . ,o))
  (if pw (%i-send "PASS :" pw))
  (%i-send (i-command 'user username "0 * :" realuser))
  (%i-send (i-command 'nick nick))
  *connection*)

(define (i-join channel)
  (%i-send (i-command 'join channel)))

(define (i-say msg)
  (%i-send "PRIVMSG " msg ":"))

(define (i-command c . args)
  (string-append
    (string-upcase (->string c)) " "
    (string-join (map (cut ->string <>) args) " ")))

;;;

(with-connection (i-connect "irc.freenode.net")
                 ((i-join "#testingtesting123")
                  (let loop ((data (%i-read)))
                    (print data)
                    (loop (%i-read)))
                  (close-input-port (%connection-in))
                  (close-output-port (%connection-out))))
