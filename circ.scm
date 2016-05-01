(require-extension irc posix posix-extras ncurses srfi-13)

(define server "irc.freenode.net")
(define nick "testing123123asdfasdf")
(define pfmt " > ")
(define fmt "~S | ~S\n")

(define con (irc:connection server: server nick: nick))

;; hackjob hardcoded values

(define ERR (integer->char 2097151))
(define NL (integer->char 10))
(define BS (integer->char 127))

;; irc init

(irc:connect con)
(irc:join con "#testasdfasdf")

(irc:add-message-handler! con ; keep connection alive
  (lambda (msg)
    (irc:command con (string-append "PONG :" (car (irc:message-parameters msg)))) )
  tag: 'ping
  command: "PING")

;; ncurses init

(initscr)
(cbreak)
(noecho)

(define prompt (newwin 1 (COLS) (- (LINES) 1) 0))
(define meswin (newwin (- (LINES) 1) (COLS) 0 0))
(if (or (not prompt) (not meswin)) (error "failed to initialize ncurses"))
(nodelay prompt #t)  ; non-blocking
(scrollok meswin #t) ; scroll at bottom of screen

(define inp "")
(define (handle-input c)
  (let-values (((cury curx) (getyx prompt)))
    (if (not (eq? c ERR))
      (cond
        ((eq? c NL)
         (wprintw meswin fmt (string->symbol nick) (string->symbol inp))
         (wrefresh meswin)
         (irc:say con inp)
         (wclear prompt)
         (wprintw prompt pfmt)
         (set! inp ""))
        ((eq? c BS)
         (if (> curx (string-length pfmt))
           (begin
             (mvwdelch prompt 0 (- curx 1))
             (set! inp (string-take inp (- (string-length inp) 1))))))
        (else
          (waddch prompt c)
          (set! inp (string-append inp (list->string `(,c)))))))))

;; main loop

(let loop ()
  (let ((m (irc:listen con)))
    (if m (begin
            (wprintw meswin fmt
                     (string->symbol (car (irc:message-prefix m)))
                     (string->symbol (let ((b (irc:message-parameters m)))
                       (if (null? (cdr b)) (car b) (cadr b)))))
            (irc:process-message con m)
            (wrefresh meswin)))
    (handle-input (wgetch prompt))
    (wrefresh prompt))
  (sleep 0.01) ; throttle at ~60FPS ;)
  (loop))

(endwin)
