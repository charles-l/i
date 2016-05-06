(require-extension irc posix posix-extras ncurses srfi-13)

(load "config.scm")

(define con (irc:connection server: server nick: nick))

;; hackjob hardcoded values

(define ERR (integer->char 2097151))
(define NL (integer->char 10))
(define BS (integer->char 127))

;; irc init

(irc:connect con)
(irc:join con channel)

(irc:add-message-handler! con ; keep connection alive
  (lambda (msg)
    (irc:command con (string-append "PONG :" (car (irc:message-parameters msg)))) )
  tag: 'ping
  command: "PING")

;; ncurses init

(initscr)
(start_color)
(cbreak)
(noecho)

(init_pair 1 sep_col COLOR_BLACK)
(init_pair 2 nick_col COLOR_BLACK)

(define prompt (newwin 1 (COLS) (- (LINES) 1) 0))
(define meswin (newwin (- (LINES) 1) (COLS) 0 0))

(nodelay prompt #t)  ; non-blocking
(scrollok meswin #t) ; scroll at bottom of screen

(define (draw-msg nick msg)
  (wattron meswin (COLOR_PAIR 2))
  (wprintw meswin "~A" (string-pad (if (> (string-length nick) nick_len)
                                (string-take nick nick_len)
                                nick)
                              nick_len))
  (wattroff meswin (COLOR_PAIR 2))
  (wattron meswin (COLOR_PAIR 1))
  (wprintw meswin sep)
  (wattroff meswin (COLOR_PAIR 1))
  (wprintw meswin msg)
  (wprintw meswin "\n")
  (wrefresh meswin))

(define (redraw-prompt)
  (wclear prompt)
  (wprintw prompt pfmt)
  (wrefresh prompt))

(define (quit-i)
  (endwin)
  (quit))

(define (run-input inp)
  (if (= (string-length inp) 0)
    (void)
    (if (eq? (string-ref inp 0) #\/)
      (let ((s (string-drop inp 1)))
        (irc:command con s)
        (irc:say con inp)
        (cond
          ((equal? s "quit")
           (quit-i)))))))

(define inp "")
(define (handle-input c)
  (let-values (((cury curx) (getyx prompt)))
    (if (not (eq? c ERR))
      (cond
        ((eq? c NL)
         (draw-msg (irc:connection-nick con) inp)
         (run-input inp)
         (redraw-prompt)
         (set! inp ""))
        ((eq? c BS)
         (if (> curx (string-length pfmt))
           (begin
             (mvwdelch prompt 0 (- curx 1))
             (set! inp (string-take inp (- (string-length inp) 1))))))
        (else
          (waddch prompt c)
          (wrefresh prompt)
          (set! inp (string-append inp (list->string `(,c)))))))))

;; main loop

(define (try thunk) ; catch errors
  (handle-exceptions exn
    (begin
      (wprintw meswin "error ~A\n" (->string (condition->list exn)))
      #f)
    (thunk)))

(redraw-prompt)
(let loop ()
  (let ((m (try (lambda () (irc:listen con)))))
    (if m
      (begin
        (draw-msg
          (car (let ((p (irc:message-prefix m)))
                 (if (null? p)
                   '("")
                   p)))
          (let ((b (irc:message-parameters m)))
            (if (null? (cdr b)) (car b) (cadr b))) )
        (irc:process-message con m)))
    (handle-input (wgetch prompt))
    (wrefresh prompt))
  (sleep 0.01) ; throttle at ~60FPS ;)
  (loop))

(endwin)
