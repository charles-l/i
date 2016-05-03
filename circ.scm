(require-extension irc posix posix-extras ncurses srfi-13)

(define server "irc.nixers.net")
(define channel "#nixers") ; TODO: allow array
(define nick "nc")
(define pfmt " > ")
(define sep " | ")
(define sep_col COLOR_CYAN)
(define nick_col COLOR_BLUE)
(define nick_len 10)

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
  (wprintw meswin "~S" (string-pad (if (> (string-length nick) nick_len)
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

(define (run-input inp)
  (if (= (string-length inp) 0)
    (void)
    (if (eq? (string-ref inp 0) #\/)
      (irc:command con (string-drop inp 1))
      (irc:say con inp))))

(define inp "")
(define (handle-input c)
  (let-values (((cury curx) (getyx prompt)))
    (if (not (eq? c ERR))
      (cond
        ((eq? c NL)
         (draw-msg (irc:connection-nick con) inp)
         (run-input inp)
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
          (wrefresh prompt)
          (set! inp (string-append inp (list->string `(,c)))))))))

;; main loop

(define (try thunk) ; catch errors
  (handle-exceptions exn
    (begin
      (wprintw meswin "error ~S\n" (->string (condition->list exn)))
      #f)
    (thunk)))

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
