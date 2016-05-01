(require-extension irc posix posix-extras ncurses srfi-13)

(define server "irc.freenode.net")
(define nick "testing123123asdfasdf")
(define pfmt " > ")
(define sep " | ")
(define sep_col COLOR_CYAN)
(define nick_col COLOR_BLUE)

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
(start_color)
(cbreak)
(noecho)

(init_pair 1 sep_col COLOR_BLACK)
(init_pair 2 nick_col COLOR_BLACK)

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
         (wattron meswin (COLOR_PAIR 2))
         (wprintw meswin nick)
         (wattroff meswin (COLOR_PAIR 2))
         (wattron meswin (COLOR_PAIR 1))
         (wprintw meswin sep)
         (wattroff meswin (COLOR_PAIR 1))
         (wprintw meswin inp)
         (wprintw meswin "\n")
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
          (wrefresh prompt)
          (set! inp (string-append inp (list->string `(,c)))))))))

;; main loop

(let loop ()
  (let ((m (irc:listen con)))
    (if m (begin
            (wattron meswin (COLOR_PAIR 2))
            (wprintw meswin (car (irc:message-prefix m)))
            (wattroff meswin (COLOR_PAIR 2))
            (wattron meswin (COLOR_PAIR 1))
            (wprintw meswin sep)
            (wattroff meswin (COLOR_PAIR 1))
            (wprintw meswin (let ((b (irc:message-parameters m)))
                              (if (null? (cdr b)) (car b) (cadr b))))
            (wprintw meswin "\n")
            (irc:process-message con m)
            (wrefresh meswin)))
    (handle-input (wgetch prompt))
    (wrefresh prompt))
  (sleep 0.01) ; throttle at ~60FPS ;)
  (loop))

(endwin)
