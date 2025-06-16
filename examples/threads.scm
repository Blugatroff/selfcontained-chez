(import (chezscheme))

(define (fork-joinable thunk)
  (let ((convar (make-condition))
        (mutex (make-mutex))
        (done #f)
        (value '()))
    (fork-thread (lambda ()
                   (set! value (thunk))
                   (set! done #t)
                   (condition-broadcast convar)))
    (lambda ()
      (with-mutex mutex
                  (do () (done value)
                    (condition-wait convar mutex))))))

(define output-mutex (make-mutex))

(define (f)
  (let go ((i 0))
    (if (< i 5)
      (begin
        (with-mutex output-mutex
          (display "Thread ") (display (get-thread-id)) (display ": iteration ") (display i) (newline))
        (sleep (make-time 'time-duration 500000000 0))
        (go (+ 1 i)))
      'Yay)))

(display (map
  (lambda (join) (join))
  (list
    (fork-joinable f)
    (fork-joinable f))))

