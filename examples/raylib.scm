(import (chezscheme))

; This is not necessary, when compiling with -lraylib and registering the symbols below using FOREIGN_SYMBOLS.
(load-shared-object "libraylib.so")

(define init-window (foreign-procedure "InitWindow" (int int utf-8) void))
(define set-target-fps (foreign-procedure "SetTargetFPS" (int) void))
(define window-should-close (foreign-procedure "WindowShouldClose" () boolean))
(define begin-drawing (foreign-procedure "BeginDrawing" () void))
(define clear-background (foreign-procedure "ClearBackground" (unsigned-32) void))
(define end-drawing (foreign-procedure "EndDrawing" () void))
(define close-window (foreign-procedure "CloseWindow" () void))
(define draw-circle-v (foreign-procedure "DrawCircleV" (double float unsigned-32) void))

(define-ftype Vec2 (struct [x float] [y float]))
(define pos (make-ftype-pointer Vec2 (foreign-alloc (ftype-sizeof Vec2))))

(init-window 600 400 "Raylib from Scheme")
(set-target-fps 60)
(do ((i 0 (+ 1 i))) ((window-should-close) '())
  (begin-drawing)
  (clear-background #xFF000000)

  (let ([r (- 200 20)])
    (ftype-set! Vec2 (x) pos (exact->inexact (+ 300 (* r (sin (* 0.01 i))))))
    (ftype-set! Vec2 (y) pos (exact->inexact (+ 200 (* r (cos (* 0.01 i)))))))
  (draw-circle-v
    (foreign-ref 'double (ftype-pointer-address pos) 0)
    20.0
    #xFFFF00FF)

  (end-drawing))
(close-window)

