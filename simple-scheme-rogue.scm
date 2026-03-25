;; ---------------------------------------------------------
;; A Minimal Roguelike in Standard Scheme
;; Created with Gemini 3.1 Pro
;; (C)Tsubasa Kato - 3/25/2026
;; Many things are not yet implemented.
;; ---------------------------------------------------------

;; Game State
(define player-x 5)
(define player-y 5)
(define score 0)
(define width 10)
(define height 10)

;; Simple map definition: 0 = floor, 1 = wall, 2 = treasure
(define game-map
  (vector
   (vector 1 1 1 1 1 1 1 1 1 1)
   (vector 1 0 0 0 2 0 0 0 0 1)
   (vector 1 0 1 1 1 0 1 1 0 1)
   (vector 1 0 0 0 0 0 0 2 0 1)
   (vector 1 1 1 0 1 1 1 1 0 1)
   (vector 1 0 0 0 0 0 0 0 0 1)
   (vector 1 0 1 1 1 1 1 1 0 1)
   (vector 1 0 0 0 2 0 0 0 0 1)
   (vector 1 0 0 0 0 0 0 0 0 1)
   (vector 1 1 1 1 1 1 1 1 1 1)))

;; Helper to read from the map
(define (get-tile x y)
  (vector-ref (vector-ref game-map y) x))

;; Helper to write to the map (e.g., removing collected treasure)
(define (set-tile! x y val)
  (vector-set! (vector-ref game-map y) x val))

;; Renders the map, player, and entities to the console
(define (draw-map)
  (newline)
  (display "Treasure Collected: ") 
  (display score) 
  (display "/3")
  (newline)
  (let loop-y ((y 0))
    (if (< y height)
        (let loop-x ((x 0))
          (if (< x width)
              (begin
                (cond
                 ;; Draw Player
                 ((and (= x player-x) (= y player-y)) (display "@ "))
                 ;; Draw Wall
                 ((= (get-tile x y) 1) (display "# "))
                 ;; Draw Treasure
                 ((= (get-tile x y) 2) (display "$ "))
                 ;; Draw Floor
                 (else (display ". ")))
                (loop-x (+ x 1)))
              (begin
                (newline)
                (loop-y (+ y 1))))))))

;; Handles player movement and collision detection
(define (move dx dy)
  (let ((nx (+ player-x dx))
        (ny (+ player-y dy)))
    (if (not (= (get-tile nx ny) 1)) ; Check if the target tile is not a wall
        (begin
          ;; Update player coordinates
          (set! player-x nx)
          (set! player-y ny)
          ;; Check for treasure
          (if (= (get-tile nx ny) 2)
              (begin
                (set! score (+ score 1))
                (set-tile! nx ny 0))))))) ; Remove treasure and replace with floor

;; The main game loop
(define (game-loop)
  (draw-map)
  (if (= score 3)
      (begin
        (newline)
        (display "You found all the treasure! You win!\n"))
      (begin
        (display "Move (w/a/s/d) and press Enter. 'q' to quit: ")
        (let ((input (read))) ; Read input as a symbol
          (cond
           ((eq? input 'w) (move 0 -1))
           ((eq? input 's) (move 0 1))
           ((eq? input 'a) (move -1 0))
           ((eq? input 'd) (move 1 0))
           ((eq? input 'q) (display "Thanks for playing!\n")))
          
          ;; Recurse unless the player quits
          (if (not (eq? input 'q))
              (game-loop))))))

;; Start the game
(game-loop)
