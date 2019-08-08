#lang racket
(require "algorithm.rkt")
(provide (all-defined-out))
(require 2htdp/image 2htdp/universe)
(define n 4)
(define ellipse-width 25)
(define ellipse-height 20)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(drawing discs)

(define (draw-disc d)    
  (define colour "black")
  (cond [(= d 1) (set! colour "orange")]
        [(= d 2) (set! colour "yellow")]
        [(= d 3) (set! colour "green")]
        [(= d 4) (set! colour "blue")]
        [(= d 5) (set! colour "violet")]
        [(= d 6) (set! colour "pink")]
        [(= d 7) (set! colour "magenta")]
        [(= d 8) (set! colour "purple")]
        [(= d 9) (set! colour "gold")]
        [(= d 10) (set! colour "olive")]
        )    
  (ellipse (* d ellipse-width) ellipse-height 'solid colour))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;drawing single stack of discs

(define (draw-stack l)
  (apply above empty-image empty-image (map draw-disc l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;main game play interface

(define *width* 1000)
(define *length* 500)
(define (bground)
  (rectangle *width* *length* "solid" "CornflowerBlue" ))
(define (moov)
  (place-image (text "MOVES : " 36  "white") 200 455 (movebg)))
(define (restart)
  (define rec
    (rectangle 250 50 "solid" "blue"))
  (define p1 (place-image rec 500 455 (moov)))
  (place-image (text "RESTART ↺" 36  "white") 500 455 p1))
(define (hint)
  (define rec
    (rectangle 250 50 "solid" "blue"))
  (define p1 (place-image rec 800 455 (restart)))
  (place-image (text "HINT" 36  "white") 800 455 p1))
(define (movebg)
  (place-image (rectangle 1000 90 "solid" "Forest Green")
               500 455 (bground)))
  (define (towerbg)
    (define base (place-image (rectangle 1000 20 "solid" "Burlywood")
                              500 400 (hint)))
    (define tower
      (rectangle  15 300 "solid" "Burlywood"))
    
    (define (placer n)
      (cond [(=  n 1) (place-image tower
                                   (* 250 n) 240
                                   base)]
          [(<=  n 3) (place-image tower
                                  (* 250 n)  240
                                  (placer (- n 1)))]))
    (placer 3))
(define back-ground (towerbg))

(define (checker l)
  (not (and (null? (car l)) (null? (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;creation of world 

(struct world (stacks score))
(define c1 10)
(define (show)
  ;;initialisation of world
  (define s (world (list (build-list n add1) '() '()) 0))
  (define (draw-world w)  (render w))
  (define (settingc1? a b)
    (if (null? (list-ref b a )) (begin (set! c1 10) #f) #t))
  
  ;;mouseoperations
  (define (move-the-disc w x y k)
    (let* ([v (world-stacks w)]
           [sc (world-score w)])
      (cond [(and (restart-region? x y) (equal? k "button-down")) (begin (set! c1 10) s)]
            [(and (hint-region? x y) (equal? k "button-down") (checker v)) (world (the-hint-giver v) (+ sc 1))]
            [(and (hint-region? x y) (equal? k "button-down")) w]
            [else (if  (= 10 c1)  (cond [(equal? k "button-down") (cond[(r1 x y) (begin (set! c1 0) (if (settingc1? c1 v) w w ))]
                                                               [(r2 x y) (begin (set! c1 1) (if (settingc1? c1 v) w w ))]
                                                               [(r3 x y) (begin (set! c1 2) (if (settingc1? c1 v) w w ))]
                                                               [#t w])]
                                 [else w])
             (cond [(equal? k "button-down")
                    (cond [(and (r2 x y) (= 0 c1))
                           (begin (set! c1 10)  (move-disc 0 1 w)  )]
                          [(and (r2 x y)(= 2 c1))
                           (begin (set! c1 10)   (move-disc 2 1 w) )]
                          [(and (r1 x y) (= 1 c1))
                           (begin (set! c1 10) (move-disc 1 0 w)  )]
                          [(and (r1 x y) (= 2 c1))
                           (begin (set! c1 10)(move-disc 2 0 w))]
                          [(and (r3 x y) (= 0 c1))
                           (begin (set! c1 10)(move-disc 0 2 w))]
                          [(and (r3 x y) (= 1 c1))
                           (begin (set! c1 10)(move-disc 1 2 w))]
                          [else  w])]
                   [else w]))])))
  (big-bang s
            (on-mouse move-the-disc)
            (to-draw draw-world)
            (name "Tower of Hanoi")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;how move changes world

(define (move-disc a b w)
  (let* ([v (world-stacks w)])
    (cond [(null? (list-ref v a)) (set! c1 0) w]
          [(null? (list-ref v b))  (world (list-move (world-stacks w) a b) (+ 1 (world-score w)))]
          [(< (car (list-ref v a)) (car (list-ref v b))) (world (list-move (world-stacks w) a b) (+ 1 (world-score w)))]
          [(> (car (list-ref v a)) (car (list-ref v b))) w])))

(define (draw n l back)
  (let((a (length l)))
    (place-image (draw-stack l) (* n 250) (- 390 (* a 10)) back)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;render takes world ,converts list of stacks in it to an image


(define (render w)
  (let* ((x (world-stacks w))
         (sc (world-score w))
         (txt (number->string sc))
         (im (draw 1 (car x) (draw 2 (cadr x) (draw 3 (caddr x) back-ground))))
         (new-w (place-image (text txt 36 "white") 300 455 im))
         (optimal (number->string (- (expt 2 n) 1))))
    (cond [(and (null? (car x)) (null? (cadr x)) (= sc (- (expt 2 n) 1))) (place-image (text "You Won! and solved it in the least number of moves" 36 "white") 500 20 new-w)] 
          [(and (null? (car x)) (null? (cadr x)) (> sc (- (expt 2 n) 1))) (place-image (text (string-append "You Won! but can be solved in " optimal " moves") 36 "white") 500 20 new-w)]
          [else new-w])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;list-move generates new list of stacks if the from and to locations are given
(define (list-move list-of-list from to)
  (define from-disc (first (list-ref list-of-list from)))
  (for/list ([x (in-list list-of-list)]
             [i (in-naturals)])
    (cond
      [(= i from)
       (rest x)]
      [(= i to)
       (list* from-disc x)]
      [else
       x])))

;;;;;;;;regions for buttons of hint,restart,rod1,rod2,rod3
(define (hint-region? x y)
  (cond [(and (>= x 675) (<= x 925) (>= y 430) (<= y 480)) #t]
        [else #f]))
(define (restart-region? x y)
  (cond [(and (>= x 375) (<= x 625) (>= y 430) (<= y 480)) #t]
        [else #f]))


(define (r1 x y)
  (cond [(and (> x 235) (< x 265)
          (> y 90) (< y 390))#t]
        [#t #f]))
(define (r2 x y)
  (cond [(and (> x 485) (< x 515) (> y 90) (< y 390)) #t]
        [#t #f]))
(define (r3 x y)
  (cond [(and (> x 735) (< x 765) (> y 90) (< y 390)) #t]
        [#t #f]))
        
;;;;;;;;;;;startscreen and buttons in it
(define (toh t)
  (let* ((p1(place-image (text "TOWER" 50  "white") 500 55 (bground)))
         (p2(place-image (text "OF" 50  "white") 500 105 p1))
         (p3(place-image (text "HANOI" 50  "white") 500 155 p2))
         (rec(rectangle 400 50 "solid" "blue"))
         (p4(place-image  rec 500 220 p3))
         (p5(place-image rec 500 290 p4))
         (p6(place-image rec 500 360 p5))
         (p7(place-image rec 500 430 p6))
         (p8(place-image (text "PLAY GAME" 40  "white") 500 220 p7))
         (p9(place-image (text "PREFERENCES" 40  "white") 500 290 p8))
         (p10(place-image (text "INFORMATION" 40  "white") 500 360 p9))
         (startscreen(place-image (text "CREDITS" 40  "white") 500 430 p10)))
 (cond ((= t 0) startscreen)
       ((= t 1) (disc-num))
       ((= t 2) (info))
       ((= t 3)(credits)))))



;;;;;;;;regions for buttons of playgame,credits,information,preferences,menu
(define (newgame? x y)
  (cond ((and (>= x 200) (<= x 700) (>= y 195) (<= y 245)) #t)
        (else #f)))

(define (credits-region? x y)
  (cond ((and (>= x 200) (<= x 700) (>= y 405) (<= y 455)) #t)
        (else #f)))

(define (info-region? x y)
  (cond ((and (>= x 200) (<= x 700) (>= y 335) (<= y 385)) #t)
        (else #f)))

;;;;menu button region inside credits and 
(define (menu? x y)
  (cond ((and (>= x 200) (<= x 700) (>= y 225) (<= y 275)) (set! astate 0) #t)))
 
;;;;okbutton region inside preferences
(define (spreferences x y)
  (cond ((and (<= x 520) (>= x 480) (<= y 440)) (check (- y 70)))
        ((and (<= x 522) (>= x 478) (<= y 492) (>= y 448)) (set! astate 0) #t)
        (#t #t)))
(define (preferences? x y)
  (cond ((and (>= x 200) (<= x 700) (>= y 265) (<= y 315)) #t)
        (else #f)))


(define (mrender t x y k)
  (if (eq? k "button-down")
      (cond ((and (= 0 astate) (newgame? x y)) (show) (set! astate1 #f) astate)
            ((and (= 0 astate) (preferences? x y)) (set! astate 1) astate)
            ((= astate 1) (spreferences x y)  astate)
            ((and (= 0 astate) (info-region? x y)) (set! astate 2) astate)
            ((and (= 0 astate) (credits-region? x y)) (set! astate 3) astate)
            ((or(= astate 2)(= astate 3)) (menu? x y) astate)
            (#t t))
        t))

(define (check y)
  (cond ((and (>= y -20) (<= y 20)) (set! n 3))
        ((and (>= y 30) (<= y 70)) (set! n 4))
        ((and (>= y 80) (<= y 120)) (set! n 5))
        ((and (>= y 130) (<= y 170)) (set! n 6))
        ((and (>= y 180) (<= y 220)) (set! n 7))
        ((and (>= y 230) (<= y 270)) (set! n 8))
        ((and (>= y 280) (<= y 320)) (set! n 9))
        ((and (>= y 330) (<= y 370)) (set! n 10))
        (#t #t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (buttons in (prefernces))

(define (disc-num)
  (let*((bk (rectangle 1000 500 "solid" "cornflowerblue"))
        (bkn (rectangle 40 40 "solid" "forestgreen"))
        (cside (text "Choose the number of discs" 20 "white"))
        (nbk (place-image cside 500 30 bk))
        (obk (place-image (place-image (text "OK" 20 "white") 22 22 (rectangle 44 44 "solid" "maroon")) 500 470 nbk)))
  (define (helper c)
    (cond ((> c 10) obk)
          (#t (place-image (place-image (text (number->string c) 20 "white") 20 20 bkn) 500 (+ 70 (+ (* 50 (- c 3)))) (helper (+ c 1))))))
  (helper 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;text in info


(define (info)
  (let*((p1(place-image (text "The objective of the puzzle is to move the entire stack to rightmost rod, obeying the following simple rules:" 20 "white")
                        490 100 (bground)))
        (p2(place-image (text "1)Only one  disk can be moved at a time." 20 "white")
                        210 130 p1))
        (p3(place-image (text "2)Each move consists of taking the upperdisk from one of the stacks and placing it on top of another stack" 20 "white")
                          500 160 p2))
        (p4(place-image (text "3)No disk can be placed on top of a smaller disk." 20 "white")
               245 190 p3))
        (rec(rectangle 400 50 "solid" "blue"))
        (p5(place-image rec 500 250 p4))
        (p6(place-image (text "MAIN MENU" 40 "white") 500 250 p5))) 
  p6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;text in credits



(define (credits)
  (let*((p1 (place-image (text "Structural interface is inspired from " 40 "white")
                         500 100 (bground)))
        (p2 (place-image (text " TOWER OF HANOI  Playstore Application " 40 "white")
                         500 150 p1))
        (rec (rectangle 400 50 "solid" "blue"))
        (p3 (place-image rec 500 250 p2))
        (p4 (place-image (text "MAIN MENU" 40 "white") 500 250 p3)))
  p4))

(define astate 0)
(define astate1 #t)
(define (start1)(big-bang astate
          (to-draw toh)
          (on-mouse mrender)
          (name "Tower of Hanoi")))
 
(define s (world '((1 2 3 4) () ()) 0))
