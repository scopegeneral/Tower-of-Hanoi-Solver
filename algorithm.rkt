#lang racket
(require graph)
(require compatibility/mlist)
(provide (all-defined-out))
; builds a list of n same elements with value a.
; (make-list 4 2) ----> '(2 2 2 2)
(define (make-list n a)
  (build-list n (λ(x) a)))

; removes all s elements from a list
; (remove-all 2 '(1 2 2 3 4 5 2)) ---> '(1 3 4 5)
(define (remove-all s lst) 
  (filter (λ(x) (not (= x s))) lst))

; add-cdr adds n at the end of every element of list.
; (add-cdr (list '(1 2) '(1 3)) 2) ----> '(((1 2) . 2) ((1 3) . 2))
(define (add-cdr l n)
  (map (lambda(x) (cons x n)) l))

; remove-car removes first element in all lists in a list of list.
; (remove-car (list '(1 2) '(1 3))) --->  '((2) (3))
(define (remove-car l)
  (map (lambda(x) (cdr x)) l))

; initial-state of hanoi
; (initial-state 3) ---> '(1 1 1)  
(define (initial-state n)
  (make-list n 1))

; checks whether a move from
; start-tower to final-tower is possible in given state
; (check-move? 1 3 '(1 2 2 2 3 1)) ---> #t
; (check-move? 3 1 '(1 2 2 2 3 1)) ----> #f
(define (check-move? start-tower final-tower state)
  (cond ((null? state) #f)
        ((eq? (car state) start-tower) #t)
        ((eq? (car state) final-tower) #f)
        (else (check-move? start-tower final-tower (cdr state)))))

; moves disk from start-tower to final-tower and gives updated state
; if move is not possible it gives the same state
; (move-disk 1 3 '(1 2 2 2 3 1)) ----> '(3 2 2 2 3 1)
; (move-disk 3 1 '(1 2 2 2 3 1)) ----> '(1 2 2 2 3 1)
(define (move-disk start-tower final-tower state)
  (cond ((null? state) '())
        ((eq? (car state) final-tower) state)
        ((eq? (car state) start-tower) (cons final-tower (cdr state)))
        (else (cons (car state) (move-disk start-tower final-tower (cdr state))))))

; to-move-list is a list of moves Ex: (list '(1 3) '(1 2) '(2 3))
; event-move performs all the moves in the list
; and stores the new state in to the list.
; (event-move (list '(1 2) '(1 3)) '(1 2 2 3 2 1)) ---> error "Illegal move"
; (event-move (list '(2 3) '(1 3)) '(1 2 2 3 2 1)) ---> '((1 3 2 3 1 1) (3 3 2 3 1 1))
(define (event-move to-move-list state)
    (if (null? to-move-list) 
        '()
        (let*  [(next-move  (car to-move-list))
               (start-tower (car next-move))
               (final-tower (cadr next-move))
               (next-state (move-disk start-tower final-tower state))]
          (if (check-move? start-tower final-tower state)
             (append (list next-state) (event-move (cdr to-move-list) next-state))
              (error 'event-move "Illegal move.")
              ))))

; standard tower-of-hanoi
; first shift top n-1 disks from home-tower to spare-tower
; then  shift  largest disk from home-tower to target-tower
; lastly shift the n-1 disks from spare-tower to target-tower
; (hunt 1 3 5) ----> '((1 3) (1 2) (3 2) (1 3) (2 1) (2 3) (1 3)
;                      (1 2) (3 2) (3 1) (2 1) (3 2)(1 3) (1 2) (3 2)
;                      (1 3) (2 1) (2 3) (1 3) (2 1) (3 2) (3 1) (2 1)
;                      (2 3) (1 3) (1 2) (3 2) (1 3) (2 1) (2 3) (1 3))
(define (hunt home-tower target-tower n)
  (let ((spare-tower (car (remove-all home-tower (remove-all target-tower '(1 2 3))))))
       (if (<= n 0) '()
              (append (hunt home-tower spare-tower (- n 1))
                      (list (list home-tower target-tower))
                      (hunt spare-tower target-tower (- n 1))))))

; main-hanoi function gives all the states involved in minimum steps in sequential order.
; (hanoi 3) ---> '((1 1 1) (3 1 1) (3 2 1) (2 2 1) (2 2 3) (1 2 3) (1 3 3) (3 3 3))
(define (hanoi n)
  (let ((state (initial-state n)))
    (append (list state) (event-move (hunt 1 3 n) state))))

; Assigns a list to a number a which represents the state of the system at that vertex in graph.
; (f 2 3) ----> '(2 1 1)
; (f 40 4) ----> '(1 2 3 2)
(define (f a n)
    (let* [(t (expt 3 (- n 1)))]
        (cond [(= n 1) (list a)]
              [(<= a t) (append (f a (- n 1)) (list 1))]
              [(and (> a t) (<= a (* 2 t))) (if (even? n)
                                                (append (f (- a t) (- n 1)) (list 2))
                                                (append (f (- a t) (- n 1)) (list 3)))]
              [(> a (* 2 t)) (if (odd? n) (append (f (- a (* 2 t)) (- n 1)) (list 2))
                               (append (f (- a (* 2 t)) (- n 1)) (list 3)))])))


; creates an unweighted-graph like triangle with vertices x (+ x 1) (+ x 2).
(define (graph-helper x)
  (let* [(a x)
         (b (+ x 1))
         (c (+ x 2))]
  (unweighted-graph/adj (list (list a b c) (list b c a) (list c a b)))))

; Gives the three  base-graphs
; (get-edges (base-graph 1)) ----> '((1 3) (1 2) (2 1) (2 3) (3 1) (3 2))
(define (base-graph k)
 (graph-helper (expt 3 (- k 1))))

; Gives the value of the vertex at the rightmost position of the triangle
; (third-term 10) ----> 36905
; (third-term 3) ----> 18
(define (third-term n)
  (cond [(= n 1) 3]
        [(= n 2) 5]
        [(= n 3) 18]
        [else (+ (third-term (- n 2)) (* 2 (expt 3 (- n 2))) (expt 3 (- n 1)))]))

; It gives the third new edge formed while joining the graphs
; (third-term-list 3) ----> '((5 14))
(define (third-time-list n)
  (let ((a (third-term (- n 1))))
             (list (list a (+ a  (expt 3 (- n 1)))))))

; Gives the value of the vertex at the leftmost position of the triangle
; (second-term 10) ----> 51669
; (second-term 3) ----> 23
(define (second-term n)
  (- (third-term (+ n 1)) (expt 3 n)))

; It gives the second new edge formed while joining the graphs
; (second-term-list 3) ----> '((9 27))
(define (second-time-list n)
  (let ((a (second-term (- n 1))))
             (list (list a (+ a  (* 2 (expt 3 (- n 1))))))))

; It gives the first new edge formed while joining the graphs
; (first-term-list 3) ----> '((10 19))
(define (first-time-list n)
  (let ((x (expt 3 (- n 1))))
    (list (list (+ 1 x) (+ 1 (* 2 x))))))

; changer sets the vertices of a graph
; to a list using the function f defined above
(define (changer g n t)
  (let ((a (expt 3 (- n 1))))
  (for ([i (build-list a  (λ(x) (+ x 1)))])
         (rename-vertex! g i (+ i (* t a)))))
  g)

; join-graph creates state diagram in form of a unweighted-graph
; for the tower of hanoi with n disks 
(define (join-graph n)
  (cond [(= n 1) (base-graph 1)]
        [else (begin (define e (join-graph (- n 1)))
                     (define p (graph-copy e))
                    (let* (
                    ( a (list->mlist (get-edges e)))
                    ( b (list->mlist (get-edges (changer e n 1))))
                    ( c (list->mlist (get-edges (changer p n 2))))
                    ( d (list->mlist (append (first-time-list n) (second-time-list n)
                                      (third-time-list n)))))
                (unweighted-graph/undirected (mlist->list (mappend! a b c d)))))]))



; finder finds the vertex to which l is associated, similar to inverse f
; (finder '(1 2 3 1 2) 5) ----> 175
(define (finder l n)
  (define (helper l n r)
    (if (= n 1) (+ r (car l) -1)
        (let* ((t (expt 3 (- n 1)))
           (a (car l)))
          (cond ((= a 1) (helper (cdr l) (- n 1) r))
                ((= a 2) (helper (cdr l) (- n 1) (if (odd? n) (+ r (* 2 t)) (+ r t))))
                ((= a 3) (helper (cdr l) (- n 1) (if (odd? n) (+ r t) (+ r (* 2 t)))))))))
  (helper (reverse l) n 1))


; the-next-list-move function finds the next state while tracing the path in minimum
; possible steps using BFS and dijkstars algorithm.
; (the-next-list-move '(3 2 1 2 3 1 2 2 1) 9) ---> '(3 1 1 2 3 1 2 2 1)
(define (the-next-list-move l n)
  (define k (join-graph n))
  (define k1 (graph-copy k))
  (define (the-state-graph n v)
  (define a k)
  (define-vertex-property a listifier #:init 0)
  (let* ((l (get-vertices a)))
  (for ([i l])
    (listifier-set! i (f i n)))
  (listifier v)))
  (let* [(a (finder l n))
         (b (finder (make-list n 3) n))
         (c (cadr (fewest-vertices-path k1 a b)))]
    (the-state-graph n c)))

; disc-finder converts our world state list-of-list
; into new-defined graph list
; (discfinder (list '(3) '() '(1 2))) ----> '(3 3 1)
(define (discfinder list-of-list)
  (define (helper  l n)
    (if (null? l) '()
        (append (add-cdr (car l) n) (helper (cdr l) (+ n 1)))))
  (let*((sorted (sort (helper list-of-list 1) (lambda(x y) (< (car x) (car y))))))
    (remove-car sorted)))

; list-of-discs convert our new defined graph list
; into world state list-of-list
; (list-of-discs '(3 3 1)) ---> '((3) () (1 2))
(define (list-of-discs l)
  (define n 0)
  (define (s) (set! n (+ n 1)))
  (define g  reverse )
  (define (helper l l1 l2 l3)
    (cond [(null? l) (list (g l1) (g l2) (g l3))]
          [(= (car l) 1) (s) (helper (cdr l) (cons n l1) l2 l3)]
          [(= (car l) 2) (s) (helper (cdr l) l1 (cons n l2) l3)]
          [(= (car l) 3) (s) (helper (cdr l) l1 l2 (cons n l3))]))
  (helper l '() '() '()))

; the-hint-giver takes a state in the list and gives best possible next
; state as an output
; (the-hint-giver (list '(1 2 6) '(4 ) '(5))) ---> '((2 5) (1 3) (4))
(define (the-hint-giver l)
  (let* [(a (length (append* l)))
         (b (discfinder l))
         (c (the-next-list-move b a))
         (d (list-of-discs c))]
    d))
