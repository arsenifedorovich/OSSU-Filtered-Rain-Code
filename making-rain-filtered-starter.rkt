(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt

;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP .)

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; for testing:

(define TLOD1 (cons (make-drop (/ WIDTH 2) (/ HEIGHT 2)) (cons (make-drop (/ WIDTH 4) (/ HEIGHT 4)) empty)))
(define TLOD2 (cons (make-drop (/ WIDTH 2) (/ HEIGHT 2)) (cons (make-drop WIDTH HEIGHT) empty)))
(define TLOD3 (cons (make-drop (/ WIDTH 2) (/ HEIGHT 2)) (cons (make-drop WIDTH (+ HEIGHT (/ (image-height DROP) 2))) empty)))
(define TLOD4 (cons (make-drop (/ WIDTH 2) (* 2 HEIGHT)) (cons (make-drop WIDTH (+ HEIGHT (/ (image-height DROP) 2))) empty)))


;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
    (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDro
    (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
    (to-draw  render-drops))) ; ListOfDrop -> Image

;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
;(define (next-drops lod) lod)

(check-expect (next-drops LOD1) empty)
(check-expect (next-drops TLOD1) (cons (make-drop (/ WIDTH 2) (+ (/ HEIGHT 2) SPEED)) (cons (make-drop (/ WIDTH 4) (+ (/ HEIGHT 4) SPEED)) empty)))
(check-expect (next-drops TLOD2) (cons (make-drop (/ WIDTH 2) (+ (/ HEIGHT 2) SPEED)) (cons (make-drop WIDTH (+ HEIGHT SPEED)) empty)))
(check-expect (next-drops TLOD3) (cons (make-drop (/ WIDTH 2) (+ (/ HEIGHT 2) SPEED)) empty))
(check-expect (next-drops TLOD4) empty)

(define (next-drops lod)
  (tick-drops (filter-drops lod)))

;; ListOfDrop -> ListOfDrop
;; produce filtered list of drops
; (define (filtered-drops) lod)

(check-expect (filter-drops LOD1) empty)
(check-expect (filter-drops TLOD1) TLOD1)
(check-expect (filter-drops TLOD2) TLOD2)
(check-expect (filter-drops TLOD3) (cons (make-drop (/ WIDTH 2) (/ HEIGHT 2)) empty))
(check-expect (filter-drops TLOD4) empty)

;; <template from ListOfDrop>

(define (filter-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (filtered-drop? (first lod))
             (cons
              (first lod)
              (filter-drops (rest lod)))
             (filter-drops (rest lod)))]))

;; Drop -> Drop
;; produce true if drop is filtered, else false
; (define (filtered-drop? d) false)

(check-expect (filtered-drop? (make-drop 0 0)) true)
(check-expect (filtered-drop? (make-drop (/ WIDTH 2) (/ HEIGHT 2))) true)
(check-expect (filtered-drop? (make-drop (/ WIDTH 4) (/ HEIGHT 4))) true)
(check-expect (filtered-drop? (make-drop WIDTH HEIGHT)) true)
(check-expect (filtered-drop? (make-drop WIDTH (+ HEIGHT (/ (image-height DROP) 2)))) false)
(check-expect (filtered-drop? (make-drop (/ WIDTH 2) (* 2 HEIGHT))) false)

;; <template from Drop>

(define (filtered-drop? d)
  ( < (drop-y d) (+ HEIGHT (/ (image-height DROP) 2))))

;; ListOfDrop -> ListOfDrop
;; produce ticked list of drops
; (define (tick-drops) lod)

(check-expect (tick-drops LOD1) empty)
(check-expect (tick-drops TLOD1) (cons (make-drop (/ WIDTH 2) (+ (/ HEIGHT 2) SPEED)) (cons (make-drop (/ WIDTH 4) (+ (/ HEIGHT 4) SPEED)) empty)))
(check-expect (tick-drops (cons (make-drop WIDTH HEIGHT) empty)) (cons (make-drop WIDTH (+ HEIGHT SPEED)) empty))

;; <template from ListOfDrop>

(define (tick-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (tick-drop (first lod))
              (tick-drops (rest lod)))]))

;; Drop -> Drop
;; produce ticked drop
; (define (tick-drop d) (make-drop 0 0))

(check-expect (tick-drop (make-drop (/ WIDTH 4) (/ HEIGHT 4))) (make-drop (/ WIDTH 4) (+ (/ HEIGHT 4) SPEED)))
(check-expect (tick-drop (make-drop (/ WIDTH 2) (/ HEIGHT 2))) (make-drop (/ WIDTH 2) (+ (/ HEIGHT 2) SPEED)))
(check-expect (tick-drop (make-drop WIDTH HEIGHT)) (make-drop WIDTH (+ HEIGHT SPEED)))

;; <template from Drop>

(define (tick-drop d)
  (make-drop (drop-x d) (+ (drop-y d) SPEED)))

;; ListOfDrop -> Image
;; Render the drops onto MTS
; (define (render-drops lod) MTS)

(check-expect (render-drops LOD1) MTS)
(check-expect (render-drops TLOD1) (place-image DROP (/ WIDTH 2) (/ HEIGHT 2) (place-image DROP (/ WIDTH 4) (/ HEIGHT 4) MTS)))
(check-expect (render-drops (cons (make-drop 0 0) empty)) (place-image DROP 0 0 MTS))

;; <template from ListOfDrop>

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (render-drops-on (first lod) (render-drops (rest lod)))]))

;; Drop Image -> Image
;; produce an image of a drop on a given image
; (define (render-drops-on d) MTS)

(check-expect (render-drops-on (make-drop 0 0) MTS) (place-image DROP 0 0 MTS))
(check-expect (render-drops-on (make-drop (/ WIDTH 2) (/ HEIGHT 2)) MTS) (place-image DROP (/ WIDTH 2) (/ HEIGHT 2) MTS))

;; <template from Drop>

(define (render-drops-on d img)
  (place-image DROP (drop-x d) (drop-y d) img))

;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
; (define (handle-mouse lod x y mevt) empty)

(check-expect (handle-mouse LOD1 3 4 "button-down") (cons (make-drop 3 4) empty))
(check-expect (handle-mouse LOD1 3 4 "move") empty)

;; <template from MouseEvent>

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else lod]))


(main empty)
















