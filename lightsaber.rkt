#lang racket

; lightsaber.rkt
;
; Draw lightsabers using Racket's pict library
; Created for Stephen De Gabrielle's Summer 2019 standard-fish competition
;
; Modification History
; -------------------------------------------------
; 07/27/2019   Justin Zamora   Initial creation

(provide lightsaber)

(require racket/contract)
(require racket/draw)
(require pict)

;-------------------------------------------------------
;
; Helper Functions

; Predicate for contract checking the supported hilt styles
(define hilt-style? (one-of/c 'luke 'vader 'kylo 'maul))

; Generate a shade of gray
(define (gray level)
  (make-color level level level))

; Flip a pict horizontally. This should really be a part of the pict library
(define (flip-horizontal pict)
  (inset (scale pict -1 1) (pict-width pict) 0))

; Flip a pict vertically. This should really be a part of the pict library
(define (flip-vertical pict)
  (inset (scale pict 1 -1) 0 (pict-height pict)))

;-------------------------------------------------------
;
; Main Function

; Create a pict of a lightsaber.  Accept a color object or a color name.
; An optional length can be specified (useful for animating blade extension)
(define/contract (lightsaber color
                             #:length [length 400]
                             #:style [hilt-style 'luke])
  (->* ((or/c string? (is-a?/c color%))) (#:length nonnegative-integer?
                                          #:style hilt-style?) pict?)

  ; Convert color to a color%, if necessary
  (define color-obj (cond
                      [(string? color) (send the-color-database find-color color)]
                      [else color]))

  ; Generate the hilt and blade and combine them.
  (let ([the-hilt (hilt hilt-style)])
    (cond
      ; This uses knowledge about how Kylo's saber is drawn that it
      ; probably shouldn't know in order to draw the cross-blades, but
      ; drawing the cross-blades in the hilt routine has its own
      ; disadvantages. 
      [(eq? hilt-style 'kylo) (cond
                                [(zero? length) the-hilt]
                                [else (let ([cross (blade color-obj (/ length 2.75))])
                                        (lc-superimpose (inset (rotate cross (/ pi 2))
                                                               100 7 0 0)
                                                        (inset (blade color-obj length)
                                                               (hilt-horiz-offset hilt-style)
                                                               (hilt-vert-offset hilt-style)
                                                               0 0)
                                                        the-hilt))])]
      ; If the blade length is zero, use a null blade so that the pict height
      ; stays the same regardless of length
      [else (lc-superimpose (inset (if (zero? length)
                                       (inset (hline 0 0) 0 50 0 0)
                                       (blade color-obj length))
                                   (hilt-horiz-offset hilt-style)
                                   (hilt-vert-offset hilt-style)
                                   0 0)
                            the-hilt)])))

; Create a pict of the lightsaber blade
(define (blade color length)
  (define r (send color red))
  (define g (send color green))
  (define b (send color blue))

  ; Produce glows with transparency and offset from the blade center
  (define (glow alpha offset)
    (filled-rounded-rectangle (+ length offset) offset -0.5
                              #:color (make-color r g b alpha)
                              #:draw-border? #false))

  ; Pict of the white core
  (define core (filled-rounded-rectangle (+ length 10) 10 -0.5
                                         #:color "White"
                                         #:draw-border? #false))

  ; Stack the glows and the core to create the blade
  (for/fold ([pict (glow 0.1 50)]
             [offset 40]
             #:result (cc-superimpose pict core))
            ([alpha (list 0.2 0.4 0.8)])
    (values (cc-superimpose (glow alpha offset) pict)
            (- offset 10))))

;-------------------------------------------------------
;
; Functions for drawing hilts

; Helper macro for filled-rectangle
(define-syntax filled-rect
  (syntax-rules ()
    [(filled-rect w h c) (filled-rectangle w h #:color c #:draw-border? #false)]))

; Helper macro for filled-rounded-rectangle
(define-syntax filled-rounded-rect
  (syntax-rules ()
    [(filled-rounded-rect w h c) (filled-rounded-rectangle w h 5 #:color c #:draw-border? #false)]))

; Draw the hilt, using the given style
(define/contract (hilt style)
  (-> hilt-style? pict?)
  (case style
    [(luke) luke-hilt]
    [(vader) vader-hilt]
    [(kylo) kylo-hilt]
    [(maul) maul-hilt]
    [else (error "Unrecognized style: ~a" style)]))

; How much to offset the blade horizontally for the specific style of hilt
(define (hilt-horiz-offset style)
  (case style
    [(luke) 111]
    [(vader) 111]
    [(kylo) 111]
    [(maul) 135]
    [else (error "Unrecognized style: ~a" style)]))

; How much to offset the blade vertically for the specific style of hilt
(define (hilt-vert-offset style)
  (case style
    [(luke) 7]
    [(vader) 1]
    [(kylo) 0]
    [(maul) 0]
    [else (error "Unrecognized style: ~a" style)]))

;-------------------------------------------------------
;
; Draw Luke Skywalker's lightsaber hilt
(define luke-hilt
  (let ([hilt-background
         (lc-superimpose
          ; Grip and Switch
          (inset (rb-superimpose
                  (inset (filled-rounded-rect 68 40 "Black") 0 0 0 0) ; Grip
                  (inset (filled-rounded-rect 31 20 "Black") 0 0 5 27)) ; Switch
                 0 0 0 0)
          ; Middle Tube
          (inset (filled-rounded-rect 41 30 "Black") 63 7 0 0)
          ; Gold Connector 1
          (inset (filled-rect 8 29 "Black") 106 6 0 0)
          ; Gold Connector 2
          (inset (filled-rect 8 21 "Black") 110 7 0 0)
          ; Emitter Base
          (inset (filled-rect 10 29 "Black") 117 8 0 1)
          ; Emitter Plate
          (inset (filled-rounded-rect 9 43 "Black") 126.5 7 0 0))]
        [hilt-foreground
         (hc-append (vl-append (filled-rect 2 10 (gray 90))
                               (filled-rect 2 7 (gray 125))
                               (filled-rect 2 10 (gray 90))
                               (filled-rect 2 7 (gray 30)))
                    (vl-append (filled-rect 5 10 (gray 140))
                               (filled-rect 5 7 (gray 185))
                               (filled-rect 5 10 (gray 140))
                               (filled-rect 5 7 (gray 45)))
                    ; Grip and Switch
                    (inset (rb-superimpose
                            ; Grip
                            (vl-append (filled-rect 55 10 (gray 175))
                                       (filled-rect 55 7 (gray 245))
                                       (filled-rect 55 10 (gray 175))
                                       (filled-rect 55 7 (gray 95)))
                            ; Switch and Shadow
                            (inset (vl-append (filled-rect 25 7 (make-color 165 95 35))
                                              (filled-rect 25 2 (make-color 90 40 5))
                                              (filled-rect 25 5 (gray 128)))
                                   0 0 5 27))
                           0 0 0 6)
                    ; Middle part of hilt
                    (vl-append (filled-rect 2 10 (gray 124))
                               (filled-rect 2 7 (gray 95))
                               (filled-rect 2 7 (gray 30)))
                    (vl-append (filled-rect 2 10 (gray 184))
                               (filled-rect 2 7 (gray 140))
                               (filled-rect 2 7 (gray 44)))
                    (vl-append (filled-rect 32 10 (gray 245))
                               (filled-rect 32 7 (gray 185))
                               (filled-rect 32 7 (gray 100)))
                    ; Emitter section
                    (filled-rect 7 15 (gray 25))
                    (filled-rect 2 25 (make-color 135 85 30)) ; Gold connector
                    (filled-rect 2 25 (make-color 185 135 85)) ; Gold connector
                    (filled-rect 5 17 (make-color 135 85 30)) ; Gold connector
                    (filled-rect 2 17 (make-color 100 60 10)) ; Gold connector
                    (filled-rect 5 25 (gray 155))
                    (filled-rect 3 25 (gray 130))
                    (filled-rect 2 25 (gray 90))
                    (filled-rect 2 37 (gray 150))
                    (filled-rect 2 37 (gray 230)))])
    (lc-superimpose hilt-background
                    (inset hilt-foreground 3 7 0 0))))

;-------------------------------------------------------
;
; Draw Darth Vader's lightsaber hilt

(define vader-hilt
  (lc-superimpose 
   (hc-append
    ; Main Tube
    (cc-superimpose (filled-rounded-rectangle 117 35 5 #:color "Black")
                    (vl-append (filled-rect 110 11 (gray 160))
                               (filled-rect 110 2 (gray 90))
                               (filled-rect 110 8(gray 245))
                               (filled-rect 110 8 (gray 80))))
    ; Emitter
    (inset/clip (shear (vl-append (filled-rect 87 15 (gray 40))
                                  (filled-rect 87 11 (gray 60))
                                  (filled-rect 87 9 (gray 20)))
                       -0.75 0)
                -75 0 0 0))
   ; Grips
   (inset (vl-append 9 (filled-rect 40 7 (gray 50))
                     (filled-rect 40 7 (gray 50))
                     (filled-rect 40 7 (gray 50)))
          8 0 0 0)
   ; Center Band
   (inset (vl-append (filled-rect 25 15 (gray 40))
                     (filled-rect 25 11 (gray 60))
                     (filled-rect 25 9 (gray 20))
                     (filled-rect 25 7 (gray 40)))
          65 7 0 0)
   ; Top Button
   (inset (filled-rect 15 5 (gray 60))
          105 0 0 34)
   ; Bottom Greeble
   (inset (filled-rect 10 6 "Black")
          110 34 0 0)))

;-------------------------------------------------------
;
; Draw Kylo Ren's lightsaber hilt

(define kylo-hilt
  (lc-superimpose
   ; Pommel and Grips
   (inset (filled-rectangle 6 35 #:color "Red" #:border-width 2) 1 1 0 0)
   (inset (rotate (filled-rectangle 25 5 #:color (gray 30) #:border-width 2)
                  (/ pi -24))
          11 0 0 40)
   (inset (rotate (filled-rectangle 25 5 #:color (gray 80) #:border-width 2)
                  (/ pi 24))
          11 43 0 0)

   ; Main Tube
   (inset (filled-rectangle 92 40 #:color "Black" #:border-width 2) 7 1 0 0)
   (inset (vl-append (filled-rect 92 10 (gray 30))
                     (filled-rect 92 10 (gray 60))
                     (filled-rect 92 10(gray 90))
                     (filled-rect 92 10 (gray 70)))
          7 0 0 0)

   ; Greebles
   (inset (filled-rectangle 12 12 #:color (gray 220) #:border-width 2) 9 0 0 0)
   (inset (filled-rectangle 30 10 #:color "LemonChiffon" #:border-width 2) 20 0 0 0)
   (inset (filled-rectangle 30 10 #:color (gray 235) #:border-width 2) 65 0 0 0)

   ; Quillons
   (let ([quillon (lt-superimpose (inset (shear (filled-rectangle 13 10
                                                                  #:color (gray 110)
                                                                  #:border-width 1)
                                                0 0.75)
                                         -0.5 0 0 0)
                                  (inset (filled-rectangle 22 19 #:color "Black"
                                                           #:border-width 2)
                                         0 8 2 0)
                                  (inset (vl-append (filled-rect 22 5 (gray 130))
                                                    (filled-rect 22 9 (gray 80))
                                                    (filled-rect 22 5 (gray 30)))
                                         0 8 0 0))])

     (inset (vc-append 45.25 quillon (flip-vertical quillon))
            114 0 0 0))

   ; Emitter
   (inset (filled-rectangle 45 44 #:color "Black" #:border-width 3) 99 0 0 0)
   (inset (vl-append (filled-rect 45 8 (gray 30))
                     (filled-rect 45 8 (gray 60))
                     (filled-rect 45 16(gray 90))
                     (filled-rect 45 12 (gray 70)))
          99 0 0 0)
   (inset (inset (filled-rectangle 10 44
                                   #:color (make-color 150 150 150 0.6)
                                   #:border-width 1) 1)
          100 0 0 0)
   (inset (linewidth 2 (vline 5 45)) 130 0 0 0)
   
   ; Red Wire
   (inset (colorize (linewidth 2 (hline 85 5)) "Firebrick") 14 0 0 32)))


;-------------------------------------------------------
;
; Draw Darth Maul's lightsaber hilt
(define maul-hilt
  (let ([segment (lambda (width) ; Draw shaded gradient for the hilt
                   (cc-superimpose (filled-rectangle (+ width 2) 36
                                                     #:color "Black"
                                                     #:border-width 2)
                                   (vc-append (filled-rect width 7 (gray 210))
                                              (filled-rect width 10 (gray 240))
                                              (filled-rect width 4 (gray 160))
                                              (filled-rect width 6 (gray 190))
                                              (filled-rect width 7 (gray 220)))))])
    (lt-superimpose
     (hc-append
      ; Main Tube
      (filled-rectangle 3 36 #:color (gray 30) #:border-width 2)
      (segment 15)
      (segment 50)
      (segment 15)
      (filled-rectangle 5 36 #:color (gray 30) #:border-width 2)

      ; Tapered part of saber;
      ; It shouldn't be this hard to draw a trapezoid.
      (dc (lambda (dc dx dy)
            (define old-brush (send dc get-brush))
            (define old-pen (send dc get-pen))
            (send dc set-brush (new brush% [style 'solid]
                                    [color (gray 210)]))
            (send dc set-pen (new pen% [width 2]))
            (define path (new dc-path%))
            (send path move-to 0 0)
            (send path line-to 38 5)
            (send path line-to 38 30)
            (send path line-to 0 35)
            (send path close)
            (send dc draw-path path dx dy)
            (send dc set-brush old-brush)
            (send dc set-pen old-pen))
          38 36)

      ; Emitter Rings
      (filled-rectangle 4 25 #:color (gray 30) #:border-width 1)
      (filled-rectangle 4 38 #:color (gray 210) #:border-width 1)
      (filled-rectangle 4 25 #:color (gray 30) #:border-width 1)
      (filled-rectangle 4 38 #:color (gray 210) #:border-width 1)
      (filled-rectangle 4 25 #:color (gray 30) #:border-width 1)
      (filled-rectangle 4 38 #:color (gray 210) #:border-width 1)
      (filled-rectangle 4 32 #:color "Peru" #:border-width 1))

     ; Tapered Lines
     (inset (rotate (linewidth 2 (hline 29 4)) -0.0875) 98 7 0 0)
     (inset (linewidth 2 (hline 29 2)) 98 17 0 0)
     (inset (rotate (linewidth 2 (hline 29 4)) 0.0875) 98 23 0 0)

     ; Buttons
     (inset (filled-rectangle 12 5 #:color (gray 235) #:border-width 2) 28 9 0 0)
     (inset (filled-rectangle 12 5 #:color "Red" #:border-width 2) 55 9 0 0))))

(module+ main
 ; Test cases
 (lightsaber "DodgerBlue")
 (lightsaber "Lime" #:length 250)
 (lightsaber "Crimson" #:style 'vader)
 (lightsaber (make-color 144 67 202) #:style 'kylo)
 (lightsaber "DeepPink" #:style 'maul)

 ; Let's make a double-bladed lightsaber!
 (define single (lightsaber "Gold" #:length 200 #:style 'maul))
 (define double (hc-append (flip-horizontal single) single))
 double)

#|
Thoughts
--------
pict was clearly designed for Slideshow and text with decorations;
using it for drawing complex pictures seems a little like an abuse.
Animating these in slideshow/play is awkward
flip-horizontal and flip-vertical should be part of the pict library
Sometimes gaps appear in the combiners
Difficult to make non-rectangular shapes (e.g. a trapezoid) dc is overkill
It would be nice to be able to set the alpha independently of the color (non-functional)
|#