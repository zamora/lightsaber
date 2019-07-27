#lang racket

; lightsaber.rkt
;
; Draw lightsabers using Racket's pict library
; Created for Stephen De Gabrielle's Summer 2019 standard fish competition
;
; Modification History
; -------------------------------------------------
; 07/27/2019   Justin Zamora   Initial creation

(provide lightsaber)

(require racket/contract)
(require racket/draw)
(require pict)

; Contract for the allowed hilt styles
(define hilt-style? (flat-contract (one-of/c 'luke 'vader 'obi-wan 'anakin)))

; Generate a shade of gray
(define (gray level)
  (make-color level level level))

; Helper macro for filled-rectangle
(define-syntax filled-rect
  (syntax-rules ()
    [(filled-rect w h c) (filled-rectangle w h #:color c #:draw-border? #false)]))

; Create a pict of a lightsaber.  Accept a color object or a color name.
; An optional length can be specified (useful for animating blade extension)
(define/contract (lightsaber color [length 400] [hilt-style 'luke])
  (->* ((or/c string? (is-a?/c color%))) (nonnegative-integer? hilt-style?) pict?)
  (define color-obj (cond
                      [(string? color) (send the-color-database find-color color)]
                      [else color]))

  ; Generate the hilt
  (define the-hilt (hilt hilt-style))
  ; Generate the blade
  (define the-blade (blade color-obj length))

  ; Combine the hilt and the blade
  (if (zero? length)
      the-hilt
      (lc-superimpose (inset the-blade (- (pict-width the-hilt) 25) 0 0 0)
                      the-hilt)))

; Create a pict of the lightsaber blade
(define (blade color length)
  (define r (send color red))
  (define g (send color green))
  (define b (send color blue))

  ; Pict of the white core
  (define core (filled-rounded-rectangle (+ length 10) 10 -0.5
                                         #:color "White"
                                         #:draw-border? #false))

  ; Generate glows with transparency and offset from the blade center
  (define (glow alpha offset)
    (filled-rounded-rectangle (+ length offset) offset -0.5
                              #:color (make-color r g b alpha)
                              #:draw-border? #false))

  ; Stack the glows and the core to create the blade
  (for/fold ([pict (glow 0.1 50)]
             [offset 40]
             #:result (cc-superimpose pict core))
            ([alpha (list 0.2 0.4 0.8)])
    (values (cc-superimpose (glow alpha offset) pict)
            (- offset 10))))

; Draw the hilt, using the given style
; Note: Only the 'luke style is implemented
(define/contract (hilt style)
  (-> hilt-style? pict?)
  (case style
    [(luke) luke-hilt]
    [else (error "Unrecognized style: ~a" style)]))

; Draw Luke Skywalker's lightsaber hilt
(define luke-hilt
  (let ([hilt-background
         (hc-append (inset (rb-superimpose (filled-rounded-rectangle 68 40 5 #:color "Black")
                                           (inset (filled-rounded-rectangle 32 22 5 #:color "Black")
                                                  0 0 5 30))
                           2 0 0 13)
                    (inset (filled-rounded-rectangle 41 30 5 #:color "Black") -5 0 0 0)
                    (hc-append (inset (filled-rectangle 10 30 #:color "Black")
                                      1 0 0 0)
                               (filled-rectangle 2 22 #:color "Black")
                               (filled-rectangle 9 30.5 #:color "Black")
                               (filled-rounded-rectangle 10 45 5 #:color "Black")))]
        [hilt-foreground
         (hc-append (vl-append (filled-rect 2 10 (gray 90))
                               (filled-rect 2 7 (gray 125))
                               (filled-rect 2 10 (gray 90))
                               (filled-rect 2 7 (gray 30)))
                    (vl-append (filled-rect 5 10 (gray 140))
                               (filled-rect 5 7 (gray 185))
                               (filled-rect 5 10 (gray 140))
                               (filled-rect 5 7 (gray 45)))
                    ; Long part of grip
                    (inset (rb-superimpose
                            (vl-append (filled-rect 55 10 (gray 175))
                                       (filled-rect 55 7 (gray 245))
                                       (filled-rect 55 10 (gray 175))
                                       (filled-rect 55 7 (gray 95)))
                            ; Switch
                            (inset (vl-append (filled-rect 25 10 (make-color 165 95 35))
                                              (filled-rect 25 2 (make-color 90 40 5))
                                              (filled-rect 25 7 (gray 128)))
                                   0 0 5 27))
                           0 0 0 12)
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
    (cc-superimpose hilt-background
                    (inset hilt-foreground 2 0 0 2))))

; Test cases
(lightsaber "Crimson")
(lightsaber "DodgerBlue")
(lightsaber "Lime" 250)
(lightsaber (make-color 144 67 202))
