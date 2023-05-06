;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 10 -final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;Exercise 3 (MODE is defined on LINE 161) (Exercise 4 starts on line 930)

(require 2htdp/image)
(require 2htdp/universe)
 
(define BACKGROUND (empty-scene 1000 500 "white")) ; the background for the text buffer
(define FSIZE-TEXT 20)                             ; the text font size
(define COLOR-TEXT "black")                        ; the text font color


; Line is a String
; represnts a string in a list of lines

(define LINE-0 "")
(define LINE-1 "I am Superman")
(define LINE-2 "Boston MA 02115")

(define (line-templ l)
  (... l ...))

; A LoL is one of ’() or (cons Line LoL)
; a list of Line

(define lol-0 '())
(define lol-1 (cons "MA 1600 is my home" '()))
(define lol-2 (cons "MA 1700 rox" lol-1))
(define lol-3 (cons "MA 1800 is where my friend rocky lives" lol-2))
(define lol-4 (cons "MA 1900 south" lol-3))
(define lol-5 (cons "MA 2000" lol-4))

(define (lol-templ lol)
  (... (cond
         [(empty? lol) ...]
         [(cons? lol) (... (first lol)
                           ... (lol-templ (rest lol)) ...)])))

(define-struct buffer [text lnum cnum fontsize mode])
; A Buffer is one of
; - (make-buffer [List-of Line] NonNegativeInt NonNegativeInt PosInt Mode)
; - text is a list of lines
; - lnum represents the line number in which the cursor is
; - cnum represents the column number inn which the cursor is
; - PosInt represnts the current font-size
; - mode represents the three possible modes the editor is in

(define buffer0 (make-buffer (list "") 0 0 1 "MENU"))
(define buffer1 (make-buffer lol-1 0 0 50 "EDIT"))
(define buffer2 (make-buffer lol-3 2 3 12 "SEARCH"))
(define buffer3 (make-buffer lol-4 2 6 10 "EDIT"))
(define buffer4 (make-buffer lol-4 3 6 1 "SEARCH"))
(define buffer5 (make-buffer lol-5 4 7 6 "MENU"))
(define buffer5-start (make-buffer lol-5 0 0 100 "EDIT"))
(define buffer5-end (make-buffer lol-5 4 18 50 "MENU"))

(define (buffer-templ b)
  (... (lol-templ (buffer-text b)) ...
       (buffer-lnum b) ...
       (buffer-cnum b) ...
       (buffer-fontsize b) ...
       (mode-templ (buffer-mode b) ...)))

;A Mode is one of:
;- "EDIT"
;- "MENU"
;- "SEARCH"

;Represents the mode the buffer is in where:
; - "EDIT" indicates the user is editing the buffer text
; - "MENU", indicating that a help menu is being displayed
; - "SEARCH", indicating that the buffer is in search mode 

(define mode1 "EDIT")
(define mode2 "MENU")
(define mode3 "SEARCH")

(define (mode-templ m)
  (cond
    [(string=? m "EDIT") ...]
    [(string=? m "MENU") ...]
    [(string=? m "SEARCH") ...]))


; <=start-of-buffer? : Buffer -> Boolean
; are we at the top-left corner of the buffer or before that?

; I modified this function to check if the buffer is less than or equal to buffer.
; this was to help me in the functions key-backspace, key-enter, key-right, key-left
; because my implementation was making the lnum and cnum index negative when I called list-ref.
; Hence this function helps me in checking if they are become negative so that I could write code
; to keep the index at 0.

(check-expect (<=start-of-buffer? buffer0) #true)
(check-expect (<=start-of-buffer? buffer1) #true)
(check-expect (<=start-of-buffer? buffer5-start) #true)
(check-expect (<=start-of-buffer? buffer4) #false)

(define (<=start-of-buffer? buffer)
  (and
   (<= (buffer-lnum buffer) 0)
   (<= (buffer-cnum buffer) 0)))
 
; >=end-of-buffer? : Buffer -> Boolean
; are we at the lower right corner of the buffer, i.e. last line, last column?

; I modified this function to check if the buffer is greater than or equal to buffer.
; this was to help me in the functions key-backspace, key-enter, key-right, key-left
; because my implementation was making the lnum and cnum index greater than lnum and cnum when I
; called list-ref.
; Hence this function helps me in checking if they are become greater so that I could write code
; to keep the index within the highest limits of lnum and cnum.

(check-expect (>=end-of-buffer? buffer0) #true)
(check-expect (>=end-of-buffer? buffer1) #false)
(check-expect (>=end-of-buffer? buffer5-end) #true)

(define (>=end-of-buffer? buffer)
  (and
   (>= (buffer-lnum buffer) (- (length (buffer-text buffer)) 1))
   (>= (buffer-cnum buffer) (- (string-length (first (reverse (buffer-text buffer)))) 1))))
 
; end-of-line : Buffer NonNegInt[0, (sub1 (length (buffer-text buffer)))] -> NonNegInt
; the end-of-line position of the given line in the buffer

(check-expect (end-of-line buffer5 4) 18)
(check-expect (end-of-line buffer5 2) 38)

(define (end-of-line buffer lnum) 
  (string-length (list-ref (buffer-text buffer) lnum)))




; editor : Text-> Text
; creates the text editor

(define (editor initial-text)
  (local [(define initial-buffer
            (make-buffer (text->lol initial-text) 0 0 FSIZE-TEXT "MENU"))]
    (lol->text  (buffer-text (big-bang initial-buffer
                               [name "A Simple Text Editor"]
                               [to-draw draw-buffer]
                               [on-key process-key])))))

; draw-buffer : Buffer -> Image
; draws the intial state of the buffer and the new state once a new event takes place

(check-expect (draw-buffer buffer0)
              MENU-IMAGE)
              
              

(check-expect (draw-buffer buffer2)
              (place-image/align
               (above/align "left"
                            (text/font "MA 1800 is where my friend rocky lives" 12 COLOR-TEXT
                                       "Monospace" "default" "normal" "normal" #f)
                            (text/font "MA 1700 rox" 12 COLOR-TEXT
                                       "Monospace" "default" "normal" "normal" #f)
                            (beside
                             (text/font "MA " 12 COLOR-TEXT
                                        "Monospace" "default" "normal" "normal" #f)
                             (text/font "1" 12 COLOR-TEXT
                                        "Monospace" "default" "normal" "normal" #t)
                             (text/font "600 is my home" 12 COLOR-TEXT
                                        "Monospace" "default" "normal" "normal" #f)))
               0 0 "left" "top" BACKGROUND))
              

(define (draw-buffer b)
  (cond
    [(string=? "MENU" (buffer-mode b)) MENU-IMAGE]
    [else 
     (stack-images-left-aligned
      (lol->loi (buffer-text b) (buffer-lnum b) (buffer-cnum b) (buffer-fontsize b)))]))

; lol->loi : [List-of Line] NonNegativeInt NonNegativeInt -> [List-of Image]
; converts a list of Line to a list of Image and underlines the character in the lnum-th line
; on the cnum-th character of that line. If lnum and cnum are out of range, it does not underline
; any image. 

(check-expect (lol->loi '() 1 2 20) '())
(check-expect (lol->loi lol-3 2 1 20)
              (list  (text/font "MA 1800 is where my friend rocky lives" 20 COLOR-TEXT
                                "Monospace" "default" "normal" "normal" #f)
                     (text/font "MA 1700 rox" FSIZE-TEXT COLOR-TEXT
                                "Monospace" "default" "normal" "normal" #f)
                     (beside
                      (text/font "M" FSIZE-TEXT COLOR-TEXT
                                 "Monospace" "default" "normal" "normal" #f)
                      (text/font "A" FSIZE-TEXT COLOR-TEXT
                                 "Monospace" "default" "normal" "normal" #t)
                      (text/font " 1600 is my home" FSIZE-TEXT COLOR-TEXT
                                 "Monospace" "default" "normal" "normal" #f))))
(check-expect (lol->loi lol-3 4 2 11)
              (list  (text/font "MA 1800 is where my friend rocky lives" 11 COLOR-TEXT
                                "Monospace" "default" "normal" "normal" #f)
                     (text/font "MA 1700 rox" 11 COLOR-TEXT
                                "Monospace" "default" "normal" "normal" #f)
                     (text/font "MA 1600 is my home" 11 COLOR-TEXT
                                "Monospace" "default" "normal" "normal" #f)))

(define (lol->loi lol lnum cnum fsize)
  (cond
    [(empty? lol) '()]
    [(cons? lol) (if (= lnum 0)
                     (cons (line->u-image (first lol) cnum fsize)
                           (lol->loi (rest lol) (- lnum 1) cnum fsize))
                     (cons (line->image (first lol) fsize)
                           (lol->loi (rest lol) (- lnum 1) cnum fsize)))]))

;Create function to draw text at new size
; line->image : Line Nat -> Image
; converts a line to an image with the given font size 

(check-expect (line->image "" 22)
              (text/font "" 22 COLOR-TEXT "Monospace" "default" "normal"
                         "normal" #f))
(check-expect (line->image "Kush " 1)
              (text/font "Kush " 1 COLOR-TEXT "Monospace" "default"
                         "normal" "normal" #f))
(define (line->image line fsize)
  (text/font line fsize COLOR-TEXT "Monospace" "default" "normal" "normal" #f))

; line->u-image : Line NonNegInt Nat -> Image
; Converts a line to an image of text, of a given size, with one underlined character on index n.
; Places the underline after the text if n is out of range

(check-expect (line->u-image "" 1 1)
              (text/font " " 1 COLOR-TEXT "Monospace" "default"
                         "normal" "normal" #t))
(check-expect (line->u-image "Kush" 1 22)
              (beside
               (text/font "K" 22 COLOR-TEXT
                          "Monospace" "default" "normal" "normal" #f)
               (text/font "u" 22 COLOR-TEXT
                          "Monospace" "default" "normal" "normal" #t)
               (text/font "sh" 22 COLOR-TEXT
                          "Monospace" "default" "normal" "normal" #f)))
(check-expect (line->u-image "Kush" 8 15)
              (beside
               (text/font "Kush" 15 COLOR-TEXT
                          "Monospace" "default" "normal" "normal" #f)
               (text/font " " 15 COLOR-TEXT
                          "Monospace" "default" "normal" "normal" #t)))

(define (line->u-image line n fsize)
  (if (>= n (string-length line))
      (beside (line->image line fsize)
              (text/font " " fsize COLOR-TEXT
                         "Monospace" "default" "normal" "normal" #t))
      (beside
       (text/font (substring line 0 n) fsize COLOR-TEXT
                  "Monospace" "default" "normal" "normal" #f)
       (text/font (substring line n (+ n 1)) fsize COLOR-TEXT
                  "Monospace" "default" "normal" "normal" #t)
       (text/font (substring line (+ n 1)) fsize COLOR-TEXT
                  "Monospace" "default" "normal" "normal" #f))))


; stack-images : [List-of Images] -> Image
; stacks each image in the list below another

(check-expect (stack-images-left-aligned '())
              BACKGROUND)

(check-expect (stack-images-left-aligned (lol->loi lol-3 2 3 12))
              (place-image/align
               (above/align "left"
                            (text/font "MA 1800 is where my friend rocky lives" 12 COLOR-TEXT
                                       "Monospace" "default" "normal" "normal" #f)
                            (text/font "MA 1700 rox" 12 COLOR-TEXT
                                       "Monospace" "default" "normal" "normal" #f)
                            (beside
                             (text/font "MA " 12 COLOR-TEXT
                                        "Monospace" "default" "normal" "normal" #f)
                             (text/font "1" 12 COLOR-TEXT
                                        "Monospace" "default" "normal" "normal" #t)
                             (text/font "600 is my home" 12 COLOR-TEXT
                                        "Monospace" "default" "normal" "normal" #f)))
               0 0 "left" "top" BACKGROUND))

(define (stack-images-left-aligned loi)
  (place-image/align (foldr (lambda (x y) (above/align "left" x y))
                            (text/font "" FSIZE-TEXT COLOR-TEXT "Monospace" "default"
                                       "normal" "normal" #f)
                            loi)
                     0
                     0
                     "left"
                     "top"
                     BACKGROUND))





;the list below contains all characters that should not be printed on the editor
(define NONCHARS (list  "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12"
                        "F13" "F14" "F15" "F16" "F17" "F18" "F19" "F20" "F21" "F22" "F23" "F24"
                        "wheel-up" "wheel-down" "wheel-left" "wheel-right" "shift" "tab"
                        "control" "rshift" "rcontrol" "start" "cancel" "clear" "menu" "pause"
                        "prior" "end" "next" "home" "escape" "select" "print" "execute" "snapshot"
                        "insert" "help" "numlock" "scroll" "press" "release" "\u007F"))


; process-key : KeyEvent Buffer -> Buffer
; takes in a key and changes the current state of the buffer to a new state

(check-expect (process-key buffer2 "a")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA a1600 is my home") 2 4 12 "SEARCH"))
(check-expect (process-key buffer2 "shift")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 2 3 12 "SEARCH"))
(check-expect (process-key buffer2 "up")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 1 3 12 "SEARCH"))
(check-expect (process-key buffer2 "down")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 2 18 12 "SEARCH"))
(check-expect (process-key buffer2 "right")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 2 4 12 "SEARCH"))
(check-expect (process-key buffer2 "left")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 2 2 12 "SEARCH"))
(check-expect (process-key buffer2 "\b")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA1600 is my home") 2 2 12 "SEARCH"))
(check-expect (process-key buffer2 "\r")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA " "1600 is my home") 3 0 12 "SEARCH"))
(check-expect (process-key buffer2 "home")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 2 0 12 "SEARCH"))
(check-expect (process-key buffer2 "end")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 2 18 12 "SEARCH"))
(check-expect (process-key buffer2 "f1")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 2 3 13 "SEARCH"))
(check-expect (process-key buffer2 "f2")
              (make-buffer (list "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 2 3 11 "SEARCH"))

(define (process-key b ke)
  (if (not (string=? "MENU" (buffer-mode b)))
      (local [; column : X -> NonNegativeInt
              ; a acts a place holder. returns the column number of the buffer
              ;(check-expect (column 3) 7) when b = buffer5
              (define (column a) (buffer-cnum b))

              ; (row 1) : X -> NonNegativeInt
              ; a acts a place holder. returns the line number of the buffer
              ;(check-expect ((row 1) 8) 4) when b = buffer5
              (define (row a) (buffer-lnum b))

              ; lol : X -> [List-of Line]
              ; a acts a place holder. returns the text of the buffer
              ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
              (define (lol a) (buffer-text b))]
        (cond
          [(string=? "up" ke) (key-up b)]
          [(string=? "down" ke) (key-down b)]
          [(string=? "right" ke) (key-right b)]
          [(string=? "left" ke) (key-left b)]
          [(string=? "\b" ke) (key-backspace b)]
          [(string=? "\r" ke) (key-enter b)]
          [(string=? "home" ke) (key-home b)]
          [(string=? "end" ke) (key-end b)]
          [(string=? "f1" ke) (increase-fontsize b)]
          [(string=? "f2" ke) (decrease-fontsize b)]
          [(string=? "escape" ke) (key-escape b)]
          [else (if (item-in-list? ke NONCHARS)
                    (make-buffer (lol 1) (row 1) (column 1) (buffer-fontsize b) (buffer-mode b))
                    (make-buffer (subst
                                  (char-insert ke (column 1) (list-ref (lol 1) (row 1))) (row 1)
                                  (lol 1)) 
                                 (row 1)
                                 (add1 (column 1)) (buffer-fontsize b) (buffer-mode b)))]))
      (if (and (string=? "MENU" (buffer-mode b)) (not (string=? ke "escape")))
          b
          (key-escape b))))

; item-in-list? : String [List-of String] -> Boolean
; checks if the String is in [List-of String]

(check-expect (item-in-list? "yo" '()) #false)
(check-expect (item-in-list? "up" NONCHARS) #false)
(check-expect (item-in-list? "shift" NONCHARS) #true)

(define (item-in-list? s los)
  (ormap
   (lambda (val)
     (string=? s val)) los))

; key-up : Buffer -> Buffer
; changes the position of the cursor by moving it up a line
; but there's no change in the cursor position when it's at
; the top of the buffer. If the cursor is in the top line, the cursor is shifted to the start
; of the buffer 

(check-expect (key-up buffer0) (make-buffer (list "") 0 0 1 "MENU"))
(check-expect (key-up buffer5) (make-buffer (buffer-text buffer5) 3 7 6 "MENU"))
(check-expect (key-up (make-buffer (buffer-text buffer5) 2 37 6 "MENU"))
              (make-buffer (buffer-text buffer5) 1 13 6 "MENU"))
(check-expect (key-up (make-buffer (buffer-text buffer5) 0 2 6 "MENU"))
              (make-buffer (buffer-text buffer5) 0 0 6 "MENU"))
(check-expect (key-up buffer5-start)
              (make-buffer (buffer-text buffer5) 0 0 100 "EDIT"))
(check-expect (key-up (make-buffer (buffer-text buffer5) 2 37 6 "MENU"))
              (make-buffer (buffer-text buffer5) 1 13 6 "MENU"))


(define (key-up b)
  (if (= (buffer-lnum b) 0)
      (make-buffer (buffer-text b) 0 0 (buffer-fontsize b) (buffer-mode b))
      (local [; (column 1): X -> NonNegativeInt
              ; a acts a place holder. returns the column number of the buffer
              ;(check-expect (column 3) 7) when b = buffer5
              (define (column a) (buffer-cnum b))

              ; (row 1) : X -> NonNegativeInt
              ; a acts a place holder. returns the line number of the buffer
              ;(check-expect ((row 1) 8) 4) when b = buffer5
              (define (row a) (buffer-lnum b))

              ; lol : X -> [List-of Line]
              ; a acts a place holder. returns the text of the buffer
              ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
              (define (lol a) (buffer-text b))

              (define prev-line (list-ref (lol 1) (sub1 (row 1))))]
        (if (>= (column 1) (string-length prev-line))
            (make-buffer (lol 1) (sub1 (row 1)) (string-length prev-line) (buffer-fontsize b)
                         (buffer-mode b))
            (make-buffer (lol 1) (sub1 (row 1)) (column 1) (buffer-fontsize b) (buffer-mode b))))))

; key-down : Buffer -> Buffer
; changes the position of the cursor by moving it up a line
; but there's no change in the cursor position when it's at
; the bottom of the buffer. If the cursor is in the last line, it shifts it to the end of
; the line 

(check-expect (key-down buffer0) (make-buffer (list "") 0 0 1 "MENU"))
(check-expect (key-down buffer5) (make-buffer (buffer-text buffer5) 4 18 6 "MENU"))
(check-expect (key-down (make-buffer (buffer-text buffer5) 0 2 6 "MENU"))
              (make-buffer (buffer-text buffer5) 1 2 6 "MENU"))
(check-expect (key-down (make-buffer (buffer-text buffer5) 0 0 6 "MENU"))
              (make-buffer (buffer-text buffer5) 1 0 6 "MENU"))
(check-expect (key-down (make-buffer (buffer-text buffer5) 2 37 6 "MENU"))
              (make-buffer (buffer-text buffer5) 3 11 6 "MENU"))
(check-expect (key-down buffer5-end)
              (make-buffer (buffer-text buffer5) 4 18 50 "MENU"))

(define (key-down b)
  (if (= (buffer-lnum b) (- (length (buffer-text b)) 1))
      (make-buffer (buffer-text b) (- (length (buffer-text b)) 1)
                   (string-length (list-ref (buffer-text b) (- (length (buffer-text b)) 1)))
                   (buffer-fontsize b) (buffer-mode b))
      (local [; column : X -> NonNegativeInt
              ; a acts a place holder. returns the column number of the buffer
              ;(check-expect (column 3) 7) when b = buffer5
              (define (column a) (buffer-cnum b))

              ; (row 1) : X -> NonNegativeInt
              ; a acts a place holder. returns the line number of the buffer
              ;(check-expect ((row 1) 8) 4) when b = buffer5
              (define (row a) (buffer-lnum b))

              ; lol : X -> [List-of Line]
              ; a acts a place holder. returns the text of the buffer
              ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
              (define (lol a) (buffer-text b))
              (define next-line (list-ref (lol 1) (add1 (row 1))))]
        (if (>= (column 1) (string-length next-line))
            (make-buffer (lol 1) (add1 (row 1)) (string-length next-line) (buffer-fontsize b)
                         (buffer-mode b))
            (make-buffer (lol 1) (add1 (row 1)) (column 1) (buffer-fontsize b) (buffer-mode b))))))
      

; key-right : Buffer -> Buffer
; changes the position of the cursor by moving a unit to the 
; right and goes on the next line only if its at the end of a line
; but there's no change in the cursor position when it's at
; the end of the buffer

(check-expect (key-right buffer0) (make-buffer (list "") 0 0 1 "MENU"))
(check-expect (key-right buffer5) (make-buffer (buffer-text buffer5) 4 8 6 "MENU"))
(check-expect (key-right (make-buffer (buffer-text buffer5) 2 38 6 "MENU"))
              (make-buffer (buffer-text buffer5) 3 0 6 "MENU"))
(check-expect (key-right (make-buffer (buffer-text buffer5) 0 2 6 "MENU"))
              (make-buffer (buffer-text buffer5) 0 3 6 "MENU"))
(check-expect (key-right buffer5-end)
              (make-buffer (buffer-text buffer5) 4 18 50 "MENU"))

(define (key-right b)
  (local [; column : X -> NonNegativeInt
          ; a acts a place holder. returns the column number of the buffer
          ;(check-expect (column 3) 7) when b = buffer5
          (define (column a) (buffer-cnum b))

          ; (row 1) : X -> NonNegativeInt
          ; a acts a place holder. returns the line number of the buffer
          ;(check-expect ((row 1) 8) 4) when b = buffer5
          (define (row a) (buffer-lnum b))

          ; lol : X -> [List-of Line]
          ; a acts a place holder. returns the text of the buffer
          ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
          (define (lol a) (buffer-text b))
          (define current-line (list-ref (lol 1) (row 1))) 
          (define last-possible-lnum (sub1 (length (lol 1))))
          (define last-line (list-ref (lol 1) (sub1 (length (lol 1)))))]
    (cond
      [(>=end-of-buffer? b)
       (make-buffer (lol 1) last-possible-lnum (string-length last-line)
                    (buffer-fontsize b) (buffer-mode b))]
      [(and (= (column 1) (string-length current-line)) (< (row 1) last-possible-lnum))
       (make-buffer (lol 1) (add1 (row 1)) 0 (buffer-fontsize b) (buffer-mode b))]
      [else (make-buffer (lol 1) (row 1) (add1 (column 1)) (buffer-fontsize b) (buffer-mode b))])))
  


; key-left : Buffer -> Buffer
; changes the position of the cursor by moving a unit to the 
; left and goes on the previous line only if its at the start of a line
; but there's no change in the cursor position when it's at
; the start of the buffer

(check-expect (key-left buffer0) (make-buffer (list "") 0 0 1 "MENU"))
(check-expect (key-left buffer5) (make-buffer (buffer-text buffer5) 4 6 6 "MENU"))
(check-expect (key-left (make-buffer (buffer-text buffer5) 3 0 6 "MENU"))
              (make-buffer (buffer-text buffer5) 2 38 6 "MENU"))
(check-expect (key-left buffer5-start) buffer5-start)

(define (key-left b)
  (local [; column : X -> NonNegativeInt
          ; a acts a place holder. returns the column number of the buffer
          ;(check-expect (column 3) 7) when b = buffer5
          (define (column a) (buffer-cnum b))

          ; (row 1) : X -> NonNegativeInt
          ; a acts a place holder. returns the line number of the buffer
          ;(check-expect ((row 1) 8) 4) when b = buffer5
          (define (row a) (buffer-lnum b))

          ; lol : X -> [List-of Line]
          ; a acts a place holder. returns the text of the buffer
          ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
          (define (lol a) (buffer-text b))
          (define current-line (list-ref (lol 1) (row 1)))
          (define prev-line (if (> (row 1) 0)
                                (list-ref (lol 1) (sub1 (row 1)))
                                (list-ref (lol 1) (row 1))))
          (define last-line (list-ref (lol 1) (sub1 (length (lol 1)))))]
    (cond
      [(<=start-of-buffer? b)
       (make-buffer (lol 1) 0 0 (buffer-fontsize b) (buffer-mode b))]
      [(and (= (column 1) 0) (> (row 1) 0))
       (make-buffer (lol 1) (sub1 (row 1)) (string-length prev-line)
                    (buffer-fontsize b) (buffer-mode b))]
      [else (make-buffer (lol 1) (row 1) (sub1 (column 1)) (buffer-fontsize b) (buffer-mode b))])))


; key-backspace : Buffer -> Buffer
; deletes the character to the left of the cursor if the cursor not at the start of the line
; if the cursor is at the start of the line, its apppends that line with the previous line

(check-expect (key-backspace buffer0)
              (make-buffer (list "") 0 0 1 "MENU"))
(check-expect (key-backspace buffer5-start)
              (make-buffer (buffer-text buffer5-start) 0 0 100 "EDIT"))
(check-expect (key-backspace (make-buffer (buffer-text buffer5) 3 0 6 "MENU"))
              (make-buffer (list "MA 2000" "MA 1900 south"
                                 "MA 1800 is where my friend rocky livesMA 1700 rox"
                                 "MA 1600 is my home")
                           2 38 6 "MENU"))
(check-expect (key-backspace (make-buffer (buffer-text buffer5) 2 14 6 "MENU"))
              (make-buffer (list "MA 2000" "MA 1900 south" "MA 1800 is whre my friend rocky lives"
                                 "MA 1700 rox" "MA 1600 is my home") 2 13 6 "MENU"))


(define (key-backspace b)
  (if (= (buffer-cnum b) 0)
      (local [; column : X -> NonNegativeInt
              ; a acts a place holder. returns the column number of the buffer
              ;(check-expect (column 3) 7) when b = buffer5
              (define (column a) (buffer-cnum b))

              ; (row 1) : X -> NonNegativeInt
              ; a acts a place holder. returns the line number of the buffer
              ;(check-expect ((row 1) 8) 4) when b = buffer5
              (define (row a) (buffer-lnum b))

              ; lol : X -> [List-of Line]
              ; a acts a place holder. returns the text of the buffer
              ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
              (define (lol a) (buffer-text b))
              (define current-line (list-ref (lol 1) (row 1)))
              (define prev-line
                (if (= (row 1) 0) (list-ref (lol 1) (row 1)) (list-ref (lol 1) (sub1 (row 1)))))]
        (if (<= (row 1) 0)
            (make-buffer (buffer-text b) 0 0 (buffer-fontsize b) (buffer-mode b))
            (make-buffer (subst-for-two (string-append prev-line current-line) (sub1 (row 1)) (lol 1))
                         (sub1 (row 1)) (string-length prev-line)
                         (buffer-fontsize b) (buffer-mode b))))
      (make-buffer
       (subst (char-delete (sub1 (buffer-cnum b)) (list-ref (buffer-text b) (buffer-lnum b)))
              (buffer-lnum b) (buffer-text b))
       (buffer-lnum b)
       (sub1 (buffer-cnum b))
       (buffer-fontsize b) (buffer-mode b))))

; key-enter : Buffer -> Buffer
; creates a new empty line below the line when the cursor is at the end of a line.
; if the cursor is at the start of a line, makes a new line above the cursor.
; if its in the middle of the line then the cursor makes a new line
; with the rest of the string to the right of the cursor

(check-expect (key-enter buffer0)
              (make-buffer (list "" "") 1 0 1 "MENU"))
(check-expect (key-enter buffer5-start)
              (make-buffer (list "" "MA 2000" "MA 1900 south"
                                 "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home") 1 0 100 "EDIT"))
(check-expect (key-enter buffer5-end)
              (make-buffer (list "MA 2000" "MA 1900 south"
                                 "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600 is my home" "") 5 0 50 "MENU"))
(check-expect (key-enter buffer5)
              (make-buffer (list "MA 2000" "MA 1900 south"
                                 "MA 1800 is where my friend rocky lives" "MA 1700 rox"
                                 "MA 1600" " is my home") 5 0 6 "MENU"))
              

(define (key-enter b)
  (local [; column : X -> NonNegativeInt
          ; a acts a place holder. returns the column number of the buffer
          ;(check-expect (column 3) 7) when b = buffer5
          (define (column a) (buffer-cnum b))

          ; (row 1) : X -> NonNegativeInt
          ; a acts a place holder. returns the line number of the buffer
          ;(check-expect ((row 1) 8) 4) when b = buffer5
          (define (row a) (buffer-lnum b))

          ; lol : X -> [List-of Line]
          ; a acts a place holder. returns the text of the buffer
          ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
          (define (lol a) (buffer-text b))
          (define current-line (list-ref (lol 1) (row 1)))]
    (cond
      [(= (column 1) 0)
       (make-buffer (subst-two "" current-line (row 1) (lol 1)) (add1 (row 1)) 0
                    (buffer-fontsize b) (buffer-mode b))]
      [(= (column 1) (end-of-line b (row 1)))
       (make-buffer (subst-two current-line "" (row 1) (lol 1)) (add1 (row 1)) 0
                    (buffer-fontsize b) (buffer-mode b))]
      [else
       (make-buffer
        (subst-two (substring current-line 0 (column 1))
                   (substring current-line (column 1)) (row 1) (lol 1))
        (add1 (row 1))
        0
        (buffer-fontsize b) (buffer-mode b))])))

; key-home : Buffer -> Buffer
; puts the cursor to the start of the line
; and then puts the cursor to the start of
; the buffer if its at the start of the line

(check-expect (key-home buffer0)
              (make-buffer (list "") 0 0 1 "MENU"))
(check-expect (key-home buffer5-start)
              (make-buffer (buffer-text buffer5) 0 0 100 "EDIT"))
(check-expect (key-home buffer5)
              (make-buffer (buffer-text buffer5) 4 0 6 "MENU"))
(check-expect (key-home (make-buffer (buffer-text buffer5) 4 0 100 "EDIT"))
              (make-buffer (buffer-text buffer5) 0 0 100 "EDIT"))

(define (key-home b)
  (local [; column : X -> NonNegativeInt
          ; a acts a place holder. returns the column number of the buffer
          ;(check-expect (column 3) 7) when b = buffer5
          (define (column a) (buffer-cnum b))

          ; (row 1) : X -> NonNegativeInt
          ; a acts a place holder. returns the line number of the buffer
          ;(check-expect ((row 1) 8) 4) when b = buffer5
          (define (row a) (buffer-lnum b))

          ; lol : X -> [List-of Line]
          ; a acts a place holder. returns the text of the buffer
          ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
          (define (lol a) (buffer-text b))
          (define current-line (list-ref (lol 1) (row 1)))]
    (cond
      [(or (<=start-of-buffer? b) 
           (and (= (column 1) 0) (>= (row 1) 0))) (make-buffer (lol 1) 0 0 (buffer-fontsize b)
                                                               (buffer-mode b))]
      [else (make-buffer (lol 1) (row 1) 0 (buffer-fontsize b) (buffer-mode b))])))


; key-end : Buffer -> Buffer
; puts the cursor to the end of the line.
; puts the cursor to the end of the buffer if its at the end of the line

(check-expect (key-end buffer0)
              (make-buffer (list "") 0 0 1 "MENU"))
(check-expect (key-end buffer5-end)
              (make-buffer (buffer-text buffer5) 4 18 50 "MENU"))
(check-expect (key-end buffer4)
              (make-buffer (buffer-text buffer4) 3 18 1 "SEARCH"))
(check-expect (key-end (make-buffer (buffer-text buffer5) 2 38 10 "SEARCH"))
              (make-buffer (buffer-text buffer5) 4 18 10 "SEARCH"))
             
(define (key-end b)
  (local [; column : X -> NonNegativeInt
          ; a acts a place holder. returns the column number of the buffer
          ;(check-expect (column 3) 7) when b = buffer5
          (define (column a) (buffer-cnum b))

          ; (row 1) : X -> NonNegativeInt
          ; a acts a place holder. returns the line number of the buffer
          ;(check-expect ((row 1) 8) 4) when b = buffer5
          (define (row a) (buffer-lnum b))

          ; lol : X -> [List-of Line]
          ; a acts a place holder. returns the text of the buffer
          ;(check-expect (lol 8) (list "MA 1600 is my home")) when b = buffer1
          (define (lol a) (buffer-text b))
          (define current-line (list-ref (lol 1) (row 1)))
          (define index-last-line (sub1 (length (lol 1))))
          (define last-line (list-ref (lol 1) (sub1 (length (lol 1)))))
          (define length-last-line (string-length last-line))]
    (cond
      [(or (>=end-of-buffer? b) 
           (and (= (column 1) (string-length current-line)) (<= (row 1) index-last-line)))
       (make-buffer (lol 1) index-last-line (string-length last-line) (buffer-fontsize b)
                    (buffer-mode b))]
      [else (make-buffer (lol 1) (row 1) (string-length current-line) (buffer-fontsize b)
                         (buffer-mode b))])))


; subst : [X] X NonNegInt[0, (length lox)] [List-of X] -> [List-of X]
; substitute new for the element at pos in lox
; (error if pos does not exist)

(check-expect (subst "Kush" 2 lol-4) (list "MA 1900 south" "MA 1800 is where my friend rocky lives"
                                           "Kush" "MA 1600 is my home"))

(define (subst new pos lox)
  (cond
    [(empty? lox) '()]
    [(cons? lox) (if (= pos 0)
                     (cons new (subst new (sub1 pos) (rest lox)))
                     (cons (first lox) (subst new (sub1 pos) (rest lox))))]))

  
 
; subst-two : [X] X X NonNegInt[0, (length lox)] [List-of X] -> [List-of X]
; substitute the *two* elements new1 and new2 for the element at pos
; (error if pos does not exist in lox)

(check-expect (subst-two "Kush" "Kush Loves Chocolates" 2 lol-4)
              (list
               "MA 1900 south"
               "MA 1800 is where my friend rocky lives"
               "Kush"
               "Kush Loves Chocolates"
               "MA 1600 is my home"))

(define (subst-two new1 new2 pos lox)
  (cond
    [(empty? lox) '()]
    [(cons? lox) (if (= pos 0)
                     (cons new1 (cons new2 (subst-two new1 new2 (sub1 pos) (rest lox))))
                     (cons (first lox) (subst-two new1 new2 (sub1 pos) (rest lox))))]))
  
 
; subst-for-two : [X] X NoNegInt[0, (length lox)] [List-of X]-> [List-of X]
; substitute new for the *two* elements as positions pos and pos+1


(check-expect (subst-for-two "partner" 2 lol-5)
              (list "MA 2000" "MA 1900 south" "partner" "MA 1600 is my home"))

(define (subst-for-two new pos lox)
  (cond
    [(empty? lox) '()]
    [(cons? lox)
     (if (= pos 0)
         (cons new (rest (subst-for-two new (- pos 2) (rest lox))))
         (cons (first lox) (subst-for-two new (- pos 1) (rest lox))))]))


; char-delete : NonNegInt String -> String
; deletes the character at the given position or returns "invalid position" if the position does
; not exist.

(check-expect (char-delete 2 "hello") "helo")
(check-expect (char-delete 2 "kush") "kuh")
(check-expect (char-delete 5 "kush") "invalid position")
(check-expect (char-delete 0 "kush") "ush")
(check-expect (char-delete 0 "k") "")


(define (char-delete pos s)
  (if (> pos (string-length s))
      "invalid position"
      (string-append (substring s 0 pos)
                     (substring s (+ pos 1)))))


; char-insert : 1String NonNegInt[0, (string-length s)] String -> String
; insert character c into s at position pos, or at end if pos = (string-length s)

(check-expect (char-insert "u" 0 "") "u")
(check-expect (char-insert "u" 1 "ksh") "kush")
(check-expect (char-insert "r" 4 "kush") "kushr")

(define (char-insert c pos s)
  (if (= pos (string-length s))
      (string-append s c)
      (string-append (substring s 0 pos) c (substring s pos))))




; Exercise 4

;increase-fontsize: Buffer -> Buffer
;Increases the font size by one
(check-expect (increase-fontsize buffer0) (make-buffer (list "") 0 0 2 "MENU"))
(check-expect (increase-fontsize buffer1) (make-buffer (buffer-text buffer1) 0 0 51 "EDIT"))
(check-expect (increase-fontsize buffer5) (make-buffer (buffer-text buffer5) 4 7 7 "MENU"))

(define (increase-fontsize b)
  (make-buffer (buffer-text b) (buffer-lnum b) (buffer-cnum b) (add1 (buffer-fontsize b))
               (buffer-mode b)))

 

;decrease-fontsize: Buffer -> Buffer
;decreases the font size by one
(check-expect (decrease-fontsize buffer0) (make-buffer (buffer-text buffer0) 0 0 1 "MENU"))
(check-expect (decrease-fontsize buffer1) (make-buffer (buffer-text buffer1) 0 0 49 "EDIT"))
(check-expect (decrease-fontsize buffer5) (make-buffer (buffer-text buffer5) 4 7 5 "MENU"))
(check-expect (decrease-fontsize buffer3) (make-buffer (buffer-text buffer3) 2 6 9 "EDIT"))

(define (decrease-fontsize b)
  (if (= (buffer-fontsize b) 1)
      b
      (make-buffer (buffer-text b) (buffer-lnum b) (buffer-cnum b) (sub1 (buffer-fontsize b))
                   (buffer-mode b))))

; Exercise 5

;key-escape: Buffer -> Buffer
;When in menu mode, goes to edit mode and vice versa.
(check-expect (key-escape buffer0) (make-buffer (list "") 0 0 1 "EDIT"))
(check-expect (key-escape buffer1) (make-buffer lol-1 0 0 50 "MENU"))
(check-expect (key-escape buffer2) buffer2)
(define (key-escape b)
  (cond
    [(string=? (buffer-mode b) "MENU") (make-buffer (buffer-text b) (buffer-lnum b) (buffer-cnum b)
                                                    (buffer-fontsize b) "EDIT")]
    [(string=? (buffer-mode b) "EDIT") (make-buffer (buffer-text b) (buffer-lnum b) (buffer-cnum b)
                                                    (buffer-fontsize b) "MENU")]
    [(string=? (buffer-mode b) "SEARCH") b]))



(define TEXT "Welcome to our editor
We have 3 different modes: MENU, EDIT and, SEARCH
To switchto EDIT mode, press 'esc'
To switch to back to MENU, press 'esc'
Press F1 to increase the font size
Press F2 to decrease the font size")

(define MENU-FONTSIZE 30)
(define MENUTEXT-COLOR "red")

(define MENU-IMAGE (place-image (text TEXT MENU-FONTSIZE MENUTEXT-COLOR) 500 250 BACKGROUND))

; Exercise 6

(define LINE-SEP "\n")

;A Text is a String
;Represents the entire text contained in a text buffer where lines are spearated by the new line
;symbol

(define text-empty LINE-SEP)
(define text-0  (string-append
                 "About a hiring visit by Edsger Dijkstra (1930-2002)," LINE-SEP
                 "a famous computer scientist:"                         LINE-SEP
                 LINE-SEP))
(define text-1  (string-append
                 "This is our project." LINE-SEP  "We Worked very hard" LINE-SEP))
(define text-2 (string-append
                "1." LINE-SEP  "2." LINE-SEP "3." LINE-SEP))
(define (text-templ t)
  (... t ...))

; Exercise 7

; split : String 1-String -> [List-of String]
; creates a [list-of String] without the seperators

(check-expect (split "AB|B|C|D"   "|") (list "AB" "B" "C" "D"))
(check-expect (split "|A|B|C|D"  "|") (list "" "A" "B" "C" "D"))
(check-expect (split "|A|B|C|D|" "|") (list "" "A" "B" "C" "D" ""))
(check-expect (split "A"         "|") (list "A"))
(check-expect (split ""          "|") (list ""))
(check-expect (split "|"         "|") (list "" ""))
(check-expect (split "||"        "|") (list "" "" ""))

 
(define (split str c)
  ; helper : String [List-of String]  -> [List-of Webpage]
  (local [(define (helper x los)
            (cond
              [(and (string=? c x) (empty? los)) (list "" "")]
              [(empty? los) (list x)]
              [(string=? x c) (cons "" los)]
              [else (cons (string-append x (first los)) (rest los))]))]
    (foldr helper (list "") (explode str))))


; Exercise 8

; text->lol : Text -> [NEList-of Line]
; converts a Text to [NEList-of Line]

(check-expect (text->lol text-0) (list "About a hiring visit by Edsger Dijkstra (1930-2002),"
                                       "a famous computer scientist:" "" ""))
(check-expect (text->lol "") (list  ""))
(check-expect (text->lol text-1) (list  "This is our project."
                                        "We Worked very hard" ""))



(define (text->lol text)
  (split text LINE-SEP))


; Exercise 9

; lol->text : [NEList-of Line] -> Text
; converts a [NEList-of Line] to Text
(check-expect (lol->text (list "1." "2." "3.")) "1.\n2.\n3.\n")
(check-expect (lol->text (list "This is our project." "We worked hard"))
              "This is our project.\nWe worked hard\n")
(check-expect (lol->text (list "")) "\n")

(define (lol->text nelos)
  (foldr (lambda (x y) (string-append x LINE-SEP y)) "" nelos))












