;;; searchgen.el --- Generate a word search -*- lexical-binding: t -*-

;; Author: Lens_r
;; Maintainer: Lens_r
;; Version: 0.0.1
;; Package-Requires: ((emacs))
;; Homepage: homepage
;; Keywords: game, play, word search


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(eval-when-compile (require 'cl-lib))

(defun searchgen--longest (words)
  "Given a list of strings, WORDS, return the longest length."
  (let ((n 0))
    (mapc
     (lambda (word)
       (when (> (length word) n) (setf n (length word))))
     words)
    n))

(cl-assert (= 0 (searchgen--longest '())))
(cl-assert (= 4 (searchgen--longest '("f" "foo" "b" "baar" "as"))))

(defun searchgen--print-board (board board-size)
  (cl-assert (listp board))
  (cl-assert (= (* board-size board-size) (length board)))
  (cl-assert (integerp board-size))
  (cl-assert (>= board-size 0))

  (let ((index 0))
    (mapconcat
     (lambda (char)
       (format "%s%s"
               char
               (if (= 0 (% (cl-incf index) board-size)) "\n" " ")))
     board))
  )

(defun searchgen-make (words board-size-offset)
  "'WORDS'   A list of strings which will be placed in the word search, to
be found.
'BOARD-SIZE-OFFSET'   A positive value that will be added to the length
of the longest word, resulting in the amount of characters in each row
of the word search board."

  (cl-assert (integerp board-size-offset) t)
  (cl-assert (>= board-size-offset 0) t)

  ;; TODO: The idea would be to randomly select which direction to place
  ;; the word in, and randomize the order in which we iterate positions.

  (let*
      ((board-size (+ (searchgen--longest words) board-size-offset))
       (board (make-list (* board-size board-size) 'empty)))

    (message "BOARD SIZE %s" board-size)

    ;; Attempt to place each word in the board.
    ;; For every word...
    (mapc
     (lambda (word)
       (message "WORD %s" word)
       ;; For every position
       (let ((word-placed nil))
         (cl-loop until word-placed
                  for y from 0 to board-size
                  do (progn
                       (cl-loop
                        for x from 0 to board-size
                        do (progn
                             (message "POSITION (%s,%s)" x y)
                             ;; For every direction

                             (message "DIRECTION RIGHT")
                             (when
                                 ;; Attempt to place word with direction at position
                                 ;; RIGHT
                                 ;; loop through x, (+ x (length word)), ensuring valid positions. If any
                                 ;; position would be invalid, we move on from trying this direction.
                                 (let ((validity-of-positions
                                        (cl-loop for x1 from x to (1- (+ x (length word)))
                                                 collect (progn
                                                           (message "Checking coordinates (%s,%s)"
                                                                    x1 y)
                                                           (message "Value at (%s,%s): %s"
                                                                    x1 y (nth (+ x1 (* y board-size)) board))
                                                           (message "Character in word that would go here: %c"
                                                                    (elt word (- x1 x)))
                                                           (message "Character is valid when placed here: %s"
                                                                    (or (eq 'empty (nth (+ x1 (* y board-size)) board))
                                                                        (eq (elt word (- x1 x)) (nth (+ x1 (* y board-size)) board))))
                                                           ;; If the coordinates are outside the board, it's an invalid position.
                                                           ;; If the cell is empty or if the cell matches the character we would be
                                                           ;; placing there anyway, it's a valid position.
                                                           (and
                                                            (< x1 board-size)
                                                            (< y board-size)
                                                            (or (eq 'empty (nth (+ x1 (* y board-size)) board))
                                                                (eq (elt word (- x1 x)) (nth (+ x1 (* y board-size)) board))))
                                                           ))))
                                   (message "Word valid to be placed in RIGHT direction at (%s,%s): %s"
                                            x y (unless (memq nil validity-of-positions) t))
                                   ;; When word /is/ actually valid, perform placement (alter 'board')
                                   (unless (memq nil validity-of-positions)
                                     (cl-loop for x1 from x to (1- (+ x (length word)))
                                              do (progn
                                                   (setf (nth (+ x1 (* y board-size)) board) (string (elt word (- x1 x))))
                                                   (message "Placed character %c at coordinates (%s,%s)" (elt word (- x1 x)) x1 y)
                                                   (print (searchgen--print-board board board-size))
                                                   ))
                                     t))
                               (setf word-placed t))
                             ))))))
     words)


    ;; Return board as string
    (searchgen--print-board board board-size)

    ))

(searchgen-make '("foo" "bar" "baz") 1)

(provide 'searchgen)

;;; searchgen.el ends here
