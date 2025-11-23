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

(defvar searchgen--basic-fill-chars
  "abcdefghijklmnopqrstuvwxyz")

(defvar searchgen--probability-fill-chars
  "aaaaaaaabcccddddeeeeeeeeeeeeffgghhhhhhiiiiiiijkllllmmnnnnnnoooooooppqrrrrrrsssssstttttttttuuuvwwxyyz")

;; Inspired by https://gist.github.com/purcell/34824f1b676e6188540cdf71c7cc9fc4
(defun searchgen--seq-shuffle (seq)
  (seq-sort-by (lambda (_) (random)) #'<= seq))

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

(defun searchgen--sort-by-length (words)
  "Given a list of strings, WORDS, return a sorted list longest to shortest."
  (sort words :key 'length :reverse t))
(cl-assert (equal '("goo" "fo" "bo" "f" "") (searchgen--sort-by-length '("" "f" "fo" "bo" "goo"))))

(defun searchgen--right-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (x1 (length word))
      (push (list (+ x x1) y) the-list))
    (reverse the-list)
    ))

(defun searchgen--left-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (x1 (length word))
      (push (list (- x x1) y) the-list))
    (reverse the-list)
    ))

(defun searchgen--down-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list x (+ y y1)) the-list))
    (reverse the-list)
    ))

(defun searchgen--up-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list x (- y y1)) the-list))
    (reverse the-list)
    ))

(defun searchgen--upleft-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list (- x y1) (- y y1)) the-list))
    (reverse the-list)
    ))

(defun searchgen--upright-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list (+ x y1) (- y y1)) the-list))
    (reverse the-list)
    ))

(defun searchgen--downleft-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list (- x y1) (+ y y1)) the-list))
    (reverse the-list)
    ))

(defun searchgen--downright-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list (+ x y1) (+ y y1)) the-list))
    (reverse the-list)
    ))

(defun searchgen--board-positions (board-size)
  (let ((the-list nil))
    (cl-loop
     for y in (number-sequence 0 (1- board-size))
     do (cl-loop
  	   for x in (number-sequence 0 (1- board-size))
  	   do (push (list x y) the-list)))
    the-list))

(defvar searchgen--basic-direction-functions
  (list 'searchgen--right-coordinates 'searchgen--down-coordinates))

(defvar searchgen--intermediate-direction-functions
  (list 'searchgen--right-coordinates
        'searchgen--down-coordinates
        'searchgen--downright-coordinates))

(defvar searchgen--all-direction-functions
  (list 'searchgen--left-coordinates 'searchgen--right-coordinates
        'searchgen--down-coordinates 'searchgen--up-coordinates
        'searchgen--upleft-coordinates 'searchgen--upright-coordinates
        'searchgen--downleft-coordinates 'searchgen--downright-coordinates))

(defun searchgen--valid-at (char x y board board-size)
  "Return true iff character may be placed within board at given position."
  (and
   ;; position allows
   ;;   0 >= x < board-size
   (>= x 0)
   (< x board-size)
   ;;   0 >= y < board-size
   (>= y 0)
   (< y board-size)
   ;; value allows
   (or
    ;; empty cell
    (eq 'empty (nth (+ x (* y board-size)) board))
    ;; matches character in word
    (string= (char-to-string char) (nth (+ x (* y board-size)) board)))))


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

(defun searchgen--as-latex (board board-size words)
  "Return a string containing LaTeX source (hopefully) of a single-page
PDF containing the word search puzzle"
  (with-temp-buffer

    (insert "\\documentclass{article}\n")
    (insert "\\begin{document}\n")

    (insert "\\begin{center}\n")
    (insert "\\begin{tabular}{|")
    (dotimes (n board-size)
      (insert " c"))
    (insert " |}\n")

    (insert "\\hline\n")
    (dotimes (n (* board-size board-size))
      (insert (nth (+ (% n board-size) (* board-size (/ n board-size))) board))
      (if (= (1- board-size) (% n board-size))
          (insert " \\\\\n")
        (insert " & ")))
    (insert "\\hline\n")

    (insert "\\end{tabular}\n")
    (insert "\\end{center}\n")

    (insert "\\begin{center}\n")
    (insert "\\begin{tabular}{ccccc}\n")
    ;; padding := (align - (value % align)) % align
    ;; aligned-value := value + padding
    (dotimes (_ (% (- 5 (% (length words) 5)) 5))
      (setf words (append words (list ""))))
    (seq-map-indexed
     (lambda (word index)
       (insert word)
       (if (= (1- 5) (% index 5))
           (insert " \\\\\n")
         (insert " & ")))
     words)


    (insert "\\end{tabular}\n")
    (insert "\\end{center}\n")


    (insert "\\end{document}\n")
    (buffer-string)))

(defun searchgen--as-plaintext (board board-size words)
  "Return a string of plain text source containing the given word search puzzle."
  (with-temp-buffer

    (dotimes (n (* board-size board-size))
      (insert (nth (+ (% n board-size) (* board-size (/ n board-size))) board))
      (if (= (1- board-size) (% n board-size))
          (insert "\n")
        (insert " ")))
    (insert "\n")

    (seq-map-indexed
     (lambda (word index)
       (insert word)
       (if (or (= index (1- (length words))) (= (1- 5) (% index 5)))
           (insert "\n")
         (insert "  ")))
     words)
    (buffer-string)))

(defun searchgen-make (words board-size-offset fill-character-set direction-functions &optional &key seed)
  "'WORDS'   A list of strings which will be placed in the word search, to
be found.
'BOARD-SIZE-OFFSET'   A positive value that will be added to the length
of the longest word, resulting in the amount of characters in each row
of the word search board.
'FILL-CHARACTER-SET'   A string, each character of which may be used to
fill an empty position after all words are placed.
':seed'   A string that is passed to 'random', to make it so the same
board may be generated across multiple invocations."

  (cl-assert (integerp board-size-offset) t)
  (cl-assert (>= board-size-offset 0) t)

  (cl-assert (seqp fill-character-set) t)

  (when (stringp seed)
    (random seed))

  ;; TODO: The idea would be to randomly select which direction to place
  ;; the word in, and randomize the order in which we iterate positions.

  (let*
      ((board-size (+ (searchgen--longest words) board-size-offset))
       (board (make-list (* board-size board-size) 'empty))
       ;; Attempt to place each word in the board.
       (words-placed
        ;; For every word (sorted longest to shortest)...
        (mapcar
         (lambda (word)
           ;; For every position (shuffled)
           (let ((word-placed nil))
             (cl-loop until word-placed
                      for position in (searchgen--seq-shuffle (searchgen--board-positions board-size))
                      do (let
                             ((x (nth 0 position))
                              (y (nth 1 position)))
                           ;; For every direction (shuffled)
                           (mapc
                            (lambda (direction-function)
                              (unless
                                  word-placed
                                (let
                                    ((validity-of-positions
                                      (seq-mapn
                                       (lambda (position char)
                                         (let
                                             ((x (nth 0 position))
                                              (y (nth 1 position)))
                                           (searchgen--valid-at char x y board board-size)))
                                       (funcall direction-function word x y)
                                       word)))
                                  ;; (message "Word valid to be placed in RIGHT direction at (%s,%s): %s" x y (unless (memq nil validity-of-positions) t))
                                  ;; When word /is/ actually valid, perform placement (alter 'board')
                                  (unless (memq nil validity-of-positions)
                                    (seq-mapn
                                     (lambda (position char)
                                       (let ((x1 (nth 0 position))
                                             (y1 (nth 1 position)))
                                         (setf (nth (+ x1 (* y1 board-size)) board) (char-to-string char))
                                         ;; (message "Placed character %c at coordinates (%s,%s)" char x1 y1)
                                         ;; (print (searchgen--print-board board board-size))
                                         ))
                                     (funcall direction-function word x y)
                                     word)
                                    ;; (message "Placed WORD %s at coordinates (%s,%s)" word x y)
                                    (setf word-placed t)
                                    t)))
                              )
                            (searchgen--seq-shuffle direction-functions))
                           ))
             word-placed))
         (searchgen--sort-by-length words))))

    (when (memq nil words-placed)
      (error "TODO: Couldn't place all words... Try again (possibly increasing board size)"))

    ;; With every word placed, fill in the empty positions with characters
    ;; from the fill character set (at random, for now).
    ;; TODO: I think it may be interesting to pull portions of the input words
    ;; and try to place those in the empty positions, to have red herrings.
    (setf board
          (mapcar
           (lambda (cell)
             (if (eq 'empty cell)
                 (char-to-string
                  (seq-random-elt fill-character-set))
               cell))
           board))

    ;; Return board
    board))

(defun searchgen--to-string (board board-size)
  "Returns simple string representation of given board."
  (searchgen--print-board board board-size))

(defun searchgen--to-latex-file (path board board-size words)
  "Writes the LaTeX representation of the given puzzle board to a
file at 'PATH'.
'PATH'   Passed to 'find-file-noselect' to get a buffer.
See 'searchgen--as-plaintext'."
  (with-current-buffer (find-file-noselect path)
    (delete-region (point-min) (point-max))
    (insert (searchgen--as-latex board board-size words))
    (save-buffer)))

(defun searchgen--to-plaintext-file (path board board-size words)
  "Writes the plaintext representation of the given puzzle board to a
file at 'PATH'.
'PATH'   Passed to 'find-file-noselect' to get a buffer.
See 'searchgen--as-plaintext'."
  (with-current-buffer (find-file-noselect path)
    (delete-region (point-min) (point-max))
    (insert (searchgen--as-plaintext board board-size words))
    (save-buffer)))

(defun searchgen--to-all (board board-size words)
  (searchgen--to-plaintext-file "board.txt" board board-size words)
  (searchgen--to-latex-file "board.tex" board board-size words)
  (searchgen--to-string board board-size))

(defun searchgen-basic (words &optional &key seed)
  "Limits word direction to RIGHT, and DOWN."
  (let* ((board-size-offset 1)
         (board (searchgen-make
                 words board-size-offset
                 searchgen--basic-fill-chars
                 searchgen--basic-direction-functions
                 :seed seed))
         (board-size (+ (searchgen--longest words) board-size-offset)))
    (searchgen--to-all board board-size words)))

(defun searchgen-intermediate (words &optional &key seed)
  "Limits word direction to RIGHT, DOWN, and DOWN-RIGHT.
Fill characters roughly weighted based on frequency of appearance in general text."
  (let* ((board-size-offset 1)
         (board (searchgen-make
                 words board-size-offset
                 searchgen--probability-fill-chars
                 searchgen--intermediate-direction-functions
                 :seed seed))
         (board-size (+ (searchgen--longest words) board-size-offset)))
    (searchgen--to-all board board-size words)))

(defun searchgen-advanced (words &optional &key seed)
  (let* ((board-size-offset 1)
         (board (searchgen-make
                 words board-size-offset
                 searchgen--probability-fill-chars
                 searchgen--all-direction-functions
                 :seed seed))
         (board-size (+ (searchgen--longest words) board-size-offset)))
    (searchgen--to-all board board-size words)))

;; (searchgen-string '("foo" "boo" "zoo" "moo" "coo") 1 ".")
;; (searchgen-all '("cat" "dog" "bat" "rat" "bear" "lion") 1 searchgen--basic-fill-chars searchgen--all-direction-functions)

(provide 'searchgen)

;;; searchgen.el ends here
