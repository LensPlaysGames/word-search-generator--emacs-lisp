(require 'searchgen)

(defvar basic-sources nil)
(defvar intermediate-sources nil)
(defvar advanced-sources nil)

(setf basic-sources nil)
(setf intermediate-sources nil)
(setf advanced-sources nil)

(defun make-pdf (words theme-string size)
  "'theme-string' is a string describing the list of words."
  (push
   (searchgen--as-latex
    (searchgen-make
     words
     searchgen--basic-fill-chars
     searchgen--basic-direction-functions
     :min-size size
     :max-size size)
    size
    words)
   basic-sources)
  (push
   (searchgen--as-latex
    (searchgen-make
     words
     searchgen--probability-fill-chars
     searchgen--intermediate-direction-functions
     :min-size size
     :max-size size)
    size
    words)
   intermediate-sources)
  (push
   (searchgen--as-latex
    (searchgen-make
     words
     searchgen--probability-fill-chars
     searchgen--all-direction-functions
     :min-size size
     :max-size size)
    size
    words)
   advanced-sources)
  t)

(defun unite-pdfs--paths (theme-strings)
  "Return a list of paths to PDFs produced by 'make-pdf'.
'theme-strings' is a list of strings describing the lists of words."
  (let ((paths (mapcar
                (lambda (theme)
                  (list (format "%s_basic.pdf" theme)
                        (format "%s_intermediate.pdf" theme)
                        (format "%s_advanced.pdf" theme)))
                theme-strings)))
    (flatten-list
     (list
      (mapcar (lambda (pathlist) (nth 0 pathlist)) paths)
      (mapcar (lambda (pathlist) (nth 1 pathlist)) paths)
      (mapcar (lambda (pathlist) (nth 2 pathlist)) paths)))))

(defun unite-pdfs--command-args (theme-strings outpath)
  "'theme-strings' should contain all 'theme-string's
 ever passed to 'make-pdf'.
'outpath' is the path the output PDF will be written to."
  (append
   (mapcar
    (lambda (p) (concat p))
    (unite-pdfs--paths theme-strings))
   (list outpath)))

(defun unite-pdfs (theme-strings outpath)
  (eval `(call-process "pdfunite" nil nil nil ,@(unite-pdfs--command-args theme-strings outpath))))
(unite-pdfs '(common conspiracies crass_bodyparts roadways) "searches.pdf")

;; Conspiracy theories.
(make-pdf
 '("nineeleven" "moonlanding" "areafiftyone" "flatearth" "paulisdead"
   "princessdiana" "jfktwoshooters" "aliensinroswell" "birdsarentreal")
 'conspiracies
 15)

;; Naughty!
(make-pdf
 '("boobs" "butts" "dicks" "pussies" "holes"
   "cocks" "tits" "thicc" "coochie" "cooter")
 'crass_bodyparts
 10)

;; Drugs!
(make-pdf
 '("weed" "ecstasy" "cocaine" "nicotine" "alcohol"
   "booze" "percocet" "valium" "vicodin" "caffeine")
 'drugs
 10)

;; Cutesie words, or something.
(make-pdf
 '("bonny" "love" "wifey" "boo" "rideordie"
   "cutie" "adorable" "marriage" "pretty" "beautiful")
 'cutesie
 10)

;; Non-proper names of roadways.
(make-pdf
 '("avenue" "boulevard" "culdesac" "drive" "freeway"
   "highway" "place" "road" "street" "way")
 'roadways
 10)

;; The ten most common words in the English language.
(make-pdf
 '("the" "be" "to" "of" "in" "that" "have" "it" "for" "not")
 'common
 5)

(with-current-buffer (find-file-noselect "./searches.tex")
  (erase-buffer)
  (insert
   (searchgen--as-latex-document-string
    (concat
     (mapconcat 'identity basic-sources)
     (mapconcat 'identity intermediate-sources)
     (mapconcat 'identity advanced-sources))))
  (save-buffer))
(call-process "xelatex" nil nil nil "./searches.tex")
