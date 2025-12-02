(require 'searchgen)

(defun make-pdf (words theme-string size difficulty-level)
  "'theme-string' is a string describing the list of words.
'difficulty-level' is one of 'basic', 'intermediate', or 'advanced'."
  (cond
   ((eq 'basic difficulty-level)
    (searchgen-basic words :min-size size :max-size size))
   ((eq 'intermediate difficulty-level)
    (searchgen-intermediate words :min-size size :max-size size))
   ((eq 'advanced difficulty-level)
    (searchgen-advanced words :min-size size :max-size size))
   (t (error "Invalid value for 'difficulty-level'")))
  (call-process "xelatex" nil nil nil (format "-jobname=%s_%s" theme-string difficulty-level) "./board.tex"))

(defmacro make-pdf--all (words theme-string size)
  `(progn
     (make-pdf ,words ,theme-string ,size 'basic)
     (make-pdf ,words ,theme-string ,size 'intermediate)
     (make-pdf ,words ,theme-string ,size 'advanced)))

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
      (mapcar
       (lambda (pathlist) (nth 0 pathlist))
       paths)
      (mapcar
       (lambda (pathlist) (nth 1 pathlist))
       paths)
      (mapcar
       (lambda (pathlist) (nth 2 pathlist))
       paths)))))

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

;; "The ten most common words in the English language."
(make-pdf--all
 '("the" "be" "to" "of" "and" "a" "in" "that" "i" "it")
 'common
 5)

(make-pdf--all
 '("nineeleven" "moonlanding" "areafiftyone" "flatearth" "paulisdead"
   "princessdiana" "jfktwoshooters" "aliensinroswell" "birdsarentreal")
 'conspiracies
 15)

(make-pdf--all
 '("boobs" "butts" "dicks" "pussies" "holes"
   "cocks" "tits" "thicc" "coochie" "cooter")
 'crass_bodyparts
 10)

(make-pdf--all
 '("weed" "ecstasy" "cocaine" "nicotine" "alcohol"
   "booze" "percocet" "valium" "vicodin" "caffeine")
 'drugs
 10)

(make-pdf--all
 '("bonny" "love" "wifey" "boo" "rideordie"
   "cutie" "adorable" "marriage" "pretty" "beautiful")
 'cutesie
 10)

(make-pdf--all
 '("avenue" "boulevard" "culdesac" "drive" "freeway"
   "highway" "place" "road" "street" "way")
 'roadways
 10)

(unite-pdfs '(common conspiracies crass_bodyparts drugs cutesie roadways) "searches.pdf")
