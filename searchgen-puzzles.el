(require 'searchgen)

;; Just a bunch of lists of words and pre-made calls so that you can just
;; evaluate an expression and get started on your puzzle.

(defvar searchgen-words--animals
  '("bat" "bear" "bee" "cat"
    "cow" "deer" "dog" "dove"
    "duck" "frog" "goose" "horse"
    "kiwi" "lion" "pig" "rat"
    "robin" "whale"))
(searchgen-basic searchgen-words--animals)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_basic_animals" "./board.tex")

(searchgen-intermediate searchgen-words--animals)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_intermediate_animals" "./board.tex")

(searchgen-advanced searchgen-words--animals :size 5)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_advanced_animals" "./board.tex")

(defvar searchgen-words--metals
  '("copper" "iron" "silver" "gold"
    "steel" "nickel"))
(searchgen-basic searchgen-words--metals)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_basic_metals" "./board.tex")

(searchgen-intermediate searchgen-words--metals)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_intermediate_metals" "./board.tex")

(searchgen-advanced searchgen-words--metals :size 4)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_advanced_metals" "./board.tex")

(defvar searchgen-words--chemistry
  '("ion" "electron" "proton" "neutron"
    "hydrogen" "oxidize" "reduce" "reaction"))
(searchgen-basic searchgen-words--chemistry)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_basic_chemistry" "./board.tex")

(searchgen-intermediate searchgen-words--chemistry)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_intermediate_chemistry" "./board.tex")

(searchgen-advanced searchgen-words--chemistry)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_advanced_chemistry" "./board.tex")

(defvar searchgen-words--space
  '("Mercury" "Venus" "Earth" "Mars"
    "Jupiter" "Saturn" "Uranus" "Neptune"
    "Pluto" "comet" "planet" "moon"
    "eclipse" "galaxy" "rocket" "telescope"))
(searchgen-basic searchgen-words--space)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_basic_space" "./board.tex")

(searchgen-intermediate searchgen-words--space)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_intermediate_space" "./board.tex")

(searchgen-advanced searchgen-words--space)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_advanced_space" "./board.tex")

(defvar searchgen-words--office-equipment
  '("tape" "pen" "ruler" "paper"
    "stapler" "eraser" "pencil" "scissors"))
(searchgen-basic searchgen-words--office-equipment)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_basic_officeequipment" "./board.tex")
(searchgen-intermediate searchgen-words--office-equipment)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_intermediate_officeequipment" "./board.tex")
(searchgen-advanced searchgen-words--office-equipment :size 7)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_advanced_officeequipment" "./board.tex")

(defvar searchgen-words--groups
  '("crew" "team" "throng" "herd"
    "flock" "swarm" "pack" "pride"
    "set" "bunch" "batch" "bundle"))
(searchgen-basic searchgen-words--groups)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_basic_groups" "./board.tex")
(searchgen-intermediate searchgen-words--groups)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_intermediate_groups" "./board.tex")
(searchgen-advanced searchgen-words--groups)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_advanced_groups" "./board.tex")

(defvar searchgen-words--titles-for-people
  '("captain" "pilot" "commander" "admiral"
    "officer" "chief" "leader"))
(searchgen-basic searchgen-words--titles-for-people)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_basic_titlesforpeople" "./board.tex")
(searchgen-intermediate searchgen-words--titles-for-people)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_intermediate_titlesforpeople" "./board.tex")
(searchgen-advanced searchgen-words--titles-for-people :size 8)
(call-process "xelatex" nil nil nil "-jobname=wordsearch_advanced_titlesforpeople" "./board.tex")

(call-process "pdfunite" nil nil nil "wordsearch_basic*.pdf" "wordsearch_basic.pdf")
(call-process "pdfunite" nil nil nil "wordsearch_intermediate*.pdf" "wordsearch_intermediate.pdf")
(call-process "pdfunite" nil nil nil "wordsearch_advanced*.pdf" "wordsearch_advanced.pdf")
