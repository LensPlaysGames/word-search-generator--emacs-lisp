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
(searchgen-intermediate searchgen-words--animals)
(searchgen-advanced searchgen-words--animals :size 5)

(defvar searchgen-words--metals
  '("copper" "iron" "silver" "gold"
    "steel" "nickel"))
(searchgen-basic searchgen-words--metals)
(searchgen-intermediate searchgen-words--metals)
(searchgen-advanced searchgen-words--metals)

(defvar searchgen-words--chemistry
  '("ion" "electron" "proton" "neutron"
    "hydrogen" "oxidize" "reduce" "reaction"))
(searchgen-basic searchgen-words--chemistry)
(searchgen-intermediate searchgen-words--chemistry)
(searchgen-advanced searchgen-words--chemistry)

(defvar searchgen-words--space
  '("Mercury" "Venus" "Earth" "Mars"
    "Jupiter" "Saturn" "Uranus" "Neptune"
    "Pluto" "comet" "planet" "moon"
    "eclipse" "galaxy" "rocket" "telescope"))
(searchgen-basic searchgen-words--space)
(searchgen-intermediate searchgen-words--space)
(searchgen-advanced searchgen-words--space)

(defvar searchgen-words--office-equipment
  '("tape" "pen" "ruler" "paper"
    "stapler" "eraser" "pencil" "scissors"))
(searchgen-basic searchgen-words--office-equipment)
(searchgen-intermediate searchgen-words--office-equipment)
(searchgen-advanced searchgen-words--office-equipment :size 7)

(defvar searchgen-words--groups
  '("crew" "team" "throng" "herd"
    "flock" "swarm" "pack" "pride"
    "set" "bunch" "batch" "bundle"))
(searchgen-basic searchgen-words--groups)
(searchgen-intermediate searchgen-words--groups)
(searchgen-advanced searchgen-words--groups :size 6)

(defvar searchgen-words--titles-for-people
  '("captain" "pilot" "commander" "admiral"
    "officer" "chief" "leader"))
(searchgen-basic searchgen-words--titles-for-people)
(searchgen-intermediate searchgen-words--titles-for-people)
(searchgen-advanced searchgen-words--titles-for-people :size 8)
