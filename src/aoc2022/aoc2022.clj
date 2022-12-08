(ns aoc2022.aoc2022
  (:require [clojure.string :as str :refer [split]])
  (:require [swiss.arrows :refer [-<>>]]))

(def d1
  (let
   [totals-sorted
    (-<>> (slurp "input1")
          (split <> #"\n")
          (map parse-long)
          (partition-by nil?)
          (filter (comp (complement nil?) first))
          (map #(reduce + %))
          (sort >))]
    {:p1 (first totals-sorted)
     :p2 (reduce + (take 3 totals-sorted))}))

(def d2
  (let
   [moves-guess
    {"A" :rock "X" :rock
     "B" :paper "Y" :paper
     "C" :scissors "Z" :scissors}

    moves-elf
    {"A" :rock "X" :lose
     "B" :paper "Y" :draw
     "C" :scissors "Z" :win}

    input
    (->> (slurp "input2")
         str/split-lines
         (map #(str/split % #" ")))

    rounds
    (map #(map moves-guess %) input)

    outcomes
    (map #(map moves-elf %) input)

    points
    {:rock 1
     :paper 2
     :scissors 3
     :lose 0
     :draw 3
     :win 6}

    play
    {'(:rock :paper) :win
     '(:rock :rock) :draw
     '(:rock :scissors) :lose
     '(:paper :paper) :draw
     '(:paper :rock) :lose
     '(:paper :scissors) :win
     '(:scissors :paper) :lose
     '(:scissors :rock) :win
     '(:scissors :scissors) :draw}

    sign-for
    {'(:rock :lose) :scissors
     '(:rock :draw) :rock
     '(:rock :win) :paper
     '(:paper :lose) :rock
     '(:paper :draw) :paper
     '(:paper :win) :scissors
     '(:scissors :lose) :paper
     '(:scissors :draw) :scissors
     '(:scissors :win) :rock}

    calc (fn [strategy guide]
            (reduce + (concat
                       (map (comp points second) guide)
                       (map (comp points strategy) guide))))]

    {:p1 (calc play rounds)
     :p2 (calc sign-for outcomes)}))

(comment
  d2
  (do (def d3
        2)
      d3)
  :rcf)
