(ns aoc2022.aoc2022
  (:require clojure.inspector
            [clojure.set :as set]
            [clojure.string :as str :refer [split replace split-lines]])
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
         split-lines
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

(def d3
  (let
   [rucksacks-split
    (->>
     (slurp "input3")
     (split-lines)
     (map #(partition (/ (count %) 2) %)))

    priority
    (fn [c]
      (let [i (int c)]
        (if (>= i (int \a))
          (- i 96)
          (- i 38))))]

    {:p1
     (->>
      rucksacks-split
      (map (fn [[left right]]
             (map priority (set/intersection (set left) (set right)))))
      flatten
      (reduce +))
     :p2
     (->>
      (slurp "input3")
      (split-lines)
      (partition 3)
      (map (fn [group]
             (->> group
                  (map set)
                  (apply set/intersection)
                  (map priority))))
      flatten
      (reduce +))}))

(def d4
  (let
   [sets-contained?
    (fn [set1 set2]
      (or (clojure.set/subset? set1 set2)
          (clojure.set/superset? set1 set2)))

    line-ranges
    (->>
     (slurp "input4")
     split-lines
     (map
      (fn [line]
        (->> (split line #",")
             (map
              (fn [pairstr]
                (let [[start end]
                      (map parse-long (split pairstr #"-"))]
                  (range start (inc end)))))))))

    calc
    (fn [pred]
      (->> line-ranges
           (map (fn [[r1 r2]] (pred (set r1) (set r2))))
           (filter true?)
           count))]

    {:p1 (calc sets-contained?)
     :p2 (calc #(> (count (clojure.set/intersection %1 %2)) 0))}))

(defn transpose [m]
  (apply mapv vector m))

(defn n-times [n f]
  (apply comp (repeat n f)))

(def d5
  (let
   [[sstacks smoves]
    (->> (slurp "input5")
         split-lines
         (split-with (complement str/blank?)))
    sstacks (drop-last sstacks)
    smoves (drop 1 smoves)

    stacks (->> sstacks
                (map (fn [s] (-> (replace s #"    " "[0]")
                                 (replace #" " "")
                                 (replace #"\[([A-Z0])\]" "$1"))))
                transpose
                (map (fn [crates]
                       (into [] (reverse (filter #(not= % \0) crates)))))
                (into []))

    moves (map (fn [moves]
                 (let [[cnt src dst]
                       (as-> moves $
                         (replace $ #"[a-z]" "")
                         (str/trim $)
                         (split $ #"  ")
                         (map parse-long $))]
                   [cnt (dec src) (dec dst)])) smoves)

    calc (fn [preserve-order?]
           (->> moves
                (reduce
                 (fn [acc [cnt src dst]]
                   (-> acc
                       (update dst #(into % ((if preserve-order? identity reverse) (take-last cnt (nth acc src)))))
                       (update src (n-times cnt pop)))) stacks)
                (map peek)
                (apply str)))]

    {:p1 (calc false)
     :p2 (calc true)}))

(def d6
  (let
   [calc
    (fn [n]
      (->> (slurp "input6")
           (partition-all n 1)
           (keep-indexed (fn [i v] (when (= n (count (set v))) i)))
           first
           (+ n)))]
    {:p1 (calc 4)
     :p2 (calc 14)}))

(comment
  (do
    (def d6)
    d6)
  :rcf)
