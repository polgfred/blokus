(ns blokus.rules
  (:use [clojure.set]))

(def +size+ 20)

(defmacro coord
  [x y]
  `(vector-of :int ~x ~y))

(defmacro coords
  [& pairs]
  `(eval (into #{} (map #(conj % 'coord) '~pairs))))

(def +pieces+ [
    ;;  1
    (coords (0 0))

    ;;  2
    (coords (0 0) (0 1))

    ;;  3
    (coords (0 0) (0 1) (0 2))
    (coords (0 0) (0 1) (1 1))

    ;;  4
    (coords (0 0) (0 1) (0 2) (0 3))
    (coords (0 2) (1 0) (1 1) (1 2))
    (coords (0 0) (0 1) (0 2) (1 1))
    (coords (0 0) (0 1) (1 0) (1 1))
    (coords (0 0) (1 0) (1 1) (2 1))

    ;; 5
    (coords (0 0) (0 1) (0 2) (0 3) (0 4))
    (coords (0 3) (1 0) (1 1) (1 2) (1 3))
    (coords (0 2) (0 3) (1 0) (1 1) (1 2))
    (coords (0 1) (0 2) (1 0) (1 1) (1 2))
    (coords (0 0) (0 2) (1 0) (1 1) (1 2))
    (coords (0 0) (0 1) (0 2) (0 3) (1 1))
    (coords (0 2) (1 0) (1 1) (1 2) (2 2))
    (coords (0 0) (0 1) (0 2) (1 2) (2 2))
    (coords (0 0) (1 0) (1 1) (2 1) (2 2))
    (coords (0 0) (0 1) (1 1) (2 1) (2 2))
    (coords (0 0) (0 1) (1 1) (2 1) (1 2))
    (coords (0 1) (1 0) (1 1) (1 2) (2 1))])

(defn flip
  [p]
  (let [mx (reduce max (map first p))]
    (into #{} (map (fn [[x y]] (coord (- mx x) y)) p))))

(def flip (memoize flip))

(defn rotate
  [p]
  (let [my (reduce max (map second p))]
    (into #{} (map (fn [[x y]] (coord (- my y) x)) p))))

(def rotate (memoize rotate))

(defn orient
  [p]
  (union
    (into #{} (take 4 (iterate rotate p)))
    (into #{} (take 4 (iterate rotate (flip p))))))

(def orient (memoize orient))

(defn coords-from
  [deltas [x y]]
  (into #{} (map (fn [[dx dy]] (coord (+ x dx) (+ y dy))) deltas)))

(defn borders
  [p]
  (let [deltas (coords (-1 0) (0 -1) (0 1) (1 0))
        coords (map (partial coords-from deltas) p)
        all    (reduce union coords)]
    (difference all p)))

(def borders (memoize borders))

(defn corners
  [p]
  (let [deltas (coords (-1 -1) (-1 1) (1 -1) (1 1))
        coords (map (partial coords-from deltas) p)
        all    (reduce union coords)]
    (difference all p (borders p))))

(def corners (memoize corners))

; (let [p '((coord 0 0) (coord 1 0) (coord 2 0) (coord 0 1))]
;   (dorun
;     (for [p (orient p)]
;       (dump-p! p))))

(def +board+ (vec (repeat +size+ (vec (repeat +size+ 0)))))

(def home [nil (coord 0 0) (coord 19 0) (coord 19 19) (coord 0 19)])

(defn sq-get
  ([b x y]
    (get (get b y) x))
  ([b x dx y dy]
    (sq-get b (+ x dx) (+ y dy))))

(defn sq-put
  [b x y s]
  (assoc b y (assoc (get b y) x s)))

(defn valid
  [b s p tx ty]
  (and (every?
         (fn [[dx dy]]
           (let [x (+ tx dx) y (+ ty dy)]
             (and (=    0 (sq-get b x y))
                  (not= s (sq-get b x -1 y  0))
                  (not= s (sq-get b x  1 y  0))
                  (not= s (sq-get b x  0 y -1))
                  (not= s (sq-get b x  0 y  1)))))
         p)
       (some
         (fn [[dx dy]]
           (let [x (+ tx dx) y (+ ty dy)]
             (or (= (home s) [x y])
                 (= s (sq-get b x -1 y -1))
                 (= s (sq-get b x -1 y  1))
                 (= s (sq-get b x  1 y -1))
                 (= s (sq-get b x  1 y  1)))))
         p)))

(defn collect
  [b s p]
  (distinct
    (remove nil?
      (for [q  (orient p)
            ty (range (- +size+ (reduce max (map second q))))
            tx (range (- +size+ (reduce max (map first q))))]
        (if (valid b s q tx ty)
          [q tx ty])))))

(defn collect-all
  [b s ps]
  (reduce concat (map (partial collect b s) ps)))

; (let [b (sq-put (sq-put +board+ 0 0 1) 1 0 1)
;       p '((coord 0 0) (coord 1 0) (coord 2 0) (coord 1 1))]
;   (do
;     (dump-b! b)
;     (dorun
;       (for [[p x y] (collect-all b 1 +pieces+)]
;         (println p x y)))))

(defn play
  [b p s tx ty]
  (reduce (fn [b [dx dy]] (sq-put b (+ tx dx) (+ ty dy) s)) b p))

; (let [p '((coord 0 0) (coord 1 0) (coord 2 0) (coord 1 1))]
;   (do
;     (dump-b! (play +board+ p 1 5 5))
;     (dump-b! (play +board+ (rotate p) 3 5 5))))

(defn dump-p!
  [p]
  (let [bp (borders p)
        cp (corners p)
        rx (range -1 (+ 2 (reduce max (map first p))))
        ry (range -1 (+ 2 (reduce max (map second p))))]
    (letfn [(repr
              [x y]
              (let [c (coord x y)]
                (cond
                  (contains? p  c) "\u001B[7m*\u001B[0m"
                  (contains? bp c) "\u001B[31m-\u001B[0m"
                  (contains? cp c) "\u001B[32m@\u001B[0m"
                  :else            " ")))
            (row
              [y]
              (apply str (map #(repr % y) rx)))]
      (dorun
        (for [y ry]
          (println (str "  [" (row y) "]"))))
        (println))))

(defn dump-b!
  [b]
  (let [chr-s { 0 "." 1 "b" 2 "g" 3 "r" 4 "y" }]
    (dorun
      (for [y (range +size+)]
        (println
          (str "  [" (reduce str (map chr-s (get b y))) "]"))))
    (println)))
