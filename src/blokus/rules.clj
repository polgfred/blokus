(ns blokus.rules
  (:use [clojure.set]))

(def +size+ 20)

(def +pieces+ '(
    ;;  1
    #{[0 0]}

    ;;  2
    #{[0 0] [0 1]}

    ;;  3
    #{[0 0] [0 1] [0 2]}
    #{[0 0] [0 1] [1 1]}

    ;;  4
    #{[0 0] [0 1] [0 2] [0 3]}
    #{[0 2] [1 0] [1 1] [1 2]}
    #{[0 0] [0 1] [0 2] [1 1]}
    #{[0 0] [0 1] [1 0] [1 1]}
    #{[0 0] [1 0] [1 1] [2 1]}

    ;; 5
    #{[0 0] [0 1] [0 2] [0 3] [0 4]}
    #{[0 3] [1 0] [1 1] [1 2] [1 3]}
    #{[0 2] [0 3] [1 0] [1 1] [1 2]}
    #{[0 1] [0 2] [1 0] [1 1] [1 2]}
    #{[0 0] [0 2] [1 0] [1 1] [1 2]}
    #{[0 0] [0 1] [0 2] [0 3] [1 1]}
    #{[0 2] [1 0] [1 1] [1 2] [2 2]}
    #{[0 0] [0 1] [0 2] [1 2] [2 2]}
    #{[0 0] [1 0] [1 1] [2 1] [2 2]}
    #{[0 0] [0 1] [1 1] [2 1] [2 2]}
    #{[0 0] [0 1] [1 1] [2 1] [1 2]}
    #{[0 1] [1 0] [1 1] [1 2] [2 1]}))

(defn dump-p!
  [p]
  (letfn [(rows
            [p]
            (let [my (reduce max (map second p))]
              (for [j (range (inc my))]
                (filter (fn [[x y]] (= j y)) p))))
          (repr
            [p]
            (let [mx (reduce max (map first p))
                  cs (vec (repeat (inc mx) " "))]
              (for [r (rows p)]
                (reduce (fn [cs [x y]] (assoc cs x "*")) cs r))))]
    (println (str p ":"))
    (println)
    (dorun
      (for [r (repr p)]
        (println (str "  [" (reduce str r) "]"))))
    (println)))

; (dorun
;   (for [p +pieces+]
;     (dump-p! p)))

(defn flip
  [p]
  (let [mx (reduce max (map first p))]
    (into #{} (map (fn [[x y]] [(- mx x) y]) p))))

(def flip (memoize flip))

(defn rotate
  [p]
  (let [my (reduce max (map second p))]
    (into #{} (map (fn [[x y]] [(- my y) x]) p))))

(def rotate (memoize rotate))

(defn orient
  [p]
  (union
    (into #{} (take 4 (iterate rotate p)))
    (into #{} (take 4 (iterate rotate (flip p))))))

(def orient (memoize orient))

(defn coords-from
  [deltas [x y]]
  (into #{} (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) deltas)))

(defn borders
  [p]
  (let [deltas #{[-1 0] [0 -1] [0 1] [1 0]}
        all (reduce union (map (partial coords-from deltas) p))]
    (difference all p)))

(def borders (memoize borders))

(defn corners
  [p]
  (let [deltas #{[-1 -1] [-1 1] [1 -1] [1 1]}
        all (reduce union (map (partial coords-from deltas) p))]
    (difference all p (borders p))))

(def corners (memoize corners))

; (let [p '([0 0] [1 0] [2 0] [0 1])]
;   (dorun
;     (for [p (orient p)]
;       (dump-p! p))))

(def +board+ (vec (repeat +size+ (vec (repeat +size+ 0)))))

(defn dump-b!
  [b]
  (let [chr-s {0 "." 1 "b" 2 "g" 3 "r" 4 "y"}]
    (dorun
      (for [y (range +size+)]
        (println
          (str "[" (reduce str (map chr-s (get b y))) "]"))))
    (println)))

(def home [nil [0 0] [19 0] [19 19] [0 19]])

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
;       p '([0 0] [1 0] [2 0] [1 1])]
;   (do
;     (dump-b! b)
;     (dorun
;       (for [[p x y] (collect-all b 1 +pieces+)]
;         (println p x y)))))

(defn play
  [b p s tx ty]
  (reduce (fn [b [dx dy]] (sq-put b (+ tx dx) (+ ty dy) s)) b p))

; (let [p '([0 0] [1 0] [2 0] [1 1])]
;   (do
;     (dump-b! (play +board+ p 1 5 5))
;     (dump-b! (play +board+ (rotate p) 3 5 5))))
