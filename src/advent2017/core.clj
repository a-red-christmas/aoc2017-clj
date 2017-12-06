(ns advent2017.core
  (:use (advent2017.data)))

(defn reload []  (use 'advent2017.data :reload) (use 'advent2017.core :reload))

(defmacro tryprob
  [num part]
  `(do
     (reload)
     (~(symbol (str  "problem" num "_p" part))  ~(symbol (str  "data" num)))))

; --------------------------------------------------------------------------
; ----->  Code for solutions <----------------------------------------------
; --------------------------------------------------------------------------

(defn problem1_p1
  ([numstr]
   (problem1_p1 (apply str (concat numstr (str (first numstr)))) 0))
  ([numstr sofar]
   (if (> (count numstr) 1)
     (if (= (first numstr) (second numstr))
       (problem1_p1 (apply str (rest numstr)) (+ sofar (Character/digit (first numstr) 10)))
       (problem1_p1 (apply str (rest numstr)) sofar))
     sofar)))

(defn problem1_p2
  ([numstr]
   (let [strlen (count numstr)]
     (problem1_p2 (apply str (concat numstr numstr)) 0 strlen)))
  ([numstr sofar strlen]
   (if (> (count numstr) strlen)
     (if (= (first numstr) (nth numstr (/ strlen 2)))
       (recur (apply str (rest numstr)) (+ sofar (Character/digit (first numstr) 10)) strlen)
       (recur (apply str (rest numstr)) sofar strlen))
     sofar)))

(defn problem2_p1
  [data]
  (apply
    +
    (for [line (clojure.string/split-lines data)]
      (do
        (let [vec (sort (map read-string (clojure.string/split line #"\s+")))
              min (first vec)
              max (last vec)]
          (- max min))))))

(defn problem2_p2
  [data]
  (apply
    +
    (for [line (clojure.string/split-lines data)]
      (first
        (let [nums (map read-string (clojure.string/split line #"\s+"))]
          (filter
            some?
            (for [a nums
                  b nums]
              (if
                (and
                  (> a b)
                  (not (= a b))
                  (= (mod a b) 0))
                (/ a b)
                nil))))))))

(defn- problem3_addright
  [ingrid]
  (let [newstart (+ 1 (peek (peek ingrid)))
        newright (mapv #(+ newstart %) (reverse (range (count ingrid))))]
    (vec (for [i (range (count ingrid))]
        (conj (get ingrid i) (get newright i))))))

(defn- problem3_addtop
  [ingrid]
  (let [newstart (+ 1 (peek (first ingrid)))
        newtop (mapv #(+ newstart %) (reverse (range (count (first ingrid)))))]
    (vec (into (vector newtop) ingrid))))

(defn- problem3_addleft
  [ingrid]
  (let [newstart (+ 1 (first (first ingrid)))
        newright (mapv #(+ newstart %) (range (count ingrid)))]
    (vec (for [i (range (count ingrid))]
      (into (vector (get newright i)) (get ingrid i) )))))

(defn- problem3_addbottom
  [ingrid]
  (let [newstart (+ 1 (first (peek ingrid)))
        newbot (mapv #(+ newstart %) (range (count (first ingrid))))]
    (conj ingrid newbot)))

(defn- problem3_gentilnum
  ([num]
   (problem3_gentilnum [[1]] num))
  ([grid num]
   (let [newgrid (problem3_addbottom  (problem3_addleft  (problem3_addtop  (problem3_addright grid))))]
     (if (> (peek (peek newgrid)) num)
       newgrid
       (recur newgrid num)))))

(defn- problem3_manhattan
  [grid num]
  (let [indexes (map #(.indexOf % num) grid)
        y (.indexOf (map #(if (> % -1) 1 0) indexes) 1)
        x (nth indexes y)
        midindex (/ (- (count grid) 1) 2)]
    (map #(clojure.pprint/pprint %) [indexes y x])
    (+ (Math/abs (- midindex y)) (Math/abs (- midindex x)))))

(defn problem3_p1
  [num]
  (problem3_manhattan
    (problem3_gentilnum num)
    num))

(defn- problem3_p2_sum-progressive
  ([in]
   (problem3_p2_sum-progressive in [] 0))
  ([in working val]
   (if (= (count in) 0)
     working
     (let [nextval (+ (first in) val)]
       (recur
         (rest in)
         (conj working nextval)
         nextval)))))

(defn- problem3_p2_old->new
  [old]
  (let [ininew (mapv #(apply + %)
                     (conj
                       (into
                         [(first
                            (partition 2 1 old))]
                         (partition 3 1 old))
                       (last
                         (partition 2 1 old))))]
    (problem3_p2_sum-progressive ininew)))

(defn- problem3_p2_addright
  [ingrid]
  (let [oldright (mapv last ingrid)
        newright (vec (reverse (problem3_p2_old->new (reverse oldright))))]
    (vec (for [i (range (count ingrid))]
        (conj (get ingrid i) (get newright i))))))

(defn- problem3_p2_addtop
  [ingrid]
  (let [oldtop (first ingrid)
        newtop (reverse (problem3_p2_old->new (reverse oldtop)))]
    (vec (into (vector newtop) ingrid))))

(defn- problem3_p2_addleft
  [ingrid]
  (let [oldleft (mapv first ingrid)
        newleft (problem3_p2_old->new oldleft)]
    (vec (for [i (range (count ingrid))]
      (into (vector (get newleft i)) (get ingrid i) )))))

(defn- problem3_p2_addbottom
  [ingrid]
  (let [oldbot (last ingrid)
        newbot (problem3_p2_old->new oldbot)]
    (conj ingrid newbot)))

(defn- problem3_p2_gentilnum
  ([num]
  (problem3_p2_gentilnum [[5 4 2] [10 1 1] [11 23 25]] num))
  ([grid num]
  (let [newgrid (problem3_p2_addbottom (problem3_p2_addleft (problem3_p2_addtop (problem3_p2_addright grid))))]
     (if (> (peek (peek newgrid)) num)
       newgrid
       (recur newgrid num)))))

(defn- problem3_p2_firstlarger
  [grid num]
  (first (sort (filter #(> % num) (flatten grid)))))

(defn problem3_p2
  [num]
  (problem3_p2_firstlarger
    (problem3_p2_gentilnum num)
    num))

(defn- problem4_validray
  [in]
  (for [line (clojure.string/split-lines in)]
    (let [strs (map read-string (clojure.string/split line #"\s+"))]
      (apply distinct? strs))))

(defn problem4_p1
  [in]
  (count
    (filter
      true?
      (problem4_validray in))))

(defn- problem4_p2_validray
  [in]
  (for [line (clojure.string/split-lines in)]
    (let [strs (mapv sort (clojure.string/split line #"\s+"))]
      (apply distinct? strs))))

(defn problem4_p2
  [strs]
  (count
    (filter
      true?
      (problem4_p2_validray strs))))

(defn problem5_p1
  ([in]
   (problem5_p1 in 0 0))
  ([in index count]
   (if (or (< index 0) (>= index (clojure.core/count in)))
     count
     (let [newcount (inc count)
           newindex (+ (in index) index)
           newval (inc (in index))
           newvec (assoc in index newval)]
       (recur newvec newindex newcount)))))

(defn problem5_p2
  ([in]
   (problem5_p2 in 0 0))
  ([in index count]
   (if (or (< index 0) (>= index (clojure.core/count in)))
     count
     (let [newcount (inc count)
           newindex (+ (in index) index)
           newval (if (>= (in index) 3) (dec (in index)) (inc (in index)))
           newvec (assoc in index newval)]
       (recur newvec newindex newcount)))))

(defn problem6_newvec
  [in start points per]
  (if (= 0 points)
    in
    (let [ind (mod start (count in))]
      (recur
        (assoc in ind (+ (in ind) (min points per)))
        (inc start)
        (- points (min points per))
        per))))

(defn problem6_redistrib
  [in]
  (let [maxval (apply max in)
        bank (.indexOf in maxval)
        startvec (assoc in bank 0)
        start (+ 1 bank)
        per (int (Math/ceil (/ maxval (count in))))]
    (problem6_newvec startvec start maxval per)))

(defn problem6_track
  ([in]
   (problem6_track in []))
  ([in sofar]
   (let [newin (problem6_redistrib in)
         indlast (.indexOf sofar newin)
         newsofar (conj sofar newin)]
     (if-not (= indlast -1)
       [(+ 1 (count sofar)) (- (.lastIndexOf newsofar newin) (.indexOf newsofar newin))]
       (recur newin newsofar)))))

(defn problem6_p1
  [in]
  (first (problem6_track in)))

(defn problem6_p2
  [in]
  (second (problem6_track in)))
