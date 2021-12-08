(ns advent2017.core
  (:use (advent2017.data))
  (:require [taoensso.tufte :as tufte :refer (defnp p)]))

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
       (recur (apply str (rest numstr)) (+ sofar (Character/digit (first numstr) 10)))
       (recur (apply str (rest numstr)) sofar))
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

(defn problem5_core
  [in index count withdec]
  (if (or (< index 0) (>= index (clojure.core/count in)))
    count
    (let [newcount (inc count)
          newindex (+ (in index) index)
          newval (if (and withdec (>= (in index) 3)) (dec (in index)) (inc (in index)))
          newvec (assoc in index newval)]
      (recur newvec newindex newcount withdec))))

(defn problem5_p1
  [in]
  (problem5_core in 0 0 false))

(defn problem5_p2
  [in]
  (problem5_core in 0 0 true))

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

(defrecord problem7_obj [name weight belowstrs atop])

(defn problem7_unique_index
  [in]
  (if (= (.indexOf in (first in)) (.lastIndexOf in (first in)))
    0
    (let [oddball (second (distinct in))]
      (.indexOf in oddball))))

(defn problem7_weight_supported
  [graph obj]
  (let [belowstrs (:belowstrs obj)
        subobjs (map #(get graph %) belowstrs)
        subweights (map #(problem7_weight_supported graph %) subobjs)
        subseq (if (> (count subweights) 0) (apply = subweights) true)
        subpassthrough (first (filter #(map? %) subweights))
        ]
    (if subseq
      (apply + (:weight obj) subweights)
      (if subpassthrough
        subpassthrough
        (let [
              uniqindex (problem7_unique_index subweights)
              delta (first (filter
                      #(not (= % 0))
                      (distinct (map
                                  #(- % (nth subweights uniqindex))
                                  subweights))))
              uniqstr (nth belowstrs uniqindex)
              uniqweight (:weight (nth subobjs uniqindex))
              retVal {:delta delta :obj uniqstr :weight uniqweight :wanted (+ delta uniqweight)}]
          retVal)))))

(defn problem7_trampoline
  [targetname]
  (fn [graph]
    (let [parent (-> (filter
                       #(and (some? (-> % second :belowstrs)) (> (.indexOf (-> % second :belowstrs) targetname) -1))
                       (-> graph seq))
                     first
                     second)]
      (if-not (nil? parent)
        (fn [] ( (:atop parent) graph))
        targetname))))

(defn problem7_spec2obj
  [[name weightstr & deps]]
  (let [weight (Integer/parseInt (clojure.string/replace weightstr #"[\(\)]" ""))
        supportstrs (map #(clojure.string/replace % "," "") (rest deps))
        supporter (problem7_trampoline name)]
    (problem7_obj. name weight supportstrs supporter)))

(defn problem7_initgraph
  [instr]
  (apply merge
         (for [line (clojure.string/split-lines instr)]
           (let [strs (clojure.string/split line #"\s+")]
             {(first strs)
              (problem7_spec2obj strs)}))))

(defn problem7_p1
  ([in]
   (problem7_p1 in (problem7_initgraph in)))
  ([in graph]
   (trampoline (-> graph seq first second :atop) graph)))

(defn problem7_p2
  [in]
  (let [graph  (problem7_initgraph in)
        root (problem7_p1 in graph)]
    (:wanted (problem7_weight_supported graph (get graph root)))))

(comment "Useful for day 8. The normal Clojure way is (not (= arg1 arg2))")
(defn !=
  [& args]
  (not (apply = args)))

(defn problem8_action
  [ctx [mod opstr amount _ test testop testamount]]
  (let [modkey (keyword mod)
        testkey (keyword test)
        iniint (get ctx modkey 0)
        op (if (= opstr "inc") +
             (if (= opstr "dec") -))
        op-int (Integer/parseInt amount)
        testint (Integer/parseInt testamount)
        testactual (get ctx testkey 0)]
    (if ((ns-resolve *ns* (symbol testop)) testactual testint)
       (assoc ctx modkey (op iniint op-int))
       ctx)))

(defn problem8_run_machine
  ([lines-remaining]
   (problem8_run_machine lines-remaining {}))
  ([lines-remaining ctx]
   (if (= (count lines-remaining) 0)
     ctx
     (let [linevec (clojure.string/split (clojure.string/trim (first lines-remaining)) #"\s+")
           newctx (problem8_action ctx linevec)]
       (recur (rest lines-remaining) newctx))))
  ([lines-remaining ctx maxthusfar] ; This is just for part 2.
   (if (= (count lines-remaining) 0)
     maxthusfar
     (let [linevec (clojure.string/split (clojure.string/trim (first lines-remaining)) #"\s+")
           newctx (problem8_action ctx linevec)]
       (recur (rest lines-remaining) newctx (apply max (conj (vals newctx) maxthusfar)))))))

(defn problem8_p1
  [data]
  (apply
    max
    (vals
      (let [lines (clojure.string/split-lines data)]
        (problem8_run_machine lines)))))

(defn problem8_p2
  [data]
  (let [lines (clojure.string/split-lines data)]
    (problem8_run_machine lines {} 0)))

(defn problem9_garbage
  [str sum]
  (let [this (first str)
        next (rest str)]
    (case this
      \! (recur (rest next) sum)
      \> {:next next :sum sum}
      (recur next (inc sum)))))

(defn problem9_subgroups
  [in depth sum garbage]
  (if (< (count in) 1)
    {:next in :depth depth :sum sum :garbage garbage}
    (let
      [this (first in)
       next (rest in)]
      (case this
        \{ (let [fromsub (problem9_subgroups next (inc depth) sum 0)]
             (recur (:next fromsub) depth (:sum fromsub) (+ (-> fromsub :garbage) garbage)))
        \} {:next next :depth depth :sum (+ sum depth) :garbage garbage}
        \!  (recur (rest next) depth sum garbage)
        \< (let [newgarbage (problem9_garbage next 0)]
             (recur (:next newgarbage) depth sum (+ garbage (:sum newgarbage))))
        (recur next depth sum garbage)))))

(defn problem9_p1
  ([in]
   (->
     (problem9_subgroups in 0 0 0) :sum)))

(defn problem9_p2
  [in]
  (->
  (problem9_subgroups in 0 0 0) :garbage))

(defn rotate
  [in moveby]
  (take (count in) (drop moveby (cycle in))))

(defn rotate-back
  [in moveby]
  (let [oneway (mod moveby (count in))
        otherway (- (count in) oneway)]
    (rotate in otherway)))

(defn problem10_sparse->dense
  [in]
  (map (partial apply bit-xor) (partition 16 16 in)))

(defn problem10_dense->hexstr
  [in]
  (apply str (map #(apply str (take-last 2 (concat "0" (Integer/toHexString %)))) in)))

(defn problem10_engine
  ([in max]
   (problem10_engine (vec (range max)) (vec in) 0 0))
  ([in lens skip moved]
   (if (> (count lens) 0)
     (let [length (first lens)
           newvec (vec (apply conj (vec (reverse (take length in))) (drop length in)))
           actualvec (vec (rotate newvec (+ skip length)))
           totalmoved (+ skip length moved)]
       (recur actualvec (vec (rest lens)) (inc skip) totalmoved))
     {:vec in :moved moved :skip skip})))

(defn problem10_p1
  [in]
  (let [invec (map #(Integer/parseInt %) (clojure.string/split in #","))
        raw_ans (problem10_engine invec 256)
        ans (rotate-back (:vec raw_ans) (:moved raw_ans))
        a (first ans)
        b (second ans)]
    (* a b)))

(defn problem10_p2
  [in]
  (let [lengths (concat (map int (map char in)) [17, 31, 73, 47, 23])
        manylengths (take (* 64 (count lengths)) (cycle lengths))
        state (problem10_engine manylengths 256)
        sparse (rotate-back (:vec state) (:moved state))
        hexstr (-> sparse problem10_sparse->dense problem10_dense->hexstr)]
    hexstr))

(defn problem11_state->dist
  [state]
  (apply + (vals (dissoc state :maxdist))))

(defn problem11_doadj
  [state to]
  (let [strto (name to)
        strdir (-> strto first str keyword)
        from (keyword (let [f (first strto)
                            newf (if (= f \n) \s \n)
                            from (str newf (second strto))]
                        from))
        valdir ((keyword strdir) state)
        valfrom (from state)
        canact (and (!= 0 valdir) (!= 0 valfrom))]
    (if canact (let [diff (min valdir valfrom)
                     newto (+ (to state) diff)
                     newdir (- valdir diff)
                     newfrom (- valfrom diff)
                     newstate (merge state {(keyword to) newto (keyword strdir) newdir from newfrom})]
                 newstate)
      state)))

(defn problem11_adj
  [state adjpossible]
  (if-let [moveto (first adjpossible)]
    (recur (problem11_doadj state moveto) (rest adjpossible))
    (if (> (count adjpossible) 0)
      (recur state (rest adjpossible))
      state)))

(defn problem11_state->consolidate
  [{:keys [nw n ne sw s se] :as state}]
  (let [myn (- n (min n s)) ;; Consolidate opposites
        mys (- s (min n s))
        mynw (- nw (min se nw))
        myse (- se (min se nw))
        myne (- ne (min sw ne))
        mysw (- sw (min sw ne))]
    (let [my2nw (- mynw (min mynw myne)) ;; Consolidate norths and souths to point just north or south
          my2n (+ myn (min mynw myne))
          my2ne (- myne (min mynw myne))
          my2sw (- mysw (min mysw myse))
          my2s (+ mys (min mysw myse))
          my2se (- myse (min mysw myse))
          newstate (merge state {:nw my2nw :n my2n :ne my2ne :sw my2sw :s my2s :se my2se})]
      (let [mystate (problem11_adj newstate [:nw :ne :sw :se])]
        (if (= mystate state)
          state
          (recur mystate))))))

(defn problem11_state
  ([in]
   (problem11_state in {:nw 0 :n 0 :ne 0 :sw 0 :s 0 :se 0 :maxdist 0}))
  ([in state]
   (if (= (count in) 0)
     state
     (let [now (first in)
           nowkey (keyword now)
           newstate (problem11_state->consolidate (assoc state nowkey (inc (nowkey state))))
           newstate (assoc newstate :maxdist (max
                                               (:maxdist newstate)
                                               (-> newstate problem11_state->dist)))]
       (recur (rest in) newstate)))))

(defn problem11_p1
  [in]
  (-> (clojure.string/split in #",")
      problem11_state
      problem11_state->dist))

(defn problem11_p2
  [in]
  (-> (clojure.string/split in #",")
      problem11_state
      :maxdist))

(defn problem12_build
  [lines]
  (apply merge (for [curline lines]
                 (let [line (clojure.string/split (clojure.string/trim curline) #"[^\d]+")]
                   {(first line) (rest line)}))))

(defn problem12_all_conn
  ([graph to]
   (problem12_all_conn graph to #{}))
  ([graph to all]
   (let [glom (peek to)
         newall (conj all glom)
         stack (vec (clojure.set/difference (set (apply conj (pop to) (get graph glom))) newall))]
     (if (> (count stack) 0)
       (recur graph stack newall)
       newall))))

(defn problem12_getallgroups
  [graph]
  (distinct (map #( problem12_all_conn graph [%]) (keys graph))))

(defn problem12_p1
  [in]
  (let [graph (problem12_build (clojure.string/split-lines in))
        ans (count (problem12_all_conn graph ["0"]))]
    ans))

(defn problem12_p2
  [in]
  (let [graph (problem12_build (clojure.string/split-lines in))
        ans (count (problem12_getallgroups graph))]
    ans))

(defn problem13_catchscore
  [[d r]]
  (let [backtotop (* 2 (- r 1))
        iscaught (= 0 (mod d backtotop))]
    (if iscaught (* d r) 0)))

(defn problem13_catchtrue
  [[d r] delay]
  (let [backtotop (* 2 (- r 1))
        iscaught (= 0 (mod (+ delay d) backtotop))]
    iscaught))

(defn problem13_p1
  [in]
  (let [lines (map #(clojure.string/split % #"[^\d]+") (clojure.string/split-lines in))
        allmap (merge (for [line lines] {(-> line first Integer/parseInt) (-> line second Integer/parseInt)}))
        allsum (apply + (map #(problem13_catchscore (-> % vec first)) (vec allmap)))]
    allsum))

(defn problem13_p2
  ([in]
  (let [lines (map #(clojure.string/split % #"[^\d]+") (clojure.string/split-lines in))
        allmap (vec (for [line lines] [(-> line first Integer/parseInt) (-> line second Integer/parseInt)]))]
    (problem13_p2 allmap 0)))
  ([allmap delay]
   (if (= (count (filter true? (map #(problem13_catchtrue % delay) allmap))) 0)
     delay
     (recur allmap (inc delay)))))

(defn problem14_strtobin
  [in]
  (let [hash (problem10_p2 in)
        asbin (apply str
                     (map
                       #(apply str
                               (take-last 4
                                          (concat
                                            "0000"
                                            (Integer/toString
                                              (Integer/parseInt (str %) 16)
                                              2))))
                       hash))]
    asbin))

(defn problem14_allbins
  [in]
  (vec (for [i (range 128)]
         (let [subj (str in "-" i)]
           (problem14_strtobin subj)))))

(defn problem14_countones
  [invec]
  (count (filter #(= \1 %) (apply str invec))))

(defn problem14_p1
  [in]
  (-> in problem14_allbins problem14_countones))

(defn problem14_indinstr
  ([in]
   (problem14_indinstr in "1"))
  ([in of]
   (problem14_indinstr in of 0 []))
  ([in of ind sofar]
   (let [newind (clojure.string/index-of in of ind)]
     (if (nil? newind)
       sofar
       (do (assert (= (str (get in newind)) "1") (str (get in newind)))
           (recur in of (inc newind) (conj sofar newind)))))))

(defn problem14_ones->locations
  ([in]
   (problem14_ones->locations in 0 []))
  ([in level arg]
   (if (= (count in) 0)
     arg
     (let [ind (mapv #(conj [level] %) (problem14_indinstr (first in)))]
       (recur (rest in) (inc level) (into arg ind))))))

(defn problem14_loc->locs
  [[a b]]
  #{[(dec a) b] [a (dec b)] [(inc a) b] [a (inc b)]})

(defn problem14_locs->region
  ([alllocs]
    (problem14_locs->region alllocs (hash-set (first alllocs)) (hash-set)))
  ([alllocs find-neighbors allinregion]
   (if (= 0 (count find-neighbors))
     allinregion
     (let [origin (first (vec find-neighbors))
           allasset (apply hash-set alllocs)
           newbies (clojure.set/intersection allasset (problem14_loc->locs origin))
           want (clojure.set/difference newbies (hash-set origin) allinregion)
           next-neighbors (clojure.set/union want (clojure.set/difference find-neighbors (hash-set origin)))
           next-allinregion (clojure.set/union allinregion want (hash-set origin))]
       (recur alllocs next-neighbors next-allinregion)))))

(defn problem14_allregions
  ([all]
   (problem14_allregions all []))
  ([all allregions]
   (if (= 0 (count all))
     allregions
     (let [newregion (problem14_locs->region all)
           next-all (clojure.set/difference (apply hash-set all) newregion)
           next-allregions (conj allregions newregion)]
       (recur next-all next-allregions)))))

(defn problem14_p2
  [in]
  (-> in problem14_allbins problem14_ones->locations problem14_allregions count))

(def problem15_factors {:A 16807 :B 48271})

(def problem15_start {:A 873 :B 583})

(def problem15_mod {:A 4 :B 8})

(defn problem15_nextnum
  [prev fact]
  (mod (* prev fact) 2147483647))

(defn problem15_rungen
  ([which lastval]
   (problem15_nextnum lastval (which problem15_factors))))

(defn problem15_tobin
  [num]
  (Integer/toString num 2))

(defn problem15_judge
  ([]
   (problem15_judge (:A problem15_start) (:B problem15_start) 0 0))
  ([lastA lastB nummatch numrun]
   (if (>= numrun (* 4 1000 1000 10))
     nummatch
     (let [newA (problem15_rungen :A lastA)
           newB (problem15_rungen :B lastB)
           domatch (= (take-last 16 (Integer/toString newA 2))
                      (take-last 16 (Integer/toString newB 2)))
           next-nummatch (if domatch (inc nummatch) nummatch)
           next-numrun (inc numrun)]
       (recur newA newB next-nummatch next-numrun)))))

(defn problem15_p2_nextnum
  [prev fact ofmod]
  (let [retVal (mod (* prev fact) 2147483647)]
    (if (= 0 (mod retVal ofmod))
      retVal
      (recur retVal fact ofmod))))

(defn problem15_p2_rungen
  ([which lastval]
   (problem15_p2_nextnum lastval (which problem15_factors) (which problem15_mod))))

(defn problem15_p2_judge
  ([]
   (problem15_p2_judge (:A problem15_start) (:B problem15_start) 0 0))
  ([lastA lastB nummatch numrun]
   (if (>= numrun (* 5 1000 1000))
     nummatch
     (let [newA (problem15_p2_rungen :A lastA)
           newB (problem15_p2_rungen :B lastB)
           domatch (= (take-last 16 (Integer/toString newA 2))
                      (take-last 16 (Integer/toString newB 2)))
           next-nummatch (if domatch (inc nummatch) nummatch)
           next-numrun (inc numrun)]
       (recur newA newB next-nummatch next-numrun)))))

(defn problem16_toindex
  [in]
  (.indexOf "abcefghijklmnopqrstuvwxyz" in))

(defn problem16_spin
  [from num]
  (let [tofront (vec (take-last num from))
        toend (vec (drop-last num from))]
    (apply str (into tofront toend))))

(defn problem16_doswitch
  [from indA indB]
  (let [firstInd (min indA indB)
        A (get from firstInd)
        secondInd (max indA indB)
        B (get from secondInd)
        firstVec (vec (take firstInd from))
        middleVec (subvec (vec from) (inc firstInd) secondInd)
        lastVec (subvec (vec from) (inc secondInd))]
    (apply str (into (conj firstVec B) (into (conj middleVec A) lastVec)))))

(defn problem16_exchange
  [from A B]
  (problem16_doswitch from A B))

(defn problem16_partner
  [from A B]
  (problem16_doswitch from (.indexOf from A) (.indexOf from B)))

(defn problem16_s
  [from arg]
  (p ::16-spin (problem16_spin from (Integer/parseInt arg))))

(defn problem16_x
  [from arg]
  (let [args (clojure.string/split arg #"/")
        A (Integer/parseInt (first args))
        B (Integer/parseInt (second args))]
  (p ::16-exchange (problem16_exchange from A B))))

(defn problem16_p
  [from arg]
  (let [args (clojure.string/split arg #"/")
        A (first args)
        B (second args)]
  (p ::16-partner (problem16_partner from A B))))

(defn problem16_run_token
  [from cmd]
  (p ::16-run_token ((p ::16-ns-resolve (ns-resolve *ns* (symbol (str "problem16_" (first cmd))))) from (apply str (rest cmd)))))

(def problem16_run_token_memo (memoize problem16_run_token))

(def problem16_do_tokens
  (memoize
    (fn
      [from tokens]
      (if (> (count tokens) 0)
        (recur (problem16_run_token_memo from (first tokens)) (rest tokens))
        from))))

(defn problem16_p1
  [tokenstr]
  (let [alltokens (clojure.string/split tokenstr #",")]
    (problem16_do_tokens "abcdefghijklmnop" alltokens)))

(def problem16_p2
  (memoize
  (fn
    ([tokenstr]
     (let [alltokens (clojure.string/split tokenstr #",")]
       (problem16_p2 alltokens "abcdefghijklmnop" 0)))
    ([alltokens instr num]
     (if (= (mod num 10000) 0) (prn num))
     (if (= (* 1 1000 1000 1000) num)
       instr
       (recur alltokens (problem16_do_tokens instr alltokens) (inc num)))))))

(defn problem17_cycle
  ([numrot max]
   (problem17_cycle [0] max numrot 1))
  ([in max numrot num]
   (if (= 0 (mod num 100)) (prn num))
   (if (<= num max)
     (recur (vec (rotate (conj (vec (rotate in numrot)) num) 1)) max numrot (inc num))
     in)))

(defn problem17_p1
  [in]
  (second (problem17_cycle in 2017)))

(defn problem17_p2
  [in]
  (let [thecycle (problem17_cycle in 50000000)]
    (nth thecycle (.indexOf thecycle 0))))
