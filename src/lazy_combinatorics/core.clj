(ns lazy-combinatorics.core)

; (set! *print-length* 100)

; Some familiar infinite sequences to play with...
(def naturals  (iterate inc 1))
(def integers+ (iterate inc 0))
(def integers- (iterate dec 0))

(defn interleave*
  "Returns a lazy seq of the first item in each coll, then the second etc. If the end of a coll is reached, continues to interleave remaining non-nil colls."
  ([& colls] 
   (lazy-seq 
     (let [ss (filter seq colls)]
          (when (some identity ss)
                (concat (map first ss)
                        (apply interleave* (map rest ss))))))))

(defn n-ary-seq
  "Generates a lazy sequence of numbers representing the base-b form of number n, starting with least digit value and increasing."
  ([b n]
   {:pre [(> b 1) (zero? (rem b 1)) (zero? (rem n 1))]}
   (lazy-seq
     (when (not (zero? n))
           (cons (int (rem n b))
                 (n-ary-seq b (quot n b)))))))

(defn numbered-subset
  "Returns a particular subset of a coll, in the ordering where 0 -> nil, 1 -> [1], 2 -> [2], 3 -> [1 2], etc."
  ([subset-number coll]
   (map (fn [[n e]] e)
        (filter (fn [[n e]] (not (zero? n)))
                (map vector (n-ary-seq 2 subset-number) coll)))))

(defn n-subsets-from
  "Returns a range of numbered subsets of a coll, starting at a particular subset number. If no start index is provided, assumes that it should take n subsets starting at the nth subset."
  ([n-subsets coll]
   (n-subsets-from n-subsets n-subsets coll))
  ([n-subsets from-index coll]
   (lazy-seq
     (when (> n-subsets 0)
           (cons (numbered-subset from-index coll)
                 (n-subsets-from (dec n-subsets) (inc from-index) coll))))))

(defn powers-of
  "Lazy sequence of the successive powers of n."
  ([n] (iterate (partial *' n) 1)))

(defn finite-powerset
  "Returns a lazy sequence of the powerset of coll. For infinite sequences, only outputs the finite powerset -- that is, { s | s in P(coll) and |s| is finite }. For an enumeration of the full powerset of a countably infinite sequence, see Cantor."
  ([coll]
   (letfn [(powerset-iter [coll rem-coll powers]
             (lazy-seq
               (when (seq rem-coll)
                     (lazy-cat (n-subsets-from (first powers) coll)
                               (powerset-iter coll (rest rem-coll) (rest powers))))))]
          (lazy-seq (cons nil (powerset-iter coll coll (powers-of 2)))))))


