(ns lazy-combinatorics.core)

; (require :reload '[clojure.repl :refer :all] '[lazy-combinatorics.core :refer :all]) (set! *print-length* 100)

; Some familiar infinite sequences to play with...
(def naturals  (iterate inc 1))
(def integers+ (iterate inc 0))
(def integers- (iterate dec 0))

; Some logic...

(defn xor ([a b] (or (and (not a) b) (and a (not b)))))
(defn iff ([a b] (not (xor a b))))
(defn implies ([a b] (or a (not b))))
(defn nand ([& xs] (not (reduce #(and %1 %2) true xs))))
(defn nor ([& xs] (not (reduce #(or %1 %2) false xs))))

; Other helper utilities...

(defn contains-all?
  ([coll things]
   (reduce #(and %1 (contains? coll %2)) true things)))

(defn take-complete
  ([n coll]
   (let [part (take n coll)]
        (if (= (count part) n) part))))

; Now the real stuff...

(defn pattern-interleave
  "Clojure's interleave on steroids. Takes two vectors of [s1 s2 ... sn] sequences. The first vector contains the sequences to interleave (what one would pass as arguments to vanilla interleave); the second, sequences of integers which describe how many elements to take from each sequence in each step as they are interleaved.
  
  For example:
  user=> (println (take 20 (pattern-interleave [(cycle [\\X]) (cycle [\\O])]
                                               [(cycle [1])  (iterate inc 1)])))
  (X O X O O X O O O X O O O O X O O O O O)
  nil
    
  There are two boolean keyword arguments :complete and :blocking (both by default false). If :complete is true, then when a sequence ends before the requested number of items is taken from it, no more items are taken from it. If :blocking is true, then when any sequence ends, the result sequence ends.
  
  Note: To achieve the exact effect of Clojure's regular interleave, one could use:
  (pattern-interleave [s1 s2 ... sn] (cycle [(cycle [1])]) :blocking true)"
  ([colls spacings & {:keys [complete blocking]}]
   (lazy-seq
     (let [amounts (map (comp (fnil identity 0) first) spacings)
           fronts  (map (if complete take-complete take) amounts colls)]
          (when ((if blocking every? some) seq fronts)
                (concat (apply concat fronts)
                        (pattern-interleave
                          (map drop amounts colls)
                          (map rest spacings)
                          :complete complete
                          :blocking blocking)))))))

(defn base-n-seq
  "Generates a lazy sequence of numbers representing the base-b form of number n, starting with least digit value and increasing."
  ([b n]
   {:pre [(> b 1) (zero? (rem b 1)) (zero? (rem n 1))]}
   (lazy-seq
     (when (not (zero? n))
           (cons (int (rem n b))
                 (base-n-seq b (quot n b)))))))

(defn numbered-subset
  "Returns a particular subset of a coll, in the ordering where 0 -> nil, 1 -> [1], 2 -> [2], 3 -> [1 2], etc."
  ([subset-number coll]
   (map (fn [[n e]] e)
        (filter (fn [[n e]] (not (zero? n)))
                (map vector (base-n-seq 2 subset-number) coll)))))

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
          (lazy-seq (cons (sequence nil) (powerset-iter coll coll (powers-of 2)))))))


