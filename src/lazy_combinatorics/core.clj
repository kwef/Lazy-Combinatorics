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

; From https://github.com/cgrand/utils:
(defn reduce-by
  "Returns a maps keyed by the result of key-fn on each element to the result of calling reduce with f (and val if provided) on same-key elements. (reduce-by key-fn conj [] coll) is equivalent to (group-by key-fn coll). (reduce-by identity (fn [n _] (inc n)) 0 coll) is equivalent to (frequencies coll)."
  ([key-fn f val coll]
   (persistent!
     (reduce (fn [m x]
               (let [k (key-fn x)]
                 (assoc! m k (f (m k val) x)))) (transient {}) coll)))
  ([key-fn f coll]
   (let [g (fn g [acc x]
             (if (= g acc)
               x
               (f acc x)))]
     (reduce-by key-fn g g coll))))

(defn take-complete
  "Returns (take n coll) iff there are at least n items in the sequence. If the sequence is too short, returns nil. Note that in order to verify that the sequence has enough items in it, this function must by necessity be non-lazy."
  ([n coll]
   (let [part (take n coll)]
     (if (= (count part) n) part))))

(defn powers-of
  "Lazy sequence of the successive powers of n."
  ([n] (iterate (partial *' n) 1)))

(defn factorial
  "Factorial of an integer."
  ([n]
   {:pre [(>= n 0)]}
   (reduce *' (take n naturals))))

(defn binomial
  "Binomial coefficient for (n, k)."
  ([n k]
   {:pre [(<= k n) (>= k 0) (>= k 0)]}
   (/ (factorial n)
      (* (factorial k)
         (factorial (- n k))))))

(defn base-n-seq
  "Generates a lazy sequence of numbers representing the base-b form of number n, starting with least digit value and increasing."
  ([b n]
   {:pre [(> b 1) (zero? (rem b 1)) (zero? (rem n 1))]}
   (lazy-seq
     (when (not (zero? n))
       (cons (int (rem n b))
             (base-n-seq b (quot n b)))))))

; Lazy powerset...

(defn- numbered-subset
  "Returns a particular subset of a coll, in the ordering where 0 -> nil, 1 -> [1], 2 -> [2], 3 -> [1 2], etc."
  ([subset-number coll]
   (map (fn [[_ e]] e)
        (filter (fn [[n _]] (not (zero? n)))
                (map vector (base-n-seq 2 subset-number) coll)))))

(defn- n-subsets-from
  "Returns a range of numbered subsets of a coll, starting at a particular subset number. If no start index is provided, assumes that it should take n subsets starting at the nth subset."
  ([n-subsets coll]
   (n-subsets-from n-subsets n-subsets coll))
  ([n-subsets from-index coll]
   (lazy-seq
     (when (> n-subsets 0)
       (cons (numbered-subset from-index coll)
             (n-subsets-from (dec n-subsets) (inc from-index) coll))))))

(defn powerset
  "Returns a lazy sequence of the powerset of coll. For infinite sequences, only outputs the finite powerset -- that is, { s | s in P(coll) and |s| is finite }. For an enumeration of the full powerset of a countably infinite sequence, see Cantor."
  ([coll]
   (letfn [(powerset-iter [coll rem-coll powers]
                          (lazy-seq
                            (when (seq rem-coll)
                              (concat (n-subsets-from (first powers) coll)
                                      (powerset-iter coll (rest rem-coll) (rest powers))))))]
     (lazy-seq (cons (sequence nil) (powerset-iter coll coll (powers-of 2)))))))

; Finer control over interleaving sequences...

(defn pattern-interleave
  "Clojure's interleave on steroids. Takes two vectors of [s1 s2 ... sn] sequences. The first vector contains sequences of integers which describe how many elements to take from each sequence in each step as they are interleaved. The second vector contains the sequences to interleave (what one would pass as arguments to vanilla interleave).
  
  For example:
  user=> (println (take 20 (pattern-interleave [(repeat 1)  (iterate inc 1)]
                                               [(repeat \\X) (repeat \\O)])))
  (X O X O O X O O O X O O O O X O O O O O)
  nil
  
  There are two boolean keyword arguments :complete and :blocking (both by default false). If :complete is true, then when a sequence ends before the requested number of items is taken from it, no more items are taken from it. If :blocking is true, then when any sequence ends, the result sequence ends.
  
  Note: To achieve the exact effect of Clojure's regular interleave, one could use:
  (pattern-interleave (repeat (repeat 1)) [s1 s2 ... sn] :blocking true)"
  ([spacings colls & {:keys [complete blocking]}]
   (lazy-seq
     (let [amounts (map (comp (fnil identity 0) first) spacings)
           fronts  (map (if complete take-complete take) amounts colls)]
       (when ((if blocking every? some) seq fronts)
         (concat (apply concat fronts)
                 (pattern-interleave
                   (map rest spacings)
                   (map drop amounts colls)
                   :complete complete
                   :blocking blocking)))))))

; Lazy Cartesian product...

(defn possible-changes
  "Returns a sequence of all possible distinct changes from a vector element's initial value to a new value computed by func applied to that element of the vector. The result list includes the null change; that is, no changes at all. A predicate is used to constrain whether an element may be changed."
  ([pred func v]
   (map (partial reduce #(assoc %1 %2 (func (get v %2))) v)
        (powerset (filter identity (map-indexed #(if (pred %2) %1) v))))))

(defn n-hypercubes
  "Returns the lazy sequence of (1^n 2^n 3^n ...)."
  ([n] (apply map *' (take (inc n) (cons (repeat 1) (repeat naturals))))))

(defn zip
  "Returns a sequence of vectors of the successive heads of each collection, where nil replaces elements of collections which are empty. Is its own inverse; that is, (apply zip (zip & colls)) should be equal to colls, assuming all elements of colls are same count."
  ([& colls]
   (when (some seq colls)
     (lazy-seq
       (cons (vec (map first colls))
             (apply zip (map rest colls)))))))

(defn constrained-cartesian
  ([moves colls]
   (when (every? seq colls)
     (lazy-seq
       (cons (map first colls)
             (->> (map (comp (juxt first (partial apply constrained-cartesian))
                             (juxt (fn [f _] f) (partial map (fn [f c] (f c)))))
                       (filter (fn [fns] (not (every? #(= identity %) fns)))
                               (possible-changes
                                 #(= % identity) (constantly rest) moves))
                       (repeat colls))
                  (reduce-by
                    (fn [[fns _]] (count (filter #(= % identity) fns)))
                    #(conj %1 (second %2))
                    []
                    ,,)
                  (sort-by key > ,,)
                  (map (fn [[number colls]]
                           [(map (partial * (count colls)) (n-hypercubes number))
                            (pattern-interleave (repeat (repeat 1)) colls)])
                       ,,)
                  (apply zip ,,)
                  (apply pattern-interleave ,,)))))))

(defn cartesian
  ([& colls]
   (constrained-cartesian (vec (repeat (count colls) identity)) colls)))
