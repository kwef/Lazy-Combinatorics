Lazy-Combinatorics
==================

Exploration of how to do fair traversals of simple combinatorial objects (e.g. powerset, cartesian product) in Clojure.

Currently, the powerset construction is efficient, but the interleaved-stream-based cartesian product, although fair (i.e. a hypercube-shaped exploration of the Cartesian product space) is not efficient, and slows to a crawl for large n.
