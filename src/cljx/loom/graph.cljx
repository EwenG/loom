(ns ^{:doc "Defines protocols for graphs, digraphs, and weighted graphs.
Also provides record implementations and constructors for simple graphs --
weighted, unweighted, directed, and undirected. The implementations are based
on adjacency lists."
      :author "Justin Kramer"}
    loom.graph
  (:require [loom.alg-generic :refer [bf-traverse]]))

;;;
;;; Protocols
;;;

(defprotocol Graph
  (nodes [g] "Return a collection of the nodes in graph g")
  (edges [g] "Edges in g. May return each edge twice in an undirected graph")
  (has-node? [g node] "Return true when node is in g")
  (has-edge? [g n1 n2] "Return true when edge [n1 n2] is in g")
  (successors [g] [g node]
              "Return direct successors of node, or (partial successors g)")
  (out-degree [g node] "Return the number of outgoing edges of node")
  (out-edges [g node] "Return all the outgoing edges of node"))

(defprotocol Digraph
  (predecessors [g] [g node]
                "Return direct predecessors of node, or (partial predecessors g)")
  (in-degree [g node] "Return the number direct predecessors to node")
  (in-edges [g node] "Return all the incoming edges of node")
  (transpose [g] "Return a graph with all edges reversed"))

(defprotocol WeightedGraph
  (weight [g] [g e] [g n1 n2] "Return weight of edge e or edge [n1 n2] or (partial weight g)"))

(defprotocol EditableGraph
  (add-nodes* [g nodes] "Add nodes to graph g. See add-nodes")
  (add-edges* [g edges] "Add edges to graph g. See add-edges")
  (remove-nodes* [g nodes] "Remove nodes from graph g. See remove-nodes")
  (remove-edges* [g edges] "Removes edges from graph g. See remove-edges")
  (remove-all [g] "Removes all nodes and edges from graph g"))

(defprotocol Edge
  (src [edge] "Returns the source node of the edge")
  (dest [edge] "Returns the dest node of the edge"))

; Default implementation for vectors
(extend-type #+clj clojure.lang.IPersistentVector #+cljs PersistentVector
  Edge
  (src [edge] (get edge 0))
  (dest [edge] (get edge 1)))

; Default implementation for maps
(extend-type #+clj clojure.lang.IPersistentMap #+cljs PersistentHashMap
  Edge
  (src [edge] (:src edge))
  (dest [edge] (:dest edge)))

;; Variadic wrappers

(defn add-nodes
  "Add nodes to graph g. Nodes can be any type of object"
  [g & nodes]
  (add-nodes* g nodes))

(defn add-edges
  "Add edges to graph g. For unweighted graphs, edges take the form [n1 n2].
  For weighted graphs, edges take the form [n1 n2 weight] or [n1 n2], the
  latter defaulting to a weight of 1"
  [g & edges]
  (add-edges* g edges))

(defn remove-nodes
  "Remove nodes from graph g"
  [g & nodes]
  (remove-nodes* g nodes))

(defn remove-edges
  "Remove edges from graph g. Do not include weights"
  [g & edges]
  (remove-edges* g edges))

;;;
;;; Records for basic graphs -- one edge per vertex pair/direction,
;;; loops allowed
;;;
;; TODO: allow custom weight fn?
;; TODO: preserve metadata?
;; TODO: leverage zippers for faster record updates?

(defrecord BasicEditableGraph [nodeset adj])
(defrecord BasicEditableDigraph [nodeset adj in])
(defrecord BasicEditableWeightedGraph [nodeset adj])
(defrecord BasicEditableWeightedDigraph [nodeset adj in])

(def ^{:dynamic true
       :doc "Weight used when none is given for edges in weighted graphs"}
  *default-weight* 1)

(def default-graph-impls
  {:all
    {:nodes (fn [g]
              (:nodeset g))
     :edges (fn [g]
              (for [n1 (nodes g)
                    e (out-edges g n1)]
                e))
     :has-node? (fn [g node]
                  (contains? (:nodeset g) node))
     :has-edge? (fn [g n1 n2]
                  (contains? (get-in g [:adj n1]) n2))
     :out-degree (fn [g node]
                   (count (get-in g [:adj node])))
     :out-edges (fn
                  ([g] (partial out-edges g))
                  ([g node] (for [n2 (successors g node)] [node n2])))}

   ;; Unweighted graphs store adjacencies as {node #{neighbor}}
   :unweighted
    {:add-nodes* (fn [g nodes]
                   (reduce
                     (fn [g n]
                       (-> g
                           (update-in [:nodeset] conj n)
                           (assoc-in [:adj n] (or ((:adj g) n) #{}))))
                     g nodes))
     :successors (fn
                   ([g] (partial successors g))
                   )}

   ;; Weighted graphs store adjacencies as {node {neighbor weight}}
   :weighted
    {:add-nodes* (fn [g nodes]
                   (reduce
                     (fn [g n]
                       (-> g
                           (update-in [:nodeset] conj n)
                           (assoc-in [:adj n] (or ((:adj g) n) {}))))
                     g nodes))
     :successors (fn
                   ([g] (partial successors g))
                   ([g node] (keys (get-in g [:adj node]))))}})






(defn default-graph-nodes [g] (:nodeset g))
(defn default-graph-edges [g] (for [n1 (nodes g)
                          e (out-edges g n1)]
                      e))
(defn default-graph-has-node? [g node]
  (contains? (:nodeset g) node))
(defn default-graph-has-edge? [g n1 n2]
  (contains? (get-in g [:adj n1]) n2))
(defn default-graph-out-degree [g node]
  (count (get-in g [:adj node])))
(defn default-graph-out-edges
  ([g] (partial out-edges g))
  ([g node] (for [n2 (successors g node)] [node n2])))
(defn default-graph-out-edges
  ([g] (partial out-edges g))
  ([g node] (for [n2 (successors g node)] [node n2])))
(defn default-graph-unweighted-add-nodes* [g nodes]
  (reduce
    (fn [g n]
      (-> g
          (update-in [:nodeset] conj n)
          (assoc-in [:adj n] (or ((:adj g) n) #{}))))
    g nodes))
(defn default-graph-unweighted-successors
  ([g] (partial successors g))
  ([g node] (get-in g [:adj node])))
(defn default-graph-weighted-add-nodes* [g nodes]
  (reduce
    (fn [g n]
      (-> g
          (update-in [:nodeset] conj n)
          (assoc-in [:adj n] (or ((:adj g) n) {}))))
    g nodes))
(defn default-graph-weighted-successors
  ([g] (partial successors g))
  ([g node] (keys (get-in g [:adj node]))))




(def default-digraph-impl
  {:predecessors (fn
                   ([g] (partial predecessors g))
                   ([g node] (get-in g [:in node])))
   :in-degree (fn [g node]
                (count (get-in g [:in node])))
   :in-edges (fn
               ([g] (partial in-edges g))
               ([g node] (for [n2 (predecessors g node)] [n2 node])))})



(defn default-digraph-predecessors
  ([g] (partial predecessors g))
  ([g node] (get-in g [:in node])))
(defn default-digraph-in-degree [g node]
  (count (get-in g [:in node])))
(defn default-digraph-in-edges
  ([g] (partial in-edges g))
  ([g node] (for [n2 (predecessors g node)] [n2 node])))




(def default-weighted-graph-impl
  {:weight (fn
             ([g] (partial weight g))
             ([g e] (weight g (src e) (dest e)))
             ([g n1 n2] (get-in g [:adj n1 n2])))})



(defn default-weighted-weight
  ([g] (partial weight g))
  ([g e] (weight g (src e) (dest e)))
  ([g n1 n2] (get-in g [:adj n1 n2])))



(defn- remove-adj-nodes [m nodes adjacents remove-fn]
  (reduce
    (fn [m n]
      (if (m n)
        (update-in m [n] #(apply remove-fn % nodes))
        m))
    (apply dissoc m nodes)
    adjacents))

(defprotocol Graph
  (nodes [g] "Return a collection of the nodes in graph g")
  (edges [g] "Edges in g. May return each edge twice in an undirected graph")
  (has-node? [g node] "Return true when node is in g")
  (has-edge? [g n1 n2] "Return true when edge [n1 n2] is in g")
  (successors [g] [g node]
              "Return direct successors of node, or (partial successors g)")
  (out-degree [g node] "Return the number of outgoing edges of node")
  (out-edges [g node] "Return all the outgoing edges of node"))



(extend-type BasicEditableGraph
  Graph
  (nodes [g] (default-graph-nodes g))
  (edges [g] (default-graph-edges g))
  (has-node? [g node] (default-graph-has-node? g node))
  (has-edge? [g n1 n2] (default-graph-has-edge? g n1 n2))
  (out-degree [g node] (default-graph-out-degree g node))
  (out-edges [g node] (default-graph-out-edges g node))
  (successors ([g] (default-graph-unweighted-successors g))
    ([g node] (default-graph-unweighted-successors g node)))

  EditableGraph
  (add-nodes* [g nodes]
              (reduce
                (fn [g node] (update-in g [:nodeset] conj node))
                g nodes))
  (add-edges* [g edges]
              (reduce
                (fn [g [n1 n2]]
                  (-> g
                      (update-in [:nodeset] conj n1 n2)
                      (update-in [:adj n1] (fnil conj #{}) n2)
                      (update-in [:adj n2] (fnil conj #{}) n1)))
                g edges))
  (remove-nodes* [g nodes]
                 (let [nbrs (mapcat #(successors g %) nodes)]
                   (-> g
                       (update-in [:nodeset] #(apply disj % nodes))
                       (assoc :adj (remove-adj-nodes (:adj g) nodes nbrs disj)))))
  (remove-edges* [g edges]
                 (reduce
                   (fn [g [n1 n2]]
                     (-> g
                         (update-in [:adj n1] disj n2)
                         (update-in [:adj n2] disj n1)))
                   g edges))
  (remove-all [g]
              (assoc g :nodeset #{} :adj {})))






(extend-type BasicEditableDigraph
  Graph
  (nodes [g] (default-graph-nodes g))
  (edges [g] (default-graph-edges g))
  (has-node? [g node] (default-graph-has-node? g node))
  (has-edge? [g n1 n2] (default-graph-has-edge? g n1 n2))
  (out-degree [g node] (default-graph-out-degree g node))
  (out-edges [g node] (default-graph-out-edges g node))
  (successors ([g] (default-graph-unweighted-successors g))
    ([g node] (default-graph-unweighted-successors g node)))

  EditableGraph
  (add-nodes* [g nodes]
    (reduce
      (fn [g node] (update-in g [:nodeset] conj node))
      g nodes))
  (add-edges* [g edges]
    (reduce
      (fn [g [n1 n2]]
        (-> g
            (update-in [:nodeset] conj n1 n2)
            (update-in [:adj n1] (fnil conj #{}) n2)
            (update-in [:in n2] (fnil conj #{}) n1)))
      g edges))
  (remove-nodes* [g nodes]
    (let [ins (mapcat #(predecessors g %) nodes)
          outs (mapcat #(successors g %) nodes)]
      (-> g
          (update-in [:nodeset] #(apply disj % nodes))
          (assoc :adj (remove-adj-nodes (:adj g) nodes ins disj))
          (assoc :in (remove-adj-nodes (:in g) nodes outs disj)))))
  (remove-edges* [g edges]
    (reduce
      (fn [g [n1 n2]]
        (-> g
            (update-in [:adj n1] disj n2)
            (update-in [:in n2] disj n1)))
      g edges))
  (remove-all [g]
    (assoc g :nodeset #{} :adj {} :in {}))
  Digraph
  (predecessors
    ([g] (default-digraph-predecessors g))
    ([g node] (default-digraph-predecessors g node)))
  (in-degree [g node] (default-digraph-in-degree g node))
  (in-edges
    ([g] (default-digraph-in-edges g))
    ([g node] (default-digraph-in-edges g node)))
  (transpose [g] (assoc g :adj (:in g) :in (:adj g))))



(extend-type BasicEditableWeightedGraph
  Graph
  (nodes [g] (default-graph-nodes g))
  (edges [g] (default-graph-edges g))
  (has-node? [g node] (default-graph-has-node? g node))
  (has-edge? [g n1 n2] (default-graph-has-edge? g n1 n2))
  (out-degree [g node] (default-graph-out-degree g node))
  (out-edges [g node] (default-graph-out-edges g node))
  (successors ([g] (default-graph-weighted-successors g))
    ([g node] (default-graph-weighted-successors g node)))

  EditableGraph
  (add-nodes* [g nodes]
    (reduce
      (fn [g node] (update-in g [:nodeset] conj node))
      g nodes))
  (add-edges* [g edges]
    (reduce
      (fn [g [n1 n2 & [w]]]
        (-> g
            (update-in [:nodeset] conj n1 n2)
            (assoc-in [:adj n1 n2] (or w *default-weight*))
            (assoc-in [:adj n2 n1] (or w *default-weight*))))
      g edges))
  (remove-nodes* [g nodes]
    (let [nbrs (mapcat #(successors g %) nodes)]
      (-> g
          (update-in [:nodeset] #(apply disj % nodes))
          (assoc :adj (remove-adj-nodes (:adj g) nodes nbrs dissoc)))))
  (remove-edges* [g edges]
    (reduce
      (fn [g [n1 n2]]
        (-> g
            (update-in [:adj n1] dissoc n2)
            (update-in [:adj n2] dissoc n1)))
      g edges))
  (remove-all [g]
    (assoc g :nodeset #{} :adj {}))


  WeightedGraph
  (weight
    ([g] (default-weighted-weight g))
    ([g e] (default-weighted-weight g e))
    ([g n1 n2] (default-weighted-weight g n1 n2))))


(extend-type BasicEditableWeightedDigraph
  Graph
  (nodes [g] (default-graph-nodes g))
  (edges [g] (default-graph-edges g))
  (has-node? [g node] (default-graph-has-node? g node))
  (has-edge? [g n1 n2] (default-graph-has-edge? g n1 n2))
  (out-degree [g node] (default-graph-out-degree g node))
  (out-edges [g node] (default-graph-out-edges g node))
  (successors ([g] (default-graph-weighted-successors g))
              ([g node] (default-graph-weighted-successors g node)))

  EditableGraph
  (add-nodes* [g nodes]
              (reduce
                (fn [g node] (update-in g [:nodeset] conj node))
                g nodes))
  (add-edges* [g edges]
              (reduce
                (fn [g [n1 n2 & [w]]]
                  (-> g
                      (update-in [:nodeset] conj n1 n2)
                      (assoc-in [:adj n1 n2] (or w *default-weight*))
                      (update-in [:in n2] (fnil conj #{}) n1)))
                g edges))
  (remove-nodes* [g nodes]
                 (let [ins (mapcat #(predecessors g %) nodes)
                       outs (mapcat #(successors g %) nodes)]
                   (-> g
                       (update-in [:nodeset] #(apply disj % nodes))
                       (assoc :adj (remove-adj-nodes (:adj g) nodes ins dissoc))
                       (assoc :in (remove-adj-nodes (:in g) nodes outs disj)))))
  (remove-edges* [g edges]
                 (reduce
                   (fn [g [n1 n2]]
                     (-> g
                         (update-in [:adj n1] dissoc n2)
                         (update-in [:in n2] disj n1)))
                   g edges))
  (remove-all [g]
              (assoc g :nodeset #{} :adj {} :in {}))



  Digraph
  (predecessors
    ([g] (default-digraph-predecessors g))
    ([g node] (default-digraph-predecessors g node)))
  (in-degree [g node] (default-digraph-in-degree g node))
  (in-edges
    ([g] (default-digraph-in-edges g))
    ([g node] (default-digraph-in-edges g node)))
  (transpose [g] (reduce (fn [tg [n1 n2]]
                           (add-edges* tg [[n2 n1 (weight g n1 n2)]]))
                         (assoc g :adj {} :in {})
                         (edges g)))
  WeightedGraph
  (weight
    ([g] (default-weighted-weight g))
    ([g e] (default-weighted-weight g e))
    ([g n1 n2] (default-weighted-weight g n1 n2))))

;;;
;;; FlyGraph -- a read-only, ad-hoc graph which uses provided functions to
;;; return values for nodes, edges, etc. Members which are not functions get
;;; returned as-is. Edges can be inferred if nodes and successors are provided.
;;; Nodes and edges can be inferred if successors and start are provided.
;;;

(defn- call-or-return [f & args]
  (if (or (fn? f)
          (and #+clj (instance? clojure.lang.IFn f) #+cljs (satisfies? Fn f)
               (seq args)))
    (apply f args)
    f))

(def ^{:private true} default-flygraph-graph-impl
  {:nodes (fn [g]
            (if (or (:fnodes g) (not (:start g)))
              (call-or-return (:fnodes g))
              (bf-traverse (successors g) (:start g))))
   :edges (fn [g]
            (if (:fedges g)
              (call-or-return (:fedges g))
              (for [n (nodes g)
                    nbr (successors g n)]
                [n nbr])))
   :successors (fn
                 ([g] (partial successors g))
                 ([g node] (call-or-return (:fsuccessors g) node)))
   :out-degree (fn [g node]
                 (count (successors g node)))
   :out-edges (get-in default-graph-impls [:all :out-edges])})

(defn default-flygraph-graph-nodes [g]
  (if (or (:fnodes g) (not (:start g)))
    (call-or-return (:fnodes g))
    (bf-traverse (successors g) (:start g))))
(defn default-flygraph-graph-edges [g]
  (if (:fedges g)
    (call-or-return (:fedges g))
    (for [n (nodes g)
          nbr (successors g n)]
      [n nbr])))
(defn default-flygraph-graph-successors
  ([g] (partial successors g))
  ([g node] (call-or-return (:fsuccessors g) node)))
(defn default-flygraph-graph-out-degree [g node]
  (count (successors g node)))
(defn default-flygraph-graph-out-edges
  ([g] (partial out-edges g))
  ([g node] (for [n2 (successors g node)] [node n2])))



(def ^{:private true} default-flygraph-digraph-impl
  {:predecessors (fn [g node] (call-or-return (:fpredecessors g) node))
   :in-degree (fn [g node] (count (predecessors g node)))
   :in-edges (get-in default-digraph-impl [:all :in-edges])})

(defn default-flygraph-digraph-predecessors [g node]
  (call-or-return (:fpredecessors g) node))
(defn default-flygraph-digraph-in-degree [g node]
  (count (predecessors g node)))
(defn default-flygraph-digraph-in-edges
  ([g] (partial in-edges g))
  ([g node] (for [n2 (predecessors g node)] [n2 node])))



(def ^{:private true} default-flygraph-weighted-impl
  {:weight (fn
             ([g] (partial weight g))
             ([g e] (weight g (src e) (dest e)))
             ([g n1 n2] (call-or-return (:fweight g) n1 n2)))})

(defn default-flygraph-weighted-weight
  ([g] (partial weight g))
  ([g e] (weight g (src e) (dest e)))
  ([g n1 n2] (call-or-return (:fweight g) n1 n2)))

(defrecord FlyGraph [fnodes fedges fsuccessors start])
(defrecord FlyDigraph [fnodes fedges fsuccessors fpredecessors start])
(defrecord WeightedFlyGraph [fnodes fedges fsuccessors fweight start])
(defrecord WeightedFlyDigraph
           [fnodes fedges fsuccessors fpredecessors fweight start])

;; Deprecate the flygraphs?  Instead provide interfaces on algorithms to
;; run the algorithm on

(extend-type FlyGraph
  Graph
  (nodes [g] (default-flygraph-graph-nodes g))
  (edges [g] (default-flygraph-graph-edges g))
  (out-degree [g node] (default-flygraph-graph-out-degree g node))
  (out-edges [g node] (default-flygraph-graph-out-edges g node))
  (successors ([g] (default-flygraph-graph-successors g))
              ([g node] (default-flygraph-graph-successors g node))))

(extend-type FlyDigraph
  Graph
  (nodes [g] (default-flygraph-graph-nodes g))
  (edges [g] (default-flygraph-graph-edges g))
  (out-degree [g node] (default-flygraph-graph-out-degree g node))
  (out-edges [g node] (default-flygraph-graph-out-edges g node))
  (successors ([g] (default-flygraph-graph-successors g))
    ([g node] (default-flygraph-graph-successors g node)))
  Digraph
  (predecessors [g node] (default-flygraph-digraph-predecessors g node))
  (in-degree [g node] (default-flygraph-digraph-in-degree g node))
  (in-edges
    ([g] (default-flygraph-digraph-in-edges g))
    ([g node] (default-flygraph-digraph-in-edges g node)))
  (transpose [g] (reduce (fn [tg [n1 n2]]
                           (add-edges* tg [[n2 n1 (weight g n1 n2)]]))
                         (assoc g :adj {} :in {})
                         (edges g))))

(extend-type WeightedFlyGraph
  Graph
  (nodes [g] (default-flygraph-graph-nodes g))
  (edges [g] (default-flygraph-graph-edges g))
  (out-degree [g node] (default-flygraph-graph-out-degree g node))
  (out-edges [g node] (default-flygraph-graph-out-edges g node))
  (successors ([g] (default-flygraph-graph-successors g))
    ([g node] (default-flygraph-graph-successors g node)))
  WeightedGraph
  (weight
    ([g] (default-flygraph-weighted-weight g))
    ([g e] (default-flygraph-weighted-weight g e))
    ([g n1 n2] (default-flygraph-weighted-weight g n1 n2))))

(extend-type WeightedFlyDigraph
  Graph
  (nodes [g] (default-flygraph-graph-nodes g))
  (edges [g] (default-flygraph-graph-edges g))
  (out-degree [g node] (default-flygraph-graph-out-degree g node))
  (out-edges [g node] (default-flygraph-graph-out-edges g node))
  (successors ([g] (default-flygraph-graph-successors g))
    ([g node] (default-flygraph-graph-successors g node)))
  Digraph
  (predecessors [g node] (default-flygraph-digraph-predecessors g node))
  (in-degree [g node] (default-flygraph-digraph-in-degree g node))
  (in-edges
    ([g] (default-flygraph-digraph-in-edges g))
    ([g node] (default-flygraph-digraph-in-edges g node)))
  (transpose [g] (reduce (fn [tg [n1 n2]]
                           (add-edges* tg [[n2 n1 (weight g n1 n2)]]))
                         (assoc g :adj {} :in {})
                         (edges g)))
  WeightedGraph
  (weight
    ([g] (default-flygraph-weighted-weight g))
    ([g e] (default-flygraph-weighted-weight g e))
    ([g n1 n2] (default-flygraph-weighted-weight g n1 n2))))

;;;
;;; Utility functions and constructors
;;;

;; TODO: make this work with read-only graphs?
;; Could also gain speed being impl-specific
(defn subgraph
  "Return a graph without all but the given nodes"
  [g ns]
  (remove-nodes* g (filter (complement (set ns)) (nodes g))))

(defn add-path
  "Add a path of edges connecting the given nodes in order"
  [g & nodes]
  (add-edges* g (partition 2 1 nodes)))

(defn add-cycle
  "Add a cycle of edges connecting the given nodes in order"
  [g & nodes]
  (add-edges* g (partition 2 1 (concat nodes [(first nodes)]))))

(defn graph?
  "Return true if g satisfies the Graph protocol"
  [g]
  (satisfies? Graph g))

(defn directed?
  "Return true if g satisfies the Digraph protocol"
  [g]
  (satisfies? Digraph g))

(defn weighted?
  "Return true if g satisfies the WeightedGraph protocol"
  [g]
  (satisfies? WeightedGraph g))

(defn editable?
  "Return true if g satisfies the EditableGraph protocol"
  [g]
  (satisfies? EditableGraph g))

(defn build-graph
  "Builds up a graph (i.e. adds edges and nodes) from any combination of
  other graphs, adjacency maps, edges, or nodes."
  [g & inits]
  (letfn [(build [g init]
                 (cond
                   ;; graph
                   (graph? init)
                   (if (and (weighted? g) (weighted? init))
                     (reduce add-edges
                             (add-nodes* g (nodes init))
                             (for [[n1 n2] (edges init)]
                               [n1 n2 (weight init n1 n2)]))
                     (-> g
                         (add-nodes* (nodes init))
                         (add-edges* (edges init))))
                   ;; adacency map
                   (map? init)
                   (let [es (if (map? (val (first init)))
                              (for [[n nbrs] init
                                    [nbr wt] nbrs]
                                [n nbr wt])
                              (for [[n nbrs] init
                                    nbr nbrs]
                                [n nbr]))]
                     (-> g
                         (add-nodes* (keys init))
                         (add-edges* es)))
                   ;; edge
                   (sequential? init) (add-edges g init)
                   ;; node
                   :else (add-nodes g init)))]
    (reduce build g inits)))

(defn graph
  "Create an unweighted, undirected graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableGraph. #{} {}) inits))

(defn digraph
  "Create an unweighted, directed graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableDigraph. #{} {} {}) inits))

(defn weighted-graph
  "Create an weighted, undirected graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableWeightedGraph. #{} {}) inits))

(defn weighted-digraph
  "Create an weighted, directed graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableWeightedDigraph. #{} {} {}) inits))

(defn fly-graph
  "Create a read-only, ad-hoc graph which uses the provided functions
  to return values for nodes, edges, etc. If any members are not functions,
  they will be returned as-is. Edges can be inferred if nodes and
  successors are provided. Nodes and edges can be inferred if successors and
  start are provided."
  [& {:keys [nodes edges successors predecessors weight start]}]
  (cond
    (and predecessors weight)
    (WeightedFlyDigraph. nodes edges successors predecessors weight start)
    predecessors
    (FlyDigraph. nodes edges successors predecessors start)
    weight
    (WeightedFlyGraph. nodes edges successors weight start)
    :else
    (FlyGraph. nodes edges successors start)))

