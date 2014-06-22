(ns ^{:doc "Graph attribute protocol and implementations for records from
loom.graph. Common uses for attributes include labels and styling (color,
thickness, etc)."
      :author "Justin Kramer"}
    loom.attr
  #+clj (:require [loom.graph :refer [directed? nodes edges src dest has-node?]
                   :as graph])

  #+cljs (:require [loom.graph :refer [directed? nodes edges src dest has-node?
                                       BasicEditableGraph BasicEditableDigraph
                                       BasicEditableWeightedGraph BasicEditableWeightedDigraph
                                       FlyGraph FlyDigraph WeightedFlyGraph WeightedFlyDigraph]
                    :as graph])
  #+clj (:import [loom.graph BasicEditableGraph BasicEditableDigraph
                  BasicEditableWeightedGraph BasicEditableWeightedDigraph
                  FlyGraph FlyDigraph WeightedFlyGraph WeightedFlyDigraph]))

(defprotocol AttrGraph
  (add-attr [g node-or-edge k v] [g n1 n2 k v] "Add an attribute to node or edge")
  (remove-attr [g node-or-edge k] [g n1 n2 k] "Remove an attribute from a node or edge")
  (attr [g node-or-edge k] [g n1 n2 k] "Return the attribute on a node or edge")
  (attrs [g node-or-edge] [g n1 n2] "Return all attributes on a node or edge"))

(def default-attr-graph-impl
  {:add-attr (fn
               ([g node-or-edge k v]
                (if (has-node? g node-or-edge)
                  (assoc-in g [:attrs node-or-edge k] v)
                  (add-attr g (src node-or-edge) (dest node-or-edge) k v)))
               ([g n1 n2 k v]
                (let [g (assoc-in g [:attrs n1 ::edge-attrs n2 k] v)
                      g (if (directed? g) g
                                          (assoc-in g [:attrs n2 ::edge-attrs n1 k] v))]
                  g)))
   :remove-attr (fn
                  ([g node-or-edge k]
                   (if (has-node? g node-or-edge)
                     (update-in g [:attrs node-or-edge] dissoc k)
                     (remove-attr g (src node-or-edge) (dest node-or-edge) k)))
                  ([g n1 n2 k]
                   (update-in g [:attrs n1 ::edge-attrs n2] dissoc k)))
   :attr (fn
           ([g node-or-edge k]
            (if (has-node? g node-or-edge)
              (get-in g [:attrs node-or-edge k])
              (attr g (src node-or-edge) (dest node-or-edge) k)))
           ([g n1 n2 k]
            (get-in g [:attrs n1 ::edge-attrs n2 k])))
   :attrs (fn
            ([g node-or-edge]
             (if (has-node? g node-or-edge)
               (dissoc (get-in g [:attrs node-or-edge]) ::edge-attrs)
               (attrs g (src node-or-edge) (dest node-or-edge))))
            ([g n1 n2]
             (let [attributes (get-in g [:attrs n1 ::edge-attrs n2])]
               (when (seq attributes) attributes))))})

(defn default-graph-add-attr
  ([g node-or-edge k v]
   (if (has-node? g node-or-edge)
     (assoc-in g [:attrs node-or-edge k] v)
     (add-attr g (src node-or-edge) (dest node-or-edge) k v)))
  ([g n1 n2 k v]
   (let [g (assoc-in g [:attrs n1 ::edge-attrs n2 k] v)
         g (if (directed? g) g
                             (assoc-in g [:attrs n2 ::edge-attrs n1 k] v))]
     g)))
(defn default-graph-remove-attr
  ([g node-or-edge k]
   (if (has-node? g node-or-edge)
     (update-in g [:attrs node-or-edge] dissoc k)
     (remove-attr g (src node-or-edge) (dest node-or-edge) k)))
  ([g n1 n2 k]
   (update-in g [:attrs n1 ::edge-attrs n2] dissoc k)))
(defn default-graph-attr
  ([g node-or-edge k]
   (if (has-node? g node-or-edge)
     (get-in g [:attrs node-or-edge k])
     (attr g (src node-or-edge) (dest node-or-edge) k)))
  ([g n1 n2 k]
   (get-in g [:attrs n1 ::edge-attrs n2 k])))
(defn default-graph-attrs
  ([g node-or-edge]
   (if (has-node? g node-or-edge)
     (dissoc (get-in g [:attrs node-or-edge]) ::edge-attrs)
     (attrs g (src node-or-edge) (dest node-or-edge))))
  ([g n1 n2]
   (let [attributes (get-in g [:attrs n1 ::edge-attrs n2])]
     (when (seq attributes) attributes))))

(extend-type BasicEditableGraph
  AttrGraph
  (add-attr
    ([g node-or-edge k v] (default-graph-add-attr g node-or-edge k v))
    ([g n1 n2 k v] (default-graph-add-attr g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (default-graph-remove-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-remove-attr g n1 n2 k)))
  (attr
    ([g node-or-edge k] (default-graph-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-attr g n1 n2 k)))
  (attrs
    ([g node-or-edge] (default-graph-attrs g node-or-edge))
    ([g n1 n2] (default-graph-attrs g n1 n2))))

(extend-type BasicEditableDigraph
  AttrGraph
  (add-attr
    ([g node-or-edge k v] (default-graph-add-attr g node-or-edge k v))
    ([g n1 n2 k v] (default-graph-add-attr g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (default-graph-remove-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-remove-attr g n1 n2 k)))
  (attr
    ([g node-or-edge k] (default-graph-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-attr g n1 n2 k)))
  (attrs
    ([g node-or-edge] (default-graph-attrs g node-or-edge))
    ([g n1 n2] (default-graph-attrs g n1 n2))))

(extend-type BasicEditableWeightedGraph
  AttrGraph
  (add-attr
    ([g node-or-edge k v] (default-graph-add-attr g node-or-edge k v))
    ([g n1 n2 k v] (default-graph-add-attr g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (default-graph-remove-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-remove-attr g n1 n2 k)))
  (attr
    ([g node-or-edge k] (default-graph-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-attr g n1 n2 k)))
  (attrs
    ([g node-or-edge] (default-graph-attrs g node-or-edge))
    ([g n1 n2] (default-graph-attrs g n1 n2))))

(extend-type BasicEditableWeightedDigraph
  AttrGraph
  (add-attr
    ([g node-or-edge k v] (default-graph-add-attr g node-or-edge k v))
    ([g n1 n2 k v] (default-graph-add-attr g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (default-graph-remove-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-remove-attr g n1 n2 k)))
  (attr
    ([g node-or-edge k] (default-graph-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-attr g n1 n2 k)))
  (attrs
    ([g node-or-edge] (default-graph-attrs g node-or-edge))
    ([g n1 n2] (default-graph-attrs g n1 n2))))

(extend-type FlyGraph
  AttrGraph
  (add-attr
    ([g node-or-edge k v] (default-graph-add-attr g node-or-edge k v))
    ([g n1 n2 k v] (default-graph-add-attr g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (default-graph-remove-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-remove-attr g n1 n2 k)))
  (attr
    ([g node-or-edge k] (default-graph-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-attr g n1 n2 k)))
  (attrs
    ([g node-or-edge] (default-graph-attrs g node-or-edge))
    ([g n1 n2] (default-graph-attrs g n1 n2))))

(extend-type FlyDigraph
  AttrGraph
  (add-attr
    ([g node-or-edge k v] (default-graph-add-attr g node-or-edge k v))
    ([g n1 n2 k v] (default-graph-add-attr g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (default-graph-remove-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-remove-attr g n1 n2 k)))
  (attr
    ([g node-or-edge k] (default-graph-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-attr g n1 n2 k)))
  (attrs
    ([g node-or-edge] (default-graph-attrs g node-or-edge))
    ([g n1 n2] (default-graph-attrs g n1 n2))))

(extend-type WeightedFlyGraph
  AttrGraph
  (add-attr
    ([g node-or-edge k v] (default-graph-add-attr g node-or-edge k v))
    ([g n1 n2 k v] (default-graph-add-attr g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (default-graph-remove-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-remove-attr g n1 n2 k)))
  (attr
    ([g node-or-edge k] (default-graph-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-attr g n1 n2 k)))
  (attrs
    ([g node-or-edge] (default-graph-attrs g node-or-edge))
    ([g n1 n2] (default-graph-attrs g n1 n2))))

(extend-type WeightedFlyDigraph
  AttrGraph
  (add-attr
    ([g node-or-edge k v] (default-graph-add-attr g node-or-edge k v))
    ([g n1 n2 k v] (default-graph-add-attr g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (default-graph-remove-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-remove-attr g n1 n2 k)))
  (attr
    ([g node-or-edge k] (default-graph-attr g node-or-edge k))
    ([g n1 n2 k] (default-graph-attr g n1 n2 k)))
  (attrs
    ([g node-or-edge] (default-graph-attrs g node-or-edge))
    ([g n1 n2] (default-graph-attrs g n1 n2))))

(defn attr?
  "Return true if g satisfies AttrGraph"
  [g]
  (satisfies? AttrGraph g))

(defn add-attr-to-nodes
  "Adds an attribute to the given nodes"
  [g k v nodes]
  (reduce
    (fn [g n]
      (add-attr g n k v))
    g nodes))

(defn add-attr-to-edges
  "Adds an attribute to the given nodes"
  [g k v edges]
  (reduce
    (fn [g [n1 n2]]
      (add-attr g n1 n2 k v))
    g edges))

(defn add-attr-to-all
  "Adds an attribute to all nodes and edges"
  [g k v]
  (-> g
      (add-attr-to-nodes k v (nodes g))
      (add-attr-to-edges k v (edges g))))

(defn add-attrs-to-all
  "Adds attributes to all nodes and edges"
  [g & kvs]
  (reduce
    (fn [g [k v]]
      (-> g
          (add-attr-to-nodes k v (nodes g))
          (add-attr-to-edges k v (edges g))))
    g (partition 2 1 kvs)))


(defn hilite
  "Adds a red :color attribute to a node or edge"
  ([g node]
   (-> g
       (add-attr node :color :red)
       (add-attr node :fontcolor :red)
       (add-attr node :fillcolor "#ffeeee")
       (add-attr node :style "filled,bold")))
  ([g n1 n2]
   (-> g
       (add-attr n1 n2 :color :red)
       (add-attr n1 n2 :fontcolor :red)
       (add-attr n1 n2 :style :bold))))

(defn hilite-path
  "Hilites nodes and edges along a path"
  [g path]
  (reduce
    (fn [g [n1 n2]]
      (-> g
          (hilite n1)
          (hilite n2)
          (hilite n1 n2)))
    g (partition 2 1 path)))
