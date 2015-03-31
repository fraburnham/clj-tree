(ns clj-tree.core)

;stores all the nodes in a vector
;this should allow the prevention of circular storage
;when storing a parent the data grows a whole mess with maps
;so now the parent node will be an index in this vec, new nodes will be
;conj'd to the node vec
;this may need to be an array, so that it can be updated in place...
;default tree size of 10 should be grown when the number of nodes
;inserted is 70% full and cut down when it is less than 70% full
(def node-array (atom (object-array 10)))
(def num-nodes (atom 0))

;rethink this, what about an object-array full of atoms?

(defstruct node :parent :children :data :index)

(defn- update-node! [index key val]
  (swap! (aget @node-array index) assoc key val))

(defn- grow-array! [arr]
  (let [len (* 2 (alength arr))
        ret (object-array len)]
    (doseq [i (range (alength arr))]
      (aset ret i (aget arr i)))
    ret))

(defn new-node! [parent children data]
  (if (> @num-nodes (* 0.7 (alength @node-array)))
    (swap! node-array grow-array!))
  (let [i @num-nodes]
    (swap! num-nodes inc)
    (aset @node-array i (atom (struct node parent children data i)))))

(defn add-child! [{children :children i :index}
                  {child-index :index}]
  (update-node! i :children (conj children child-index))
  (update-node! child-index :parent i))

(defn path-to-root [node])
