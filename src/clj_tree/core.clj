(ns clj-tree.core)

;don't want anyone else to mess with the mutables
(defonce ^:private node-array (atom (object-array 10)))
(defonce ^:private num-nodes (atom 0))

(defstruct node :parent :children :data :index)

(defn fresh-tree
  "Resets the private tree values.
  Mostly for testing."
  []
  (reset! node-array (object-array 10))
  (reset! num-nodes 0))

(defn- update-node!
  "Update node at index's key with val"
  [index key val]
  (swap! (aget @node-array index) assoc key val))

(defn- resize-array!
  "Resize the node-array and keep existing nodes"
  [arr]
  (let [len (* 2 (alength arr))
        ret (object-array len)]
    (doseq [i (range (alength arr))]
      (aset ret i (aget arr i)))
    ret))

(defn node
  "Given an index return a node"
  [index]
  (if (nil? index)
    nil
    (deref (aget @node-array index))))

(defn add-child!
  "Mutate a parent's :children to include a new child"
  [parent-index child-index]
  (let [{children :children} (node parent-index)]
    (update-node! parent-index :children (conj children child-index))
    (update-node! child-index :parent parent-index)))

;index as nil for no parent. parent 0 is a valid parent
(defn new-node!
  "Create a new node in the tree. A parent index of nil
  indicates that this is a root node."
  [parent-index child-indices data]
  (if (> @num-nodes (* 0.7 (alength @node-array)))
    (swap! node-array resize-array!))
  (deref
    (let [i @num-nodes]
      (swap! num-nodes inc)
      (aset @node-array i (atom (struct node parent-index child-indices data i)))
      (if (not (nil? parent-index))
        (add-child! parent-index i))
      (aget @node-array i))))

(defn path-to-root
  "Return the indices from the given node to the root node"
  [node]
  (loop [n (node node)
         r [(:index n)]]
    (let [parent (node (:parent n))]
      (if (nil? parent)
        r
        (recur parent (conj r (:index parent)))))))

(defn dfs
  "Depth first search from root-index looking for find-data"
  [root-index find-data]
  (loop [node-stack [root-index]
         r nil]
    (if (empty? node-stack)
      r
      (let [node (node (first node-stack))
            node-stack (rest node-stack)]
        (if (= find-data (:data node))
          (recur [] node)
          (recur (concat (:children node) node-stack) nil))))))

(defn print-tree
  "Print the tree or sub-tree starting from root-index"
  [root-index]
  (loop [node-stack [root-index]]
    (if (not-empty node-stack)
      (let [node (node (first node-stack))
            node-stack (rest node-stack)]
        (println "Child:" (:data node)
                 "Parent:" (:data (node (:parent node))))
        (recur (concat (:children node) node-stack))))))
