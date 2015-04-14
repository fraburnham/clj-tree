(ns clj-tree.core)

;don't want anyone else to mess with the mutables
(defonce ^:private node-array (atom (object-array 10)))
(defonce ^:private num-nodes (atom 0))

(defstruct node :parent :children :data :index)

(defn- update-node! [index key val]
  (swap! (aget @node-array index) assoc key val))

(defn- resize-array! [arr]
  (let [len (* 2 (alength arr))
        ret (object-array len)]
    (doseq [i (range (alength arr))]
      (aset ret i (aget arr i)))
    ret))

(defn node-by-index [index]
  (if (nil? index)
    nil
    (deref (aget @node-array index))))

(defn add-child! [parent-index child-index]
  (let [{children :children} (node-by-index parent-index)]
    (update-node! parent-index :children (conj children child-index))
    (update-node! child-index :parent parent-index)))

;index as nil for no parent. parent 0 is a valid parent
(defn new-node! [parent-index child-indices data]
  (if (> @num-nodes (* 0.7 (alength @node-array)))
    (swap! node-array resize-array!))
  (deref
    (let [i @num-nodes]
      (swap! num-nodes inc)
      (aset @node-array i (atom (struct node parent-index child-indices data i)))
      (if (not (nil? parent-index))
        (add-child! parent-index i))
      (aget @node-array i))))

(defn path-to-root [node]
  (loop [n node
         r [(:index n)]]
    (let [parent (node-by-index (:parent n))]
      (if (nil? parent)
        r
        (recur parent (conj r (:index parent)))))))

(defn dfs [root-index find-data]
  (loop [node-stack [root-index]
         r nil]
    (if (empty? node-stack)
      r
      (let [node (node-by-index (first node-stack))
            node-stack (rest node-stack)]
        (if (= find-data (:data node))
          (recur [] node)
          (recur (concat (:children node) node-stack) nil))))))

(defn print-tree [root-index]
  (loop [node-stack [root-index]]
    (if (not-empty node-stack)
      (let [node (node-by-index (first node-stack))
            node-stack (rest node-stack)]
        (println "Child:" (:index node) "Parent:" (:parent node))
        (recur (concat (:children node) node-stack))))))
