(ns clj-tree.core-test
  (:require [clojure.test :refer :all]
            [clj-tree.core :refer :all]))

(defn- simple-tree
  "A simple tree for tests"
  []
  (new-node! nil nil "root")
  (new-node! 0 nil "A")
  (new-node! 1 nil "a")
  (new-node! 0 nil "B")
  (new-node! 3 nil "b"))

(deftest test-new-node!
  (testing "clj-tree.core/new-node!"
    (let [node (new-node! nil nil "root")]
      (is (number? (:index node)))
      (is (= node (node (:index node)))))))


(deftest test-add-child!
  (testing "clj-tree.core/add-child!"
    (fresh-tree)
    (new-node! nil nil "root")
    (let [child (new-node! 0 nil "child")
          parent (node 0)]
      (is (= (:parent child) 0))
      (is (= (:children parent) [1])))))

(deftest test-node
  (testing "node using simple-tree"
    (fresh-tree)
    (simple-tree)
    (is (= (:parent (node 0)) nil))
    (is (= (:parent (node 1)) 0))
    (is (= (:parent (node 2)) 1))
    (is (= (:parent (node 3)) 0))
    (is (= (:parent (node 4)) 3))))

(deftest test-path-to-root
  (testing "path-to-root using simple-tree"
    (fresh-tree)
    (simple-tree)
    (is (= [4 3 0] (path-to-root 4)))
    (is (= [3 0] (path-to-root 3)))
    (is (= [2 1 0] (path-to-root 2)))
    (is (= [1 0] (path-to-root 1)))
    (is (= [0] (path-to-root 0)))))

(deftest test-dfs
  (testing "dfs using simple tree"
    (fresh-tree)
    (simple-tree)
    (is (= (:data (dfs 0 "a")) "a"))
    (is (= (:data (dfs 0 "root")) "root"))
    (is (= (:data (dfs 0 "A")) "A"))
    (is (= (:data (dfs 0 "B")) "B"))
    (is (= (:data (dfs 0 "b")) "b"))))
