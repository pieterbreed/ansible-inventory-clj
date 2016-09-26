(ns ansible-inventory-clj.core-test
  (:require [clojure.test :refer :all]
            [ansible-inventory-clj.core :refer :all]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]
            [clojure.spec :as s]
            [clojure.test :refer :all]))

(deftest empty-inventory-test
  (testing "That the empty inventory is in fact a valid inventory."
    (is (s/valid? :ansible-inventory-clj.core/inventory
                  ansible-inventory-clj.core/empty-inventory))))
