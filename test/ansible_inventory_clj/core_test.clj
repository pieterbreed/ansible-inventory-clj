(ns ansible-inventory-clj.core-test
  (:require [clojure.test :refer :all]
            [ansible-inventory-clj.core :refer :all]
            [clojure.test.check.generators :as tcgen]
            [clojure.test.check.properties :as tcprop]
            [clojure.test.check.clojure-test :as tct]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            ))

(deftest empty-inventory-test
  (testing "That the empty inventory is in fact a valid inventory."
    (is (s/valid? :ansible-inventory-clj.core/inventory
                  ansible-inventory-clj.core/empty-inventory))))


;; ----------------------------------------

(let [target ["username" "hostname" 22]]
  (tct/defspec add-target-merges-vars
    100
    (tcprop/for-all
     [target-vars (tcgen/map tcgen/string-alphanumeric
                             tcgen/string-alphanumeric)]
     (let [;; expected value
           ;; adds all of the "variables" at once
           exp (-> empty-inventory
                   (add-target target target-vars))

           ;; test value
           ;; add the variables one-at-a-time using the loop
           tval (loop [keys (keys target-vars)
                       inv (add-target empty-inventory target {})]
                  
                  (if (or (nil? keys)
                          (empty? keys))
                    inv
                    (let [key (first keys)
                          val (get target-vars key)]
                      (recur (rest keys)
                             (add-target inv target
                                         {key val})))))]

       ;; check that the result is the same
       (= exp tval)))))

;; ----------------------------------------

(tct/defspec add-group-merges-vars
  100
  (tcprop/for-all
   [vars (tcgen/map tcgen/string-alphanumeric
                    tcgen/string-alphanumeric)]
   (let [;; expected value
         ;; adds all of the "variables" at once
         exp (-> empty-inventory
                 (add-group "group-id" vars))

         ;; test value
         ;; add the variables one-at-a-time using the loop
         tval (loop [keys (keys vars)
                     inv (add-group empty-inventory "group-id" {})]
                
                (if (or (nil? keys)
                        (empty? keys))
                  inv
                  (let [key (first keys)
                        val (get vars key)]
                    (recur (rest keys)
                           (add-group inv "group-id"
                                      {key val})))))]

     ;; check that the result is the same
     (= exp tval))))


;; ----------------------------------------

(deftest run-test-check-tests
  (testing "That the auto-generated quick-check style tests pass."
    (let [summary (stest/summarize-results
                   (stest/check
                    (stest/enumerate-namespace 'ansible-inventory-clj.core)
                    {:clojure.spec.test.check/opts {:num-tests 50}}))]
      (is (= (:total summary)
             (:check-passed summary))
          ))))
