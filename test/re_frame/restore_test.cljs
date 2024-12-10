(ns re-frame.restore-test
  (:require [cljs.test :refer-macros [is deftest async use-fixtures testing]]
            [re-frame.frame :as frame :refer [make-restore-fn reg-sub subscribe]]
            [re-frame.subs :as subs]))

;; TODO: future tests in this area could check DB state and registrations are being correctly restored.

(def ^:dynamic *frame*)

(defn set-frame
  [f]
  (binding [*frame* (frame/make-frame)]
    (f)))

(use-fixtures :once set-frame)


(defn clear-all-handlers!
  [f]
  (subs/clear-all-handlers! *frame*)
  (f))

(use-fixtures :each clear-all-handlers!)

(defn one? [x] (= 1 x))
(defn two? [x] (= 2 x))

(defn register-test-subs []
  (reg-sub
   *frame*
   :test-sub
   (fn [db ev]
     (:test-sub db)))

  (reg-sub
   *frame*
   :test-sub2
   (fn [db ev]
     (:test-sub2 db))))

(deftest make-restore-fn-test
  (testing "no existing subs, then making one subscription"
    (register-test-subs)
    (let [original-subs @(:subs-cache *frame*)
          restore-fn    (make-restore-fn *frame*)]
      (is (zero? (count original-subs)))
      @(subscribe *frame* [:test-sub])
      (is (one? (count @(:subs-cache *frame*))))
      (is (contains? @(:subs-cache *frame*) [[:test-sub] []]))
      (restore-fn)
      (is (zero? (count @(:subs-cache *frame*)))))))

(deftest make-restore-fn-test2
  (testing "existing subs, making more subscriptions"
    (register-test-subs)
    @(subscribe *frame* [:test-sub])
    (let [original-subs @(:subs-cache *frame*)
          restore-fn    (make-restore-fn *frame*)]
      (is (one? (count original-subs)))
      @(subscribe *frame* [:test-sub2])
      (is (contains? @(:subs-cache *frame*) [[:test-sub2] []]))
      (is (two? (count @(:subs-cache *frame*))))
      (restore-fn)
      (is (not (contains? @(:subs-cache *frame*) [[:test-sub2] []])))
      (is (one? (count @(:subs-cache *frame*)))))))

(deftest make-restore-fn-test3
  (testing "existing subs, making more subscriptions with different params on same subscriptions"
    (register-test-subs)
    @(subscribe *frame* [:test-sub])
    (let [original-subs @(:subs-cache *frame*)
          restore-fn    (make-restore-fn *frame*)]
      (is (one? (count original-subs)))
      @(subscribe *frame* [:test-sub :extra :params])
      (is (two? (count @(:subs-cache *frame*))))
      (restore-fn)
      (is (one? (count @(:subs-cache *frame*)))))))

(deftest nested-restores
  (testing "running nested restores"
    (register-test-subs)
    (let [restore-fn-1 (make-restore-fn *frame*)
          _            @(subscribe *frame* [:test-sub])
          _            (is (one? (count @(:subs-cache *frame*))))
          restore-fn-2 (make-restore-fn *frame*)]
      @(subscribe *frame* [:test-sub2])
      (is (two? (count @(:subs-cache *frame*))))
      (restore-fn-2)
      (is (one? (count @(:subs-cache *frame*))))
      (restore-fn-1)
      (is (zero? (count @(:subs-cache *frame*)))))))
