(ns re-frame.subs-test
  (:require [clojure.test  :refer :all]
            [re-frame.frame :as frame]))

(def ^:dynamic *frame*)

(defn set-frame
  [f]
  (binding [*frame* (frame/make-frame)]
    (f)))

(use-fixtures :once set-frame)

(defn fixture-re-frame
  [f]
  (let [restore-re-frame (frame/make-restore-fn *frame*)]
    (f)
    (restore-re-frame)))

(use-fixtures :each fixture-re-frame)

(deftest test-reg-sub-clj-repl
  (frame/reg-sub
   *frame*
   :a-sub
   (fn [db _] (:a db)))

  (frame/reg-sub
   *frame*
   :b-sub
   (fn [db _] (:b db)))

  (frame/reg-sub
   *frame*
   :a-b-sub
   (fn [_ _]
     [(frame/subscribe *frame* [:a-sub])
      (frame/subscribe *frame* [:b-sub])])
   (fn [[a b] _]
     {:a a :b b}))

  (let [test-sub (frame/subscribe *frame* [:a-b-sub])]
    (reset! (:app-db *frame*) {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub))
    (swap! (:app-db *frame*) assoc :b 3)
    (is (= {:a 1 :b 3} @test-sub))))
