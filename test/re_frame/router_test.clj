(ns re-frame.router-test
  (:require [clojure.test :refer :all]
            [re-frame.frame :as frame]
            [re-frame.db :as db]))

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

(defn register
  []
  (frame/reg-event-db
    *frame*
    ::test
    (fn [db [_ i]]
      (update db ::test (fnil conj []) i)))

  (frame/reg-fx
    *frame*
    ::promise
    (fn [{:keys [p val]}]
      (deliver p val)))

  (frame/reg-event-fx
    *frame*
    ::sentinel
    (fn [cofx [_ p val]]
      {::promise {:p p :val val}})))

(use-fixtures :once register)

(deftest dispatching-race-condition-469-test
  ;; Checks for day8/re-frame#469
  (let [p (promise)]
    (is (nil? (dotimes [i 1000]
                (frame/dispatch *frame* [::test i]))))
    (is (nil? (frame/dispatch *frame* [::sentinel p ::done])))
    (let [val (deref p 1000 ::timed-out)]
      (is (= ::done val)))
    (is (= (::test @db/app-db)
           (range 1000)))))
