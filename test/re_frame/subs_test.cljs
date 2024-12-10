(ns re-frame.subs-test
  (:require [cljs.test         :as test :refer-macros [is deftest testing]]
            [reagent.ratom     :as r :refer-macros [reaction]]
            [re-frame.subs     :as subs]
            [re-frame.db       :as db]
            [re-frame.core     :as re-frame]))

(defn reg-sub-raw-compat
  [query-id handler-fn]
  (re-frame/reg-sub-raw
    query-id
    (fn
      ([frame query-v]
       (handler-fn (:app-db frame) query-v))
      ([frame query-v dyn-v]
       (handler-fn (:app-db frame) query-v dyn-v)))))

(test/use-fixtures :each {:before (fn [] (subs/clear-all-handlers! db/default-frame))})

;=====test basic subscriptions  ======

(deftest test-reg-sub
  (reg-sub-raw-compat
   :test-sub
   (fn [db [_]] (reaction (deref db))))

  (let [test-sub (re-frame/subscribe [:test-sub])]
    (is (= @db/app-db @test-sub))
    (reset! db/app-db 1)
    (is (= 1 @test-sub))))

(deftest test-chained-subs
  (reg-sub-raw-compat
   :a-sub
   (fn [db [_]] (reaction (:a @db))))

  (reg-sub-raw-compat
   :b-sub
   (fn [db [_]] (reaction (:b @db))))

  (reg-sub-raw-compat
   :a-b-sub
   (fn [db [_]]
     (let [a (re-frame/subscribe [:a-sub])
           b (re-frame/subscribe [:b-sub])]
       (reaction {:a @a :b @b}))))

  (let [test-sub (re-frame/subscribe [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub))
    (swap! db/app-db assoc :b 3)
    (is (= {:a 1 :b 3} @test-sub))))

(deftest test-sub-parameters
  (reg-sub-raw-compat
   :test-sub
   (fn [db [_ b]] (reaction [(:a @db) b])))

  (let [test-sub (re-frame/subscribe [:test-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= [1 :c] @test-sub))))

(deftest test-sub-chained-parameters
  (reg-sub-raw-compat
   :a-sub
   (fn [db [_ a]] (reaction [(:a @db) a])))

  (reg-sub-raw-compat
   :b-sub
   (fn [db [_ b]] (reaction [(:b @db) b])))

  (reg-sub-raw-compat
   :a-b-sub
   (fn [db [_ c]]
     (let [a (re-frame/subscribe [:a-sub c])
           b (re-frame/subscribe [:b-sub c])]
       (reaction {:a @a :b @b}))))

  (let [test-sub (re-frame/subscribe [:a-b-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a [1 :c], :b [2 :c]} @test-sub))))

(deftest test-nonexistent-sub
  (is (nil? (re-frame/subscribe [:non-existence]))))

;============== test cached-subs ================
(def side-effect-atom (atom 0))

(deftest test-cached-subscriptions
  (reset! side-effect-atom 0)

  (reg-sub-raw-compat
   :side-effecting-handler
   (fn side-effect
     [db [_] [_]]
     (swap! side-effect-atom inc)
     (reaction @db)))

  (let [test-sub (re-frame/subscribe [:side-effecting-handler])]
    (reset! db/app-db :test)
    (is (= :test @test-sub))
    (is (= @side-effect-atom 1))
    (re-frame/subscribe [:side-effecting-handler])  ;; this should be handled by cache
    (is (= @side-effect-atom 1))
    (re-frame/subscribe [:side-effecting-handler :a]) ;; should fire again because of the param
    (is (= @side-effect-atom 2))
    (re-frame/subscribe [:side-effecting-handler :a]) ;; this should be handled by cache
    (is (= @side-effect-atom 2))))

;============== test clear-subscription-cache! ================

(deftest test-clear-subscription-cache!
  (re-frame/reg-sub
   :clear-subscription-cache!
   (fn clear-subs-cache [db _] 1))

  (testing "cold cache"
    (is (nil? (subs/-cache-lookup (:subs-cache db/default-frame) [:clear-subscription-cache!]))))
  (testing "cache miss"
    (is (= 1 @(re-frame/subscribe [:clear-subscription-cache!])))
    (is (some? (subs/-cache-lookup (:subs-cache db/default-frame)  [:clear-subscription-cache!]))))
  (testing "clearing"
    (re-frame/clear-subscription-cache!)
    (is (nil? (subs/-cache-lookup (:subs-cache db/default-frame)  [:clear-subscription-cache!])))))

;============== test register-pure macros ================

(deftest test-reg-sub-macro
  (re-frame/reg-sub
   :test-sub
   (fn [db [_]] db))

  (let [test-sub (re-frame/subscribe [:test-sub])]
    (is (= @db/app-db @test-sub))
    (reset! db/app-db 1)
    (is (= 1 @test-sub))))

(deftest test-reg-sub-macro-singleton
  (re-frame/reg-sub
   :a-sub
   (fn [db [_]] (:a db)))

  (re-frame/reg-sub
   :a-b-sub
   (fn [_ _ _]
     (re-frame/subscribe [:a-sub]))
   (fn [a [_]]
     {:a a}))

  (let [test-sub (re-frame/subscribe [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1} @test-sub))
    (swap! db/app-db assoc :b 3)
    (is (= {:a 1} @test-sub))))

(deftest test-reg-sub-macro-vector
  (re-frame/reg-sub
   :a-sub
   (fn [db [_]] (:a db)))

  (re-frame/reg-sub
   :b-sub
   (fn [db [_]] (:b db)))

  (re-frame/reg-sub
   :a-b-sub
   (fn [_ _ _]
     [(re-frame/subscribe [:a-sub])
      (re-frame/subscribe [:b-sub])])
   (fn [[a b] [_]]
     {:a a :b b}))

  (let [test-sub (re-frame/subscribe [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub))
    (swap! db/app-db assoc :b 3)
    (is (= {:a 1 :b 3} @test-sub))))

(deftest test-reg-sub-macro-map
  (re-frame/reg-sub
   :a-sub
   (fn [db [_]] (:a db)))

  (re-frame/reg-sub
   :b-sub
   (fn [db [_]] (:b db)))

  (re-frame/reg-sub
   :a-b-sub
   (fn [_ _ _]
     {:a (re-frame/subscribe [:a-sub])
      :b (re-frame/subscribe [:b-sub])})
   (fn [{:keys [a b]} [_]]
     {:a a :b b}))

  (let [test-sub (re-frame/subscribe [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub))
    (swap! db/app-db assoc :b 3)
    (is (= {:a 1 :b 3} @test-sub))))

(deftest test-sub-macro-parameters
  (re-frame/reg-sub
   :test-sub
   (fn [db [_ b]] [(:a db) b]))

  (let [test-sub (re-frame/subscribe [:test-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= [1 :c] @test-sub))))

(deftest test-sub-macros-chained-parameters
  (re-frame/reg-sub
   :a-sub
   (fn [db [_ a]] [(:a db) a]))

  (re-frame/reg-sub
   :b-sub
   (fn [db [_ b]] [(:b db) b]))

  (re-frame/reg-sub
   :a-b-sub
   (fn [[_ c] _]
     [(re-frame/subscribe [:a-sub c])
      (re-frame/subscribe [:b-sub c])])
   (fn [[a b] [_ c]] {:a a :b b}))

  (let [test-sub (re-frame/subscribe [:a-b-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a [1 :c] :b [2 :c]} @test-sub))))

(deftest test-sub-macros-<-
  "test the syntactical sugar"
  (re-frame/reg-sub
   :a-sub
   (fn [db [_]] (:a db)))

  (re-frame/reg-sub
   :a-b-sub
   :<- [:a-sub]
   (fn [a [_]] {:a a}))

  (let [test-sub (re-frame/subscribe [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1} @test-sub))))

(deftest test-sub-macros-chained-parameters-<-
  "test the syntactical sugar"
  (re-frame/reg-sub
   :a-sub
   (fn [db [_]] (:a db)))

  (re-frame/reg-sub
   :b-sub
   (fn [db [_]] (:b db)))

  (re-frame/reg-sub
   :a-b-sub
   :<- [:a-sub]
   :<- [:b-sub]
   (fn [[a b] [_ c]] {:a a :b b}))

  (let [test-sub (re-frame/subscribe [:a-b-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub))))

(deftest test-sub-macros-->
  "test the syntactical sugar for input signal"
  (re-frame/reg-sub
   :a-sub
   :-> :a)

  (re-frame/reg-sub
   :b-sub
   :-> :b)

  (re-frame/reg-sub
   :c-sub
   :-> :c)

  (re-frame/reg-sub
   :d-sub
   :-> :d)

  (re-frame/reg-sub
   :d-first-sub
   :<- [:d-sub]
   :-> first)

  ;; variant of :d-first-sub without an input parameter
  (re-frame/reg-sub
   :e-first-sub
   :-> (comp first :e))

  ;; test for equality
  (re-frame/reg-sub
   :c-foo?-sub
   :<- [:c-sub]
   :-> #{:foo})

  (re-frame/reg-sub
   :a-b-sub
   :<- [:a-sub]
   :<- [:b-sub]
   :-> (partial zipmap [:a :b]))

  (let [test-sub   (re-frame/subscribe [:a-b-sub])
        test-sub-c (re-frame/subscribe [:c-foo?-sub])
        test-sub-d (re-frame/subscribe [:d-first-sub])
        test-sub-e (re-frame/subscribe [:e-first-sub])]
    (is (= nil @test-sub-c))
    (reset! db/app-db {:a 1 :b 2 :c :foo :d [1 2] :e [3 4]})
    (is (= {:a 1 :b 2} @test-sub))
    (is (= :foo @test-sub-c))
    (is (= 1 @test-sub-d))
    (is (= 3 @test-sub-e))))

(deftest test-sub-macros-=>
  "test the syntactical sugar for input signals and query vector arguments"
  (re-frame/reg-sub
   :a-sub
   :-> :a)

  (re-frame/reg-sub
   :b-sub
   :-> :b)

  (re-frame/reg-sub
   :test-a-sub
   :<- [:a-sub]
   :=> vector)

  ;; test for equality of input signal and query parameter
  (re-frame/reg-sub
   :test-b-sub
   :<- [:b-sub]
   :=> =)

  (let [test-a-sub (re-frame/subscribe [:test-a-sub :c])
        test-b-sub (re-frame/subscribe [:test-b-sub 2])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= [1 :c] @test-a-sub))
    (is (= true @test-b-sub))))

(deftest test-registering-subs-doesnt-create-subscription
  (let [sub-called? (atom false)]
    (with-redefs [re-frame/subscribe (fn [& args] (reset! sub-called? true))]
      (re-frame/reg-sub
       :a-sub
       (fn [db [_]] (:a db)))

      (re-frame/reg-sub
       :b-sub
       (fn [db [_]] (:b db)))

      (re-frame/reg-sub
       :fn-sub
       (fn [[_ c] _]
         [(re-frame/subscribe [:a-sub c])
          (re-frame/subscribe [:b-sub c])])
       (fn [db [_]] (:b db)))

      (re-frame/reg-sub
       :a-sugar-sub
       :<- [:a-sub]
       (fn [[a] [_ c]] {:a a}))

      (re-frame/reg-sub
       :a-b-sub
       :<- [:a-sub]
       :<- [:b-sub]
       (fn [[a b] [_ c]] {:a a :b b})))

    (is (false? @sub-called?))))

;; Dynamic subscriptions

(deftest test-dynamic-subscriptions
  (re-frame/reg-sub
   :dyn-sub
   (fn [db ev dynv]
     (first dynv)))

  (testing "happy case"
    (is (= 1 @(re-frame/subscribe [:dyn-sub] [(r/atom 1)]))))
  (testing "subscription that doesn't exist"
    (is (nil? (re-frame/subscribe [:non-existent] [(r/atom 1)]))))
  (testing "Passing a non-reactive value to a dynamic subscription"
    (is (thrown-with-msg? js/Error #"No protocol method IDeref" @(re-frame/subscribe [:dyn-sub] [1])))))
