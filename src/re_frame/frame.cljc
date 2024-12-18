(ns re-frame.frame
  (:require [re-frame.cofx :as cofx]
            [re-frame.events :as events]
            [re-frame.fx :as fx]
            [re-frame.interop :as interop]
            [re-frame.registrar :as reg]
            [re-frame.router :as router]
            [re-frame.std-interceptors :as stdi]
            [re-frame.subs :as subs]))

(defprotocol IFrame
  ;; dispatch ----
  (dispatch [this event-v])
  (dispatch-sync [this event-v])

  ;; subs ----
  (reg-sub-raw [this query-id handler-fn])
  (reg-sub
    ;; variadic args are not supported in protocols
    [this query-id arg1]
    [this query-id arg1 arg2]
    [this query-id arg1 arg2 arg3]
    [this query-id arg1 arg2 arg3 arg4]
    [this query-id arg1 arg2 arg3 arg4 arg5]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15]
    [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16])

  (subscribe
    [this query-v]
    [this query-v dynv])
  (clear-sub
    [this]
    [this query-id])
  (clear-subscriptions-cache [this])

  ;; fx ----
  (reg-fx [this fx-id handler-fn])
  (clear-fx
    [this]
    [this fx-id])

  ;; cofx ----
  (reg-cofx [this cofx-id handler-fn])
  (inject-cofx
    [this cofx-id]
    [this cofx-id value])
  (clear-cofx
    [this]
    [this cofx-id])

  ;; events ----
  (clear-event
    [this]
    [this event-id])
  (reg-event-db
    [this id db-handler]
    [this id interceptors db-handler])
  (reg-event-fx
    [this id fx-handler]
    [this id interceptors fx-handler])
  (reg-event-ctx
    [this id handler]
    [this id interceptors handler])

  ;; errors ----
  (event-error-handler
    [this handler]))

;; connect all the pieces of state ----
(defrecord Frame [registry event-queue app-db subs-cache default-interceptors]
  IFrame
  ;; dispatch ----
  (dispatch [this event-v]
    (router/dispatch this event-v))
  (dispatch-sync [this event-v]
    (router/dispatch-sync this event-v))

  ;; subs ----
  (reg-sub-raw [this query-id handler-fn]
    (reg/register-handler registry subs/kind query-id handler-fn))
  (reg-sub [this query-id arg1]
    (subs/reg-sub this query-id arg1))
  (reg-sub [this query-id arg1 arg2]
    (subs/reg-sub this query-id arg1 arg2))
  (reg-sub [this query-id arg1 arg2 arg3]
    (subs/reg-sub this query-id arg1 arg2 arg3))
  (reg-sub [this query-id arg1 arg2 arg3 arg4]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15))
  (reg-sub [this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16]
    (subs/reg-sub this query-id arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16))
  (subscribe [this query-v]
    (subs/subscribe this query-v))
  (subscribe [this query-v dynv]
    (subs/subscribe this query-v dynv))
  (clear-sub [this]
    (reg/clear-handlers registry subs/kind))
  (clear-sub [this query-id]
    (reg/clear-handlers registry subs/kind query-id))
  (clear-subscriptions-cache [this]
    (subs/clear-subscription-cache! subs-cache))

  ;; fx ----
  (reg-fx [this fx-id handler-fn]
    (reg/register-handler registry fx/kind fx-id handler-fn))
  (clear-fx [this]
    (reg/clear-handlers registry fx/kind))
  (clear-fx [this fx-id]
    (reg/clear-handlers registry fx/kind fx-id))

  ;; cofx ----
  (reg-cofx [this cofx-id handler-fn]
    (reg/register-handler registry cofx/kind cofx-id handler-fn))
  (inject-cofx [this cofx-id]
    (cofx/inject-cofx registry cofx-id))
  (inject-cofx [this cofx-id value]
    (cofx/inject-cofx registry cofx-id value))
  (clear-cofx [this]
    (reg/clear-handlers registry cofx/kind))
  (clear-cofx [this cofx-id]
    (reg/clear-handlers registry cofx/kind cofx-id))

  ;; events ----
  (clear-event [this]
    (reg/clear-handlers registry events/kind))
  (clear-event [this id]
    (reg/clear-handlers registry events/kind id))

  (reg-event-db [this id db-handler]
    (reg-event-db this id nil db-handler))
  (reg-event-db [this id interceptors db-handler]
    (events/register
      registry
      id
      [default-interceptors interceptors (stdi/db-handler->interceptor db-handler)]))
  (reg-event-fx [this id fx-handler]
    (reg-event-fx this id nil fx-handler))
  (reg-event-fx [this id interceptors fx-handler]
    (events/register
      registry
      id
      [default-interceptors interceptors (stdi/fx-handler->interceptor fx-handler)]))
  (reg-event-ctx [this id handler]
    (reg-event-ctx this id nil handler))
  (reg-event-ctx [this id interceptors handler]
    (events/register
      registry
      id
      [default-interceptors interceptors (stdi/ctx-handler->interceptor handler)]))

  (event-error-handler [this handler]
    (reg/register-handler registry :error :event-handler handler)))

(def frame-id (atom 0))

(defn make-frame
  "Creates a new frame, which bundles the registry (subscriptions, event-handlers,
  fx, cofx), app-db, subscription cache, default interceptors, and event queue.

  :registry, :app-db, and :interceptors can be provided through an options map."
  [& [{:keys [registry app-db interceptors] :as extra-keys}]]
  (let [registry             (or registry (reg/make-registry))
        app-db               (or app-db (interop/ratom {}))
        default-interceptors [(cofx/inject-cofx registry :db)
                              (fx/do-fx registry)
                              stdi/inject-global-interceptors]
        frame                (map->Frame
                               (merge {:frame-id             (swap! frame-id inc)
                                       :registry             registry
                                       :app-db               app-db
                                       :subs-cache           (subs/->SubscriptionCache (atom {}))
                                       :default-interceptors (if interceptors
                                                               (if (:replace (meta interceptors))
                                                                 interceptors
                                                                 (into default-interceptors interceptors))
                                                               default-interceptors)
                                       :event-queue          (router/->EventQueue :idle interop/empty-queue {} nil)}
                                      (dissoc extra-keys :registry :app-db :interceptors)))]
    ;; When events / fx fire, they get their frame from the event-queue
    (router/set-frame (:event-queue frame) frame)
    frame))

(defn make-restore-fn
  "Checkpoints the state of re-frame and returns a function which, when
  later called, will restore re-frame to that checkpointed state.

  Checkpoint includes app-db, all registered handlers and all subscriptions."
  ([frame]
   (let [handlers   (-> frame :registry :kind->id->handler deref)
         app-db     (-> frame :app-db deref)
         subs-cache (-> frame :subs-cache deref)]
     (fn []
       ;; call `dispose!` on all current subscriptions which
       ;; didn't originally exist.
       (let [original-subs (-> subs-cache vals set)
             current-subs  (-> frame :subs-cache deref vals)]
         (doseq [sub current-subs
                 :when (not (contains? original-subs sub))]
           (interop/dispose! sub)))

       ;; Reset the atoms
       ;; We don't need to reset subs-cache, as disposing of the subs
       ;; removes them from the cache anyway
       (reset! (-> frame :registry :kind->id->handler) handlers)
       (reset! (-> frame :app-db) app-db)
       nil))))
