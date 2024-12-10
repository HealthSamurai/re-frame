(ns re-frame.cofx
  (:require
   [re-frame.interceptor  :refer [->interceptor]]
   [re-frame.registrar    :refer [get-handler register-handler *current-frame*]]
   [re-frame.loggers      :refer [console]]))

;; -- Registration ------------------------------------------------------------

(def kind :cofx)
(assert (re-frame.registrar/kinds kind))

;; -- Interceptor -------------------------------------------------------------

(defn inject-cofx
  ([registry id]
   (->interceptor
    :id      :coeffects
    :before  (fn coeffects-before
               [context]
               (if-let [handler (get-handler registry kind id)]
                 (binding [*current-frame* (:frame context)]
                   (update context :coeffects handler))
                 (console :error "No cofx handler registered for" id)))))
  ([registry id value]
   (->interceptor
    :id     :coeffects
    :before  (fn coeffects-before
               [context]
               (if-let [handler (get-handler registry kind id)]
                 (binding [*current-frame* (:frame context)]
                   (update context :coeffects handler value))
                 (console :error "No cofx handler registered for" id))))))

;; -- Builtin CoEffects Handlers  ---------------------------------------------

(defn register-built-in!
  [{:keys [registry]}]
  (let [reg-cofx (partial register-handler registry kind)]
    ;; :db
    ;;
    ;; Adds to coeffects the value in `app-db`, under the key `:db`
    (reg-cofx
      :db
      (fn db-coeffects-handler
        [coeffects]
        (assoc coeffects :db @(:app-db *current-frame*))))))
