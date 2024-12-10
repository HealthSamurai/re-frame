(ns re-frame.db
  (:require [re-frame.interop :refer [ratom]]
            [re-frame.frame :as frame]
            [re-frame.fx :as fx]
            [re-frame.cofx :as cofx]))


(def default-frame (frame/make-frame))

(fx/register-built-in! default-frame)
(cofx/register-built-in! default-frame)

;; -- Application State  --------------------------------------------------------------------------
;;
;; Should not be accessed directly by application code.
;; Read access goes through subscriptions.
;; Updates via event handlers.
(def app-db (:app-db default-frame))
