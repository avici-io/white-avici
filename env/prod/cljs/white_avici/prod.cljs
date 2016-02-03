(ns white-avici.prod
  (:require [white-avici.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
