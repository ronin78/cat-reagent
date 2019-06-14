(ns cat-reagent.prod
  (:require [cat-reagent.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
