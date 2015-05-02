(ns zoo-time-series.core
  (:require [incanter.core :as i]
            [incanter.zoo :as zoo]
            [incanter.io :as io]
            [incanter.charts :as charts]
            [clojure.math.numeric-tower :as m]))

(defn read-from
  [file-name]
  (io/read-dataset file-name :header true))

(defn as-zoo-ds
  [ds]
  (zoo/zoo ds :date))

(defn- as-centi-float-precision
  [v]
  (* 0.01 (m/round (* 100 (float v)))))

(defn as-rolling-added-churn
  "Calculates a rolling average of the
   positive churn (added lines of code)."
  [ds n-days]
  (->>
   (i/sel ds :cols :added)
   (zoo/roll-mean n-days)
   (map as-centi-float-precision)
   (i/dataset [:rolling-added])
   (i/conj-cols ds)))

(defn- as-time-series-plot
  "To visualize multiple values over time, we
   need two steps; One for each series. Let's
   encapsulate that here."
  [dates raw-churn rolling-average]
  (let [chart (charts/time-series-plot
               dates
               raw-churn
               :title "Churn Trends"
               :y-label "Churn"
               :x-label "Date"
               :legend true)]
    (charts/add-lines chart dates rolling-average)))

(defn view-churn-chart
  [ds]
  (let [dates (map #(.getMillis %) (i/$ :index ds)) ; Incanter wants them as ms
        raw-added (i/$ :added ds)
        rolling-added (i/$ :rolling-added ds)]
    (i/view
     (as-time-series-plot dates raw-added rolling-added))))

(defn view-churn-trends
  ([churn-file-name]
     (view-churn-trends churn-file-name 5))
  ([churn-file-name rolling-avg-days]
     (->
      (read-from churn-file-name)
      as-zoo-ds
      (as-rolling-added-churn rolling-avg-days)
      view-churn-chart)))
