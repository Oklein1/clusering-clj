(ns clustering-proj.core
  (:gen-class)
  (:require [clustering-proj.k-means :refer k-means-cluster]))





(defn main []
  (def data (into [] (range 1 26)))
  (k-means-cluster 3 data))


(main )
