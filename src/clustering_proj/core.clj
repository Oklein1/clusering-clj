(ns clustering-proj.core
  (:gen-class)
  (:require [clustering-proj.k-means :refer [k-means-cluster]]))


(defn main [] 
  (k-means-cluster 3 (into [] (range 1 26))))


(main )
