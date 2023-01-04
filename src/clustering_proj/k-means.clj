(ns clustering-proj.k-means
  (:gen-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   DATA & DATA STRUCTURES   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn pick-k-centriods [k data]
  (into [] (take k (shuffle data))))

(defn generate-cluster-data-struct [k data]
  "The idea is to create N-number of k nested hash-maps.
   I.e.: {cluster-K {:centroid X :values [X]}}
   To do this, i need to account for varying cluster size (k),
   assign a randomly picked number to centroid X,
   and have a vector holding the values (first being the Centroid)."
  (let [centroids-vals (pick-k-centriods k data)
        cluster-num (into [] (range 1 (inc k)))
        generate-skeleton-inner-hashmap (mapv (fn [new-hash-map k-num]
                                                (assoc new-hash-map :cluster k-num)) (into [] (take k (repeat {:values []}))) cluster-num)
        inner-data-structure (mapv (fn [h-map c-vals]
                                     (assoc h-map :values (conj [] c-vals)))
                                   generate-skeleton-inner-hashmap centroids-vals)
        clusters-data-structure (zipmap cluster-num inner-data-structure)]
    (assoc {} :centroids centroids-vals :clusters clusters-data-structure)))


(defn get-centroids [cluster-hashmap]
  (:centroids cluster-hashmap))

(defn number-line-distance [centroid num]
  (- centroid num))


(defn update-data [cluster-hashmap data]
  "One time operation that removes all the starting centroids
   from the vector ref to 'data.'"
  (defn member? [ele coll]
    (loop [n ele
           arr coll]
      (cond
        (empty? arr) false
        (= n (first arr)) true
        :else (recur n (rest arr)))))
  (let [centroid (get-centroids cluster-hashmap)]
    (filterv #(not (member? % centroid)) data)))


(defn get-cluster-pos-num-and-data [cluster-hashmap data]
  "Returns a vector of 3D:
    first pos is the cluster's position in :clusters, 
    second is the cluster's numbre from 1-N, and
    third is the the data element.
    This will later be played with."
  (let [centroids-vec (get-centroids cluster-hashmap)
        distance (mapv #(abs (number-line-distance % data)) centroids-vec)
        smallest-distance (apply min distance)
        cluster-pos (.indexOf distance smallest-distance)
        cluster-num (inc cluster-pos)]
    [cluster-pos cluster-num data]))


(defn update-cluster-bin [hashmap cluster-num new-value]
  (update-in hashmap
             [:clusters
              cluster-num
              :values]
             conj new-value))


(defn update-centroid [hashmap cluster-position cluster-number]
  (defn cluster-average [hashmap cluster-num]
    (let [cluster-vals (get-in hashmap [:clusters cluster-num :values])]
      (float (/ (reduce + cluster-vals) (count cluster-vals)))))
  (assoc-in hashmap  [:centroids cluster-position] (cluster-average hashmap cluster-number)))



(defn k-means-cluster [k data]
  (let [hashmap (generate-cluster-data-struct k data)]
    (loop [cluster-hashmap hashmap
           updated-data (update-data cluster-hashmap data)]
      (let [[cluster-position cluster-number data-ele] (get-cluster-pos-num-and-data cluster-hashmap (first updated-data))
            updated-cluster-bins-hashmap (update-cluster-bin cluster-hashmap cluster-number data-ele)
            updated-hashmap (update-centroid updated-cluster-bins-hashmap cluster-position cluster-number)]
        (if (empty? (rest updated-data))
          cluster-hashmap
          (recur updated-hashmap (rest updated-data)))))))