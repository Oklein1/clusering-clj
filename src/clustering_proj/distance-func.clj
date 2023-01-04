(ns clustering-proj.distance-func
  (:gen-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    DISTANCE FUNCTIONS    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn nested-vec-or-lst? [arr]
  (let [type-of-first-ele (type (get arr 0))]
    (if (or (= (type []) type-of-first-ele)
            (= (type '()) type-of-first-ele))
      true
      false)))

(defn vector-dot-product [arr1 arr2]
  "Returns the inner product (angle) of two vectors (in radians).
   Returns a vector of scalar(s) and
   the scalar (angle) tells if the dot-product is perpendicular: 
   e.g. gives direction (< or > 90, i.e. pos or neg). 
   - Handles single and nested vectors
     - Inputs must be same length (nested and single inputs)
   - 'nested-vec-or-lst?' checks if first ele is vec/lst.
   - Handles N-dimension vectors"
  (defn dot-product [arr1 arr2]
    (let [multiply-corresponding-ele (mapv * arr1 arr2)
          dot-product-output (reduce + multiply-corresponding-ele)]
      dot-product-output))
  (if (and (nested-vec-or-lst? arr1)
           (nested-vec-or-lst? arr2))
    (mapv vector-dot-product arr1 arr2)
    (dot-product arr1 arr2)))



(defn sqaure-num [x]
  (* x x))

(defn euclid-distance [arr1 arr2]
  (let [[x1 y1] arr1
        [x2 y2] arr2]
    (Math/sqrt (+ (sqaure-num (- x2 x1)) (sqaure-num (- y2 y1))))))

(defn cosine-distance [arr1 arr2]
  (defn magnitutde [ele]
    (->> (mapv sqaure-num ele)
         (reduce +)
         (Math/sqrt)))
  (defn distance [arr1 arr2]
    (let [dot-product (vector-dot-product arr1 arr2)]
      (/ dot-product (* (magnitutde arr1)) (magnitutde arr1))))
  (if (and (nested-vec-or-lst? arr1)
           (nested-vec-or-lst? arr2))
    (mapv cosine-distance arr1 arr2)
    (distance arr1 arr2)))