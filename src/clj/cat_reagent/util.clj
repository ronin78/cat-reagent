(ns cat-reagent.util
  (:gen-class)
  )

;;Utility functions
(defn drop-nth [n coll] 
  (keep-indexed #(if (not= %1 n) %2) coll)
 )  
(defn keynum
  [num]
  (keyword (str num))
  )
