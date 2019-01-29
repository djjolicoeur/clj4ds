(ns clj4ds.data
  (:require
   [clojure.java.io :as io]
   [incanter.core :as i]
   [incanter.charts :as c]
   [incanter.stats :as s]
   [incanter.distributions :as d]
   [incanter.svg :as svg]
   [incanter.excel :as xls]
   [me.raynes.fs :as fs]))

(defmulti load-data identity)

(defmethod load-data :uk
  [_]
  (-> "UK2010.xls" io/resource str xls/read-xls))

(defn ex-1-1
  []
  (i/col-names (load-data :uk)))

(defn ex-1-2
  []
  (->> :uk
       load-data
       (i/$ "Election Year")))

(defn ex-1-3
  []
  (->> :uk
       load-data
       (i/$ "Election Year")
       distinct))

(defn ex-1-4
  []
  (->> :uk
       load-data
       (i/$ "Election Year")
       frequencies))

(defn ex-1-5
  []
  (->> :uk
       load-data
       (i/$where {"Election Year" {:$eq nil}})
       i/to-map))

(defmethod load-data :uk-scrubbed
  [_]
  (->> :uk load-data (i/$where {"Election Year" {:$ne nil}})))

(defn ex-1-6
  []
  (->> :uk-scrubbed load-data  (i/$ "Electorate") count))

(defn mean
  [xs]
  (/ (reduce + xs)
     (count xs)))

(defn ex-1-7
  []
  (->> :uk-scrubbed load-data (i/$ "Electorate") mean))

(defn median
  [xs]
  (let [n (count xs)
        mid (int (/ n 2))]
    (if (odd? n)
      (nth (sort xs) mid)
      (->> xs
           sort
           (drop (dec mid))
           (take 2)
           mean))))

(defn ex-1-8
  []
  (->> :uk-scrubbed load-data (i/$ "Electorate") median))

(defn variance
  [xs]
  (let [x-bar (mean xs)
        square-deviation (fn [x]
                           (i/sq (- x x-bar)))]
    (->> xs (map square-deviation) mean)))

(defn standard-deviation
  [xs]
  (i/sqrt (variance xs)))

(defn ex-1-9
  []
  (->> :uk-scrubbed load-data (i/$ "Electorate") standard-deviation))

(defn quantile
  [q xs]
  (let [n (dec (count xs))
        i (-> (* n q)
              (+ 1/2)
              int)]
    (nth (sort xs) i)))

(defn ex-1-10
  []
  (let [xs (->> :uk-scrubbed load-data (i/$ "Electorate"))
        f (fn [q]
            (quantile q xs))]
    (map f [0 1/4 1/2 3/4 1])))

(defn bin
  [n-bins xs]
  (let [min-x (apply min xs)
        max-x (apply max xs)
        range-x (- max-x min-x)
        bin-fn (fn [x]
                 (-> x
                     (- min-x)
                     (/ range-x)
                     (* n-bins)
                     int
                     (min (dec n-bins))))]
    (map bin-fn xs)))

(defn ex-1-11
  []
  (->> :uk-scrubbed
       load-data
       (i/$ "Electorate")
       (bin 10)
       frequencies
       (into (sorted-map))))

(defn ex-1-12
  []
  (->> :uk-scrubbed
       load-data
       (i/$ "Electorate")
       c/histogram
       i/view))

(defn uk-electorate
  []
  (->> :uk-scrubbed load-data (i/$ "Electorate")))

(defn ex-1-13
  []
  (-> (uk-electorate)
      (c/histogram :nbins 200)
      i/view))

(defn ex-1-14
  []
  (-> (uk-electorate)
      (c/histogram :x-label "UK Electorate" :nbins 20)
      i/view))

(defn ex-1-15
  []
  (let [xs (->> (repeatedly rand)
                (take 1000))]
    (-> xs
        (c/histogram :x-label "Uniform Distribution" :nbins 20)
        i/view)))

(defn ex-1-16
  []
  (let [xs (->> (repeatedly rand)
                (partition 10)
                (map mean)
                (take 1000))]
    (-> xs
        (c/histogram :x-label "Distribution of Means"
                     :nbins 20)
        i/view)))

(defn ex-1-17
  []
  (let [distribution (d/normal-distribution)
        xs (->> (repeatedly #(d/draw distribution))
                (take 1000))]
    (-> xs
        (c/histogram :x-label "Normal Distribution"
                     :nbins 20)
        i/view)))

(defn honest-baker
  [mean sd]
  (let [distribution (d/normal-distribution mean sd)]
    (repeatedly #(d/draw distribution))))

(defn ex-1-18
  []
  (-> 10000
      (take (honest-baker 1000 30))
      (c/histogram :x-label "Honest Baker" :nbins 25)
      i/view))

(defn dishonest-baker
  [mean sd]
  (let [distribution (d/normal-distribution)]
    (->> (repeatedly #(d/draw distribution))
         (partition 13)
         (map (partial apply max)))))

(defn ex-1-19
  []
  (-> 10000
      (take (dishonest-baker 950 30))
      (c/histogram :x-label "Dishonst Baker" :nbins 25)
      i/view))

(defn ex-1-20
  []
  (let [weights (take 10000 (dishonest-baker 950 30))]
    {:mean (mean weights)
     :median (median weights)
     :skewness (s/skewness weights)}))

(defn ex-1-21
  []
  (->> (honest-baker 1000 30)
       (take 10000)
       c/qq-plot
       i/view)
  (->> (dishonest-baker 950 30)
       (take 10000)
       c/qq-plot
       i/view))
