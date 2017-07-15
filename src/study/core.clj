(ns study.core
  (:gen-class))

(def trans {\E 0
            \J 1 \N 1 \Q 1
            \R 2 \W 2 \X 2
            \D 3 \S  3
            \F 4 \T  4
            \A 5 \M  5
            \C 6 \I  6 \V 6
            \B  7 \K 7 \U  7
            \L 8 \O  8 \P 8
            \G  9 \H 9 \Z  9})

(def dictionary
  (with-open [rdr (clojure.java.io/reader
                   (clojure.java.io/resource "dictionary.txt"))]
    (into [] (line-seq rdr))))


(defn word-to-digits [word]
  (->> (.toUpperCase word)
       (map #(trans %) )
       (filter #(not= % nil) )))


(def mapped-dictionary
  (->> dictionary
       (map (fn [x] [(word-to-digits x) x]) )
       (group-by (fn [[code word]] code) )
       (into {} )))

(defn to-number-array [number]
  (->> number
       (map #(- (int %) (int \0)) )
       (filter #(and (>= % 0) (< % 10)))))


(defn digit? [str]
  (Character/isDigit (first str)))

(defn find-match [acc number-array]
  (if (= 0 (count number-array))
    [acc]
    (apply concat
           (for [r (range 1 (inc (count number-array)))]
             (let [[curr next] (split-at r number-array)
                   fs (if (= (count curr) 1)
                        [['(1) (str (nth curr 0))]]
                        (mapped-dictionary curr))]
               (apply concat
                      (for [[_ f] fs]
                        (if (and f
                                 (or (= (count acc) 0)
                                     (not (and (digit? f)
                                               (digit? (last acc))))))
                          (find-match (conj acc f) next)))))))))

(defn format-res [res]
  (map #(clojure.string/join " " %) res))

(defn strings-for-number [str-number]
  (->> str-number
       to-number-array
       (find-match [])
       format-res
       (map (fn [x] [str-number x]))))

(defn do-prog []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "input.txt"))]
    (let [rows (line-seq rdr)]
      (doseq [number rows]
        (doseq [[n r] (strings-for-number number)]
          (println n ": " r))))))

(defn -main
  [& args]
  (do-prog))
