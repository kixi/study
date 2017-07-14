(ns study.core
  (:gen-class))

(def trans {\E 0
            \J 1
            \N 1
            \Q 1
            \R 2
            \W 2
            \X 2
            \D 3
            \S  3
            \F 4
            \T  4
            \A 5
            \M  5
            \C 6
            \I  6
            \V 6
            \B  7
            \K 7
            \U  7
            \L 8
            \O  8
            \P 8
            \G  9
            \H 9
            \Z  9})

(def dictionary (with-open [rdr (clojure.java.io/reader "/home/guenter/prog/study/resource/dictionary.txt")]
                  (into [] (line-seq rdr))))

(take 10 dictionary)

(defn map-word [word]
  (filter #(not= % nil) (map #(trans %) (.toUpperCase word))))

(map-word "hell\"o")

(def mapped-dictionary
  (into {} (group-by (fn [[code word]] code) (map (fn [x] [(map-word x) x]) dictionary))))

(take 2 mapped-dictionary)
(mapped-dictionary '(3 4 0 8 8 0 1 9 0 3 7 6 9))

(defn to-number-array [number]
  (filter #(and (> % 0) (< % 10)) (map #(- (int %) (int \0)) number)))

(defn find-match [number-array]
  (if (= (count number-array) 1)
    number-array
    (if (= (count number-array) 0)
      []
      (for [r (range 2 (inc (count number-array)))]
        (let [[curr next] (split-at r number-array)
              _ (println "range: " curr)
              fs (mapped-dictionary curr)
              ns (find-match next)]
          (for [[_ f] fs
                n ns]
            [f n]
            )
          )))))

(find-match [1 0 1 0 3 1 0 0 3] )
(range 2 10)
(mapped-dictionary [1 0])

(into [] (take 2 '(1 0)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
