(ns vortex-core.binary-record
  (:gen-class))

(defn to-string
  [bs]
  (apply str (map char bs)))

(defn string-to-bytes
  [s]
  (map byte s))

(defn to-int
  ([n bs]
   (if (empty? bs)
     0
     (+ (bit-shift-left (bit-and (first bs) 0xff) (* 8 n))
        (to-int (inc n) (rest bs)))))
  ([bs] (to-int 0 bs)))

(defn int-to-bytes
  [total-bytes n]
  (if (= 0 total-bytes)
    '()
    (cons (unchecked-byte (bit-and n 0xff))
          (int-to-bytes (dec total-bytes) (bit-shift-right n 8)))))

(defn read-record
  [record-def]
  (fn [bs]
    (reduce (fn [[record bs'] [field info]]
              (let [[value bs''] (split-at (:size info) bs')
                    extractor (-> info :type :from-bytes)]
                [(assoc record field (extractor value))
                 bs'']))
            [{} bs]
            (partition 2 record-def))))

(defn record-to-bytes
  [record-def]
  (fn [record]
    (reduce (fn [bs [field info]]
              (let [serializer (-> info :type :to-bytes)
                    new-bytes (serializer (field record))]
                (concat bs new-bytes)))
            []
            (partition 2 record-def))))

(defn record-size
  [record-def]
  (->> record-def
       (partition 2)
       (map second)
       (map :size)
       (apply +)))

(defrecord FieldType [from-bytes to-bytes])
(defn string-field [n] {:size n
                        :type (FieldType. to-string string-to-bytes)})

(def short-field {:size 2
                  :type (FieldType. to-int (partial int-to-bytes 2))})

(def int-field {:size 4
                :type (FieldType. to-int (partial int-to-bytes 4))})

(def byte-field {:size 1
                 :type (FieldType. first (partial int-to-bytes 1))})

(defn byte-array-field [n] {:size n
                            :type (FieldType. identity identity)})

