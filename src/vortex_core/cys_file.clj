(ns vortex-core.cys-file
  (:require [vortex-core.binary-record :as binary-record]
            [vortex-core.key-map :as km])
  (:gen-class))

(def header [:title (binary-record/string-field 4)
             :rev binary-record/short-field
             :item-size binary-record/short-field])
(def read-header (binary-record/read-record header))
(def write-header (binary-record/record-to-bytes header))

(def item [:type binary-record/byte-field
           :profile-index binary-record/byte-field
           :macro-index binary-record/short-field
           :data-shift binary-record/int-field])
(def read-item (binary-record/read-record item))
(def write-item (binary-record/record-to-bytes item))

(def profile [:length binary-record/byte-field
              :key binary-record/byte-field
              :index (binary-record/byte-array-field 2)
              :data (binary-record/byte-array-field 4)])

(def read-profile (binary-record/read-record profile))
(def write-profile (binary-record/record-to-bytes profile))

(defn profile-for-item
  [item file-bytes]
  (first (read-profile (drop (:data-shift item) file-bytes))))

(defn function-set
  [profile]
  (let [k (condp = (bit-and (:key profile) 0xff)
            0x94 :FN-G14
            0x95 :FN1-G10
            0x96 :PN-G16
            0x97 :FN3) ; don't think this key actually exists for vortex core
        index (binary-record/to-int (:index profile))
        ktms (->> profile :data (take index) (map long))]
    {:type :function-set
     :key k
     :target-keys (mapv #(get km/vortex-core-function-keys %) ktms)}))

(defn pack-function-set
  [p]
  (let [k (condp = (:key p)
            :FN-G14 0x94
            :FN1-G10 0x95
            :PN-G16 0x96
            :FN3 0x97)
        padding (repeat (- 4 (count (:target-keys p))) 0)]
    ((binary-record/record-to-bytes profile) {:length 2
                                :key k
                                :index [(count (:target-keys p)) 0]
                                :data (concat (map #(.indexOf km/vortex-core-function-keys %) (:target-keys p))
                                              padding)})))

(defn pack-key-change
  [p]
  (let [key-to-modifier {:FN-G14 1
                         :FN1-G10 2
                         :PN-G16 3
                         nil 0}
        [source-modifier source-key] (:source-chord p)
        [target-modifier target-key] (:target-chord p)]
    ((binary-record/record-to-bytes profile) {:length 2
                                :key 0x20
                                :index [(get km/key-map-reverse source-key) (get key-to-modifier source-modifier)]
                                :data [(get km/key-map-reverse target-key) (get key-to-modifier target-modifier) 0 0]})))

(defn key-change
  [profile]
  (let [modifier-to-key (fn [modifier] (condp = modifier
                                         1 :FN-G14
                                         2 :FN1-G10
                                         3 :PN-G16
                                         nil))
        to-key (fn [k] (get km/key-map (bit-and (long k) 0xff)))
        [k modifier] (:index profile)
        [target-k target-modifier] (:data profile)]
    {:type :key-change
     :source-chord [(modifier-to-key modifier) (to-key k)]
     :target-chord [(modifier-to-key target-modifier) (to-key target-k)]}))

(defn profile-to-bytes
  [profile]
  (condp = (:type profile)
    :function-set (pack-function-set profile)
    :key-change (pack-key-change profile)))

(defn to-key-def
  [profile]
  (condp = (:key profile)
    0x20 (key-change profile)
    (function-set profile)))

(defn read-n
  [f n bs]
  (if (= 0 n)
    [nil bs]
    (let [[thing bs-next] (f bs)
          [others bs-left] (read-n f (dec n) bs-next)]
      [(cons thing others) bs-left])))

(def file-bytes (byte-streams/to-byte-array (java.io.File. "resources/full_dvorak_w_normal_space.cys")))

(defn bytes-to-layers
  [file-bytes]
  (let [[h item-bs] ((binary-record/read-record header) file-bytes)
        [items _] (read-n (binary-record/read-record item) (:item-size h) item-bs)
        profiles-with-layer (map (fn [item] [item (to-key-def (profile-for-item item file-bytes))])
                                 items)
        layers (group-by #(:profile-index (first %)) profiles-with-layer)
        reduced-layers (into {} (map (fn [[k vs]] [k (map second vs)]) layers))]
    #_(def items-raw items)
    #_(def profiles profiles-with-layer)
    reduced-layers))
