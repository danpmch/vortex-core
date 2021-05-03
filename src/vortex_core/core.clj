(ns vortex-core.core
  (:require [byte-streams]
            [vortex-core.key-map :as km])
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

(def header [:title (string-field 4)
             :rev short-field
             :item-size short-field])
(def read-header (read-record header))

(def item [:type byte-field
           :profile-index byte-field
           :macro-index short-field
           :data-shift int-field])
(def read-item (read-record item))

(def profile [:length byte-field
              :key byte-field
              :index (byte-array-field 2)
              :data (byte-array-field 4)])

(def read-profile (read-record profile))

(defn record-size
  [record-def]
  (->> record-def
       (partition 2)
       (map second)
       (map :size)
       (apply +)))

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
        index (to-int (:index profile))
        ktms (->> profile :data (take index) (map long))]
    {:type :function-set
     :key k
     :target-keys (mapv #(get km/vortex-core-function-keys %) ktms)}))

(comment "problem cases"
          (2 32 -29 3 -29 0 0 0)
          (2 32 -30 3 -30 0 0 0)
          (2 32 -32 3 -32 0 0 0)
          (2 32 -28 3 -28 0 0 0))
(defn key-change
  [profile]
  (let [modifier-to-key (fn [modifier] (condp = modifier
                                         1 :fn-g14
                                         2 :fn1-g10
                                         3 :pn-g16
                                         nil))
        to-key (fn [k] (get km/key-map (bit-and (long k) 0xff)))
        [k modifier] (:index profile)
        [target-k target-modifier] (:data profile)]
    {:type :key-change
     :source-chord [(modifier-to-key modifier) (to-key k)]
     :target-chord [(modifier-to-key target-modifier) (to-key target-k)]}))

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

(def layout
  (let [[h item-bs] ((read-record header) file-bytes)
        [items _] (read-n (read-record item) (:item-size h) item-bs)
        profiles-with-layer (map (fn [item] [item (to-key-def (profile-for-item item file-bytes))])
                                 items)
        layers (group-by #(:profile-index (first %)) profiles-with-layer)
        reduced-layers (into {} (map (fn [[k vs]] [k (map second vs)]) layers))
        ]
    (def items-raw items)
    (def profiles profiles-with-layer)
    reduced-layers))

(defn layer-to-string
  [layer]
  (map (fn [mapping]
         (condp = (:type mapping)
           :function-set (str (:key mapping) " -> " (:target-keys mapping))
           :key-change (str (:source-chord mapping) " -> " (:target-chord mapping))))
       layer))

(defn make-layer-sparse
  [& mappings]
  (into {} (map vec (partition 2 mappings))))

(def all-keys
  [:ESC :Q :W :E :R :T :Y :U :I :O :P :DEL :BACKSPACE
   :TAB :A :S :D :F :G :H :J :K :L :SEMICOLON :ENTER
   :L_SHIFT :Z :X :C :V :B :N :M :COMMA :DOT :R_SHIFT :FN1
   :L_CTRL :L_WIN :L_ALT :FN :L_SPACE-G9 :SPACE :R_ALT :R_WIN :PN-G16 :R_CTRL])

(def white-keys
  [:Q :W :E :R :T :Y :U :I :O :P
   :A :S :D :F :G :H :J :K :L :SEMICOLON
   :Z :X :C :V :B :N :M :COMMA :DOT])

(defn make-layer-dense
  [key-order & ks]
  (if (not (= (count ks) (count key-order)))
    (throw (new RuntimeException (str "Expected " (count key-order) " keys but found " (count ks))))
    (into {} (map (fn [from to] [from to]) key-order ks))))

(def qwerty (apply make-layer-dense white-keys white-keys))

(def dvorak-keys
  [:Q :W :E :R :T :Y :U :I :O :P
   :A :S :D :F :G :H :J :K :L :SEMICOLON :ENTER
   :Z :X :C :V :B :N :M :COMMA :DOT :R_SHIFT])

(def dvorak
  (make-layer-dense dvorak-keys
                    :APOSTROPHE :COMMA :DOT :P :Y :F :G :C :R :L
                    :A :O :E :U :I :D :H :T :N :S :NEG
                    :SEMICOLON :Q :J :K :X :B :M :W :V :Z))

(def numbers
  (make-layer-dense [:Q :W :E :R :T :Y :U :I :O :P]
                    :1 :2 :3 :4 :5 :6 :7 :8 :9 :0))

(def symbols
  (make-layer-dense [:D :F :J :K :L :SEMICOLON]
                    :TILDE :EQUATION :SLASH :L_BRACKETS :R_BRACKETS :BACKSLASH))

(def arrows
  (make-layer-dense [:Z :X :C :V]
                    :L_ARROW :DN_ARROW :UP_ARROW :R_ARROW))

(def gray-keys
  [:ESC                               :DEL :BACKSPACE
   :TAB                                        :ENTER
   :L_SHIFT                             :R_SHIFT ; skip :FN1
   :L_CTRL :L_WIN :L_ALT    :SPACE        :R_CTRL])

(def default-gray-keys (apply make-layer-dense gray-keys gray-keys))

(def standard-pn-keys (merge default-gray-keys
                             numbers
                             symbols
                             arrows))

(def full-dvorak
  {nil (merge dvorak
              {:L_SPACE-G9 :PN-G16})
   :pn standard-pn-keys})

(def magicka
  {nil (merge (make-layer-dense [:Z :X :C :V]
                                :1 :2 :3 :4)
              {:L_SPACE-G9 :SPACE})})

(def standard-gaming
  {nil {:L_SPACE-G9 :SPACE}})

(def full-qwerty
  {nil {:L_SPACE-G9 :PN-G16}
   :pn standard-pn-keys})

(def full-layout {1 full-dvorak
                  2 magicka
                  3 standard-gaming})

(defn make-key-change
  [mod [source target]]
  {:type :key-change
   :source-chord [mod source]
   :target-chord [nil target]})

(defn make-function-set
  [[f targets]]
  {:type :function-set
   :key f
   :target-keys targets})

(defn to-records
  ([index mod mappings]
   (let [func-keys #{:FN-G14 :FN1-G10 :PN-G16}
         standard-func-changes (when-not mod [[:FN-G14 :FN-G14]
                                              [:FN1-G10 :FN1-G10]
                                              [:PN-G16 :PN-G16]])
         custom-func-changes (filter (fn [[k v]] (func-keys v))
                                      mappings)
         func-changes (concat standard-func-changes
                              (if (and mod
                                       (not (empty? custom-func-changes)))
                                (throw (new RuntimeException "Function key mappings in mod layer not supported"))
                                custom-func-changes))
         grouped-funcs (group-by second func-changes)
         reduced-funcs (map (fn [[f kvs]] [f (map first kvs)]) grouped-funcs)
         key-changes (filter (fn [[k v]] (not (func-keys v)))
                             mappings)]
     (concat (map make-function-set reduced-funcs)
             (map #(make-key-change mod %) key-changes))))
  ([[index mappings]]
   [index (mapcat (fn [[mod ms]] (to-records index mod ms)) mappings)]))

(defn room-for-profile?
  [location]
  true)

(defn item-for-profile
  [profile index next-profile-location]
  {:type 0
   :profile-index index
   :macro-index 0
   :data-shift next-profile-location})

(defn compute-items
  [index next-profile-location profiles]
  (cond (empty? profiles) []
        (room-for-profile? next-profile-location)
          (cons (item-for-profile (first profiles) index next-profile-location)
                (compute-items (+ next-profile-location (record-size profile)) (rest profiles)))
        :else (throw (new RuntimeException "not implemented"))))

(defn layout-to-bytes
  [layout]
  (let [profiles (map to-records layout)
        first-profile-location (+ (record-size header)
                                  (* (record-size item)
                                     (count profiles)))]
    profiles))

