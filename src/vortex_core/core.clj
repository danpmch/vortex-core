(ns vortex-core.core
  (:require [byte-streams]
            [vortex-core.binary-record :as binary-record]
            [vortex-core.cys-file :as cys])
  (:gen-class))

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
         custom-func-changes (filter (fn [[_ v]] (func-keys v))
                                     mappings)
         func-changes (concat standard-func-changes
                              (if (and mod
                                       (not (empty? custom-func-changes)))
                                (throw (new RuntimeException "Function key mappings in mod layer not supported"))
                                custom-func-changes))
         grouped-funcs (group-by second func-changes)
         reduced-funcs (map (fn [[f kvs]] [f (map first kvs)]) grouped-funcs)
         key-changes (filter (fn [[_ v]] (not (func-keys v)))
                             mappings)]
     (concat (map make-function-set reduced-funcs)
             (map #(make-key-change mod %) key-changes))))
  ([[index mappings]]
   [index (mapcat (fn [[mod ms]] (to-records index mod ms)) mappings)]))

(defn room-for-profile?
  [location]
  (< (+ (mod location 0x1000) 8)
     0x1000))

(defn item-for-profile
  [index next-profile-location]
  {:type 0
   :profile-index index
   :macro-index 0
   :data-shift next-profile-location})

(defn compute-item
  [index current-profile pos]
  (if (room-for-profile? @pos)
    (let [item (item-for-profile index @pos)]
      (swap! pos #(+ (binary-record/record-size cys/profile) %))
      item)
    (do (while (not (room-for-profile? @pos))
          (swap! pos inc))
        (compute-item index current-profile pos))))

(defn compute-items
  [index next-profile-location profiles]
  (if (empty? profiles)
    []
    (cons (compute-item index (first profiles) next-profile-location)
          (compute-items index next-profile-location (rest profiles)))))

(defn layout-to-bytes
  [layout]
  (let [profiles (sort-by first (map to-records layout))
        total-profiles (apply + (map (comp count second) profiles))
        first-profile-location (+ (binary-record/record-size cys/header)
                                  (* (binary-record/record-size cys/item) total-profiles))
        profile-location (atom first-profile-location)
        items (map (fn [[index profiles]]
                     (let [is (compute-items index profile-location profiles)]
                       ; the original macro logic adds 12 zero bytes for no apparent reason
                       (swap! profile-location #(+ 12 %))
                       is))
                   profiles)
        flat-items (apply concat items)
        layout-header {:title "CYFI"
                       :rev 0
                       :item-size (count flat-items)}
        layer-records (fn [[_ profiles]] (concat profiles [{:type :padding}]))
        records (concat [layout-header]
                        flat-items
                        (apply concat (map layer-records profiles)))
        record-bytes (reduce (fn [bytes record]
                               (let [record-bytes (condp = (:type record)
                                                    nil (cys/write-header record)
                                                    0 (cys/write-item record)
                                                    :padding (repeat 12 (byte 0))
                                                    (cys/profile-to-bytes record))]
                                 #_(println record)
                                 #_(println record-bytes)
                                 (into bytes record-bytes)))
                             []
                             records)]
    (concat record-bytes
            (repeat (- 8192 (count record-bytes)) 255))))

(defn save-layout
  [layout path]
  (let [bytes (layout-to-bytes layout)]
    (byte-streams/transfer (byte-array bytes) (java.io.File. path) {:append? false})))

(def all-keys
  [:ESC :Q :W :E :R :T :Y :U :I :O :P :DEL :BACKSPACE
   :TAB :A :S :D :F :G :H :J :K :L :SEMICOLON :ENTER
   :L_SHIFT :Z :X :C :V :B :N :M :COMMA :DOT :R_SHIFT :FN1
   :L_CTRL :L_WIN :L_ALT :FN :L_SPACE-G9 :SPACE :R_ALT :R_WIN :PN-G16 :R_CTRL])

(def white-keys
  [:Q :W :E :R :T :Y :U :I :O :P
   :A :S :D :F :G :H :J :K :L :SEMICOLON
   :Z :X :C :V :B :N :M :COMMA :DOT])

(defn make-key-bindings
  [key-order & ks]
  (if (not (= (count ks) (count key-order)))
    (throw (new RuntimeException (str "Expected " (count key-order) " keys but found " (count ks))))
    (into {} (map (fn [from to] [from to]) key-order ks))))

(def qwerty (apply make-key-bindings white-keys white-keys))

(def dvorak-keys
  [:Q :W :E :R :T :Y :U :I :O :P
   :A :S :D :F :G :H :J :K :L :SEMICOLON :ENTER
   :Z :X :C :V :B :N :M :COMMA :DOT :R_SHIFT])

(def dvorak
  (make-key-bindings dvorak-keys
                    :APOSTROPHE :COMMA :DOT :P :Y :F :G :C :R :L
                    :A :O :E :U :I :D :H :T :N :S :NEG
                    :SEMICOLON :Q :J :K :X :B :M :W :V :Z))

(def numbers
  (make-key-bindings [:Q :W :E :R :T :Y :U :I :O :P]
                    :1 :2 :3 :4 :5 :6 :7 :8 :9 :0))

(def symbols
  (make-key-bindings [:A   :S          :D     :F        :J     :K          :L          :SEMICOLON]
                    :NEG :APOSTROPHE :TILDE :EQUATION :SLASH :L_BRACKETS :R_BRACKETS :BACKSLASH))

(def arrows
  (make-key-bindings [:Z :X :C :V]
                    :L_ARROW :DN_ARROW :UP_ARROW :R_ARROW))

(def gray-keys
  [:ESC                               :DEL :BACKSPACE
   :TAB                                        :ENTER
   :L_SHIFT                             :R_SHIFT ; skip :FN1
   :L_CTRL :L_WIN :L_ALT    :SPACE        :R_CTRL])

(def default-gray-keys (apply make-key-bindings gray-keys gray-keys))

(def standard-pn-keys (merge default-gray-keys
                             numbers
                             symbols
                             arrows))

(def full-dvorak
  {nil (merge dvorak
              {:L_SPACE-G9 :PN-G16})
   :PN-G16 standard-pn-keys})

(def magicka
  {nil (merge (make-key-bindings [:Z :X :C :V]
                                :1 :2 :3 :4)
              {:L_SPACE-G9 :SPACE})})

(def standard-gaming
  {nil {:L_SPACE-G9 :SPACE}
   :PN-G16 standard-pn-keys})

(def full-qwerty
  {nil {:L_SPACE-G9 :PN-G16}
   :PN-G16 standard-pn-keys})

(def full-layout {1 full-qwerty
                  2 full-dvorak
                  3 standard-gaming})

(save-layout full-layout "test2.cys")

