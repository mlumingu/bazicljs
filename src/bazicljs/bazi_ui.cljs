(ns bazicljs.bazi-ui
  (:require
   [re-frame.core :as rf]   
   [reagent.dom :as rdom]
   [reagent.core :as r]

   [bazicljs.styles :as styles]
   [bazicljs.calendar :as cal]
   [bazicljs.bazi-util :as bu]
   [bazicljs.bazi :as b]
   [bazicljs.re-frame :as brf]

   [cljs-time.core :as tc]
   [cljs-time.format :as tf]
   [cljs-http.client :as http]   

   [clojure.string :as string]
   ))




(defn palace-bg-style [setting element usefull]
  (case setting
    "element" (styles/element-color element)
    "usefull" (styles/usefull-color usefull)
    "none"    nil))

(defn stem [{sid :stem dm :dm palace :palace} col]
  (let [element  (bu/stem-element sid)
        usefulls @(rf/subscribe [:usefull-elem])
        usefull  (usefulls element)
        bg-setting (:palace-bg @(rf/subscribe [:settings]))
        bg-style (palace-bg-style bg-setting element usefull)]
    [:div {:class [(styles/palace col) bg-style]}
     (bu/STEM-HTML sid)
     (if (not= palace :D)[:div {:class (styles/palace-god)} (bu/GOD-NAMES (bu/stem-god dm sid))])]))


(defn branch [{bid :branch dm :dm void :void} col]
  (let [element (bu/branch-element bid)
        usefulls @(rf/subscribe [:usefull-elem])
        usefull (usefulls element)
        bg-setting (:palace-bg @(rf/subscribe [:settings]))
        bg-style (palace-bg-style bg-setting element usefull)]
    [:div {:class [(styles/palace col) bg-style]}
     (bu/BRANCH-HTML bid)
     [:div {:class (styles/palace-god)} (bu/GOD-NAMES (bu/branch-god dm bid))]
     (if void [:div {:class (styles/void)} "DE"])]))


(defn hstem [sid order dm]
  (let [element (bu/stem-element sid)
        usefulls @(rf/subscribe [:usefull-elem])
        usefull (usefulls element)
        bg-setting (:palace-bg @(rf/subscribe [:settings]))
        bg-style (palace-bg-style bg-setting element usefull)
        shtml (bu/STEM-HTML sid)]
    [:div {:class [(styles/hstem order) bg-style]} shtml
     [:div {:class (styles/hs-god)} (bu/GOD-NAMES (bu/stem-god dm sid))]]))


(defn hstems [{bid :branch dm :dm} col]
  (let [stems         (bu/HIDDEN-STEMS bid)
        order         (if (= (count stems) 3) [1 0 2] [0 1])
        indexed-stems (map vector order stems)]
    [:div {:style {:grid-column-start col
                   :display "flex "
                   :font-size "1.5em"
                   :justify-content :center
                   :gap "0.2em"}}
     (for [[i hs] indexed-stems] ^{:key i} [hstem hs i dm])]))


(defn symbol-str [s]
  (str (rest (str s))))


(defn relations [rels]
  (for [[i {:keys [rtype palaces element]}] (map-indexed vector rels)]
    ^{:key i}
    [:div {:class (styles/relation (if element  (styles/element-colors element)))}
     (str (name rtype) " " (string/join " " (map name palaces)))]))

(defn n-relations [{ss :cshas rels :n-relations} col]
  [:div {:class (styles/relations col)}
   (concat
    (relations rels)
    (for [[i name] (map-indexed vector ss)]
      ^{:key (str i "s")} [:div name]))
   ])

(defn p-relations [{rels :p-relations} col]
  [:div {:class (styles/relations col)} (relations rels)])


(defn shas [{ss :shas} col]
  [:div {:style {:grid-column-start col
                 :font-size "small"
                 :display "flex"
                 :flex-direction "column"
                 :padding "0.2em"
                 :gap "0em"
                 :background-color :lavender
                 }}
   (for [[i name] (map-indexed vector ss)]
     ^{:key i} [:div name])])


(defn qi [[stage sid] col]
  (let [element (bu/stem-element sid)]
    [:div {:class (styles/qi element col)} stage]
    ))

(defn qi-legend [palace col]
  [:div {:class (styles/qi-base col)} palace])

(defn qi-stage-legend []
  (let [settings @(rf/subscribe [:settings])
        j-qs     (:Jiazi-qi-stage settings)
        s-qs     (:Stem-qi-stage settings)
        b-qs     (:Branch-qi-stage settings)
        h-qs     (:Hstems-qi-stage settings)]
    (list
     ^{:key "1x"} [qi-legend "/" 1]
     (if j-qs ^{:key "1j"} [qi-legend "j" 1])
     (if s-qs ^{:key "1s"} [qi-legend "s" 1])
     (if b-qs ^{:key "1b"} [qi-legend "b" 1])
     (if h-qs ^{:key "1h"} [qi-legend "h" 1]))))

(defn qi-hs [col i hs]
  ^{:key (str col "hs" i)} [qi hs col]
  )

(defn qi-stage-pillar [ix [p s b j hss]]
  (let [i        (+ 2 ix)
        settings @(rf/subscribe [:settings])
        j-qs     (:Jiazi-qi-stage settings)
        s-qs     (:Stem-qi-stage settings)
        b-qs     (:Branch-qi-stage settings)
        h-qs     (:Hstems-qi-stage settings)]
    (concat
     (list
      ^{:key (str i p)} [qi-legend (name p) i]
      (if j-qs ^{:key (str i "j")} [qi j i])
      (if s-qs ^{:key (str i "s")} [qi s i])
      (if b-qs ^{:key (str i "b")} [qi b i]))
     (if h-qs (map-indexed (partial qi-hs i) hss) ))
    ))


(defn qi-stage1 [stages col]
  (into [:div {:class (styles/qi-stages col)} (doall (apply concat (cons (qi-stage-legend) (map-indexed qi-stage-pillar stages))))]))


(defn nayin [{jiazi :jiazi} col]
  (let [{:keys [element description]} (bu/nayin (quot jiazi 2))]
    [:div {:class (styles/nayin element col)} description]))

(defn slug [{s :slug} col]
  [:div {:style {:grid-column-start col
                 :background-color
                 "black"
                 :color "white"
                 :text-align :center}}
   s])


(defn selectable [func {:keys [palace id] :as p} col]
  [:div {:style {:grid-column-start col}
         :on-click #(rf/dispatch [:select-pillar palace id])}
   [func p col]
   ])


(defn pillar1 [add-select i {slugg :slug sid :stem palace :palace id :id :as p}]
  (let [col      (+ i 1)
        settings @(rf/subscribe [:settings])]
    (if sid
      [
       ^{:key (str col "sl")}   (if add-select [selectable slug p col] [slug p col])
       ^{:key (str col "st")}   (if add-select [selectable stem p col] [stem p col])
       ^{:key (str col "br")} (if add-select [selectable branch p col] [branch p col])
       (if (:Hidden-stems settings) ^{:key (str col "hs")} [hstems p col])
       (if (:Nayin settings) ^{:key (str col "ny")} [nayin p col])
       (if (:N-relations settings) ^{:key (str col "nr")} [n-relations p col])
       (if (:P-relations settings) ^{:key (str col "pr")} [p-relations p col])
       (if (:Sha settings) ^{:key (str col "sh")}[shas p col])
       (if (:Natal-qi-stages settings) ^{:key (str col "nq")} [qi-stage1 (:qi-stages p) col])
       (if (:Time-qi-stages settings) ^{:key (str col "tq")}  [qi-stage1 (:r-qi-stages p) col])
       ]
      
      [^{:key (str col "sl")}  (if add-select [selectable slug p col] [slug p col])
       ^{:key (str col "sl2")} (if add-select
                                 [:div {:style {:grid-column-start col
                                                :grid-row "2 / 4"
                                                :background-color :grey}
                                        :on-click #(rf/dispatch [:select-pillar palace id])}]
                                 [:div {:style {:grid-column-start col
                                                :grid-row "2 / 4"
                                                :background-color :grey
                                                :min-height "7em"
                                                }}])])))


(defn pillars1 [ps add-select]
  (let [settings @(rf/subscribe [:settings])]
    (into [:div {:class (styles/pillars)}] cat (map-indexed (partial pillar1 add-select) ps)
          )))


(defn norm-scores [v]
  (let [total (apply + v)
        norm-total (/ total 100)]
    (map #(/ % norm-total) v)))


(defn score [scs]
  (let [sorted-scs (reverse (sort-by #(nth % 1) scs))]
    [:table {:style {:margin-right "3em"}}
     [:tbody
      (for [[name s ns] sorted-scs]
        ^{:key name}
        [:tr
         [:td name]
         [:td (str (.toFixed ns 1) "%")]
         [:td (str "(" (.toFixed s 0) ")")]
         ]
        )]]))


(defn scores [{g-scores :god-scores e-scores :elem-scores sw-scores :strong-weak-scores}]
  [:div {:style {:display "flex" :align-items :start}}
   [score (map list bu/GOD-NAMES g-scores (norm-scores g-scores))]
   [score (map list bu/FACTOR-NAMES e-scores (norm-scores e-scores))]
   [score (map list ["Supporting" "Weakening"] sw-scores (norm-scores sw-scores))]
   ])


(defn selected-pillars []
  (let [s-pillars @(rf/subscribe [:selected-pillars])
        l         (s-pillars :l)
        y         (s-pillars :y)
        m         (s-pillars :m)
        d         (s-pillars :d)
        h         (s-pillars :h)
        ps        (filter identity [l y m d h])]
    [:div 
     [:h3 "Time pillars"]
     [pillars1 ps false]
     ]
    ))


(defn tab-button [tab-name text]
  (let [current-tab @(rf/subscribe [:current-tab])
        active      (= current-tab tab-name)
        ymdh-tab    (contains? #{:y :m :d :h} tab-name)
        disabled    (and ymdh-tab (not (tab-name @(rf/subscribe [:chart]))))
        ]
    [:button {:class (if active (styles/active) "")
              :disabled (if disabled true false )
              :on-click #(rf/dispatch [:tab-change tab-name])} text]
    ))


(defn tabs []
  (let [chart       @(rf/subscribe [:chart])
        current-tab @(rf/subscribe [:current-tab])]
    (case current-tab
      :score [scores chart]
      :l     [pillars1 (vals (:l chart)) true]
      :y     [pillars1 (vals (:y chart)) true]
      :m     [pillars1 (vals (:m chart)) true]
      :d     [pillars1 (vals (:d chart)) true]
      :h     [pillars1 (vals (:h chart)) true])
    ))


(defn chart []
  (let [chart @(rf/subscribe [:chart])]
    (if chart
      [:div 
       [:div {:style {:display "flex" :gap "4em" :overflow-x "auto"}}
        [:div
         [:h3  "Natal chart"]
         [pillars1 (:natal-pillars chart) false]]
        [selected-pillars]]

       [:div {:style {:display "flex" :gap "0.5em" :margin-top "1em" :margin-bottom "1em"}}
        ^{:key :score} [tab-button :score "Scores"]
        ^{:key :l} [tab-button :l "Luck pillars"]
        ^{:key :y} [tab-button :y "Year pillars"]
        ^{:key :m} [tab-button :m "Month pillars"]
        ^{:key :d} [tab-button :d "Day pillars"]
        ^{:key :h} [tab-button :h "Hour pillars"]
        ]

       [tabs]])))
