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

(defn stem [{sid :stem dm :dm palace :palace}]
  (let [element  (bu/stem-element sid)
        usefulls @(rf/subscribe [:usefull-elem])
        usefull  (usefulls element)
        bg-setting (:palace-bg @(rf/subscribe [:settings]))
        bg-style (palace-bg-style bg-setting element usefull)]
    [:div {:class [(styles/palace 2) bg-style]}
     (bu/STEM-HTML sid)
     (if (not= palace :D)[:div {:class (styles/palace-god)} (bu/GOD-NAMES (bu/stem-god dm sid))])]))


(defn branch [{bid :branch dm :dm void :void}]
  (let [element (bu/branch-element bid)
        usefulls @(rf/subscribe [:usefull-elem])
        usefull (usefulls element)
        bg-setting (:palace-bg @(rf/subscribe [:settings]))
        bg-style (palace-bg-style bg-setting element usefull)]
    [:div {:class [(styles/palace 3) bg-style]}
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


(defn hstems [{bid :branch dm :dm}]
  (let [stems         (bu/HIDDEN-STEMS bid)
        order         (if (= (count stems) 3) [1 0 2] [0 1])
        indexed-stems (map vector order stems)]
    [:div {:style {:grid-row-start 4
                   :display "flex "
                   :font-size "1.5em"
                   :justify-content :center
                   :gap "0.2em"}}
     (for [[i hs] indexed-stems] ^{:key i} [hstem hs i dm])]))


(defn symbol-str [s]
  (str (rest (str s))))


(defn relations [{rels :relations}]
  [:div {:style {:grid-row-start 6
                 :font-size "small"
                 :display "flex"
                 :flex-direction "column"
                 :align-items "flex-start"
                 :gap "0.2em"
                 :background-color "lavender"
                 :padding "0.2em"
                 }}
   (for [[i {:keys [rtype palaces element]}] (map-indexed vector rels)]
     ^{:key i}
     [:div {:class (styles/relation (if element  (styles/element-colors element)))}
      (str (name rtype) " " (string/join " " (map name palaces)))])]) 


(defn shas [{ss :shas}]
  [:div {:style {:grid-row-start 7
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

(defn qi-stage-pillar [ix [p s b j hss]]
  (let [i        (+ 2 ix)
        settings @(rf/subscribe [:settings])
        j-qs     (:Jiazi-qi-stage settings)
        s-qs     (:Stem-qi-stage settings)
        b-qs     (:Branch-qi-stage settings)
        h-qs     (:Hstems-qi-stage settings)]
    (list
     ^{:key (str i p)} [qi-legend (name p) i]
     (if j-qs ^{:key (str i "j")} [qi j i])
     (if s-qs ^{:key (str i "s")} [qi s i])
     (if b-qs ^{:key (str i "b")} [qi b i])
     ;;(if h-qs ^{:key "1h"} [qi h i])
     )))


(defn qi-stage1 [stages row-start]
  (into [:div {:class (styles/qi-stages row-start)} (doall (apply concat (cons (qi-stage-legend) (map-indexed qi-stage-pillar stages))))]))

(defn qi-stage [stages row-start]
  [:div {:class (styles/qi-stages row-start)}
   (for [[i [p s b j hss]] (map-indexed vector stages)]
     
     [:div {:style {:display "flex" :flex-direction "column" :gap "0.2em" :justify-content "flex-start" }}
      ^{:key 0} [qi j]      
      ^{:key 1} [qi s]
      ^{:key 2} [qi b]
      (for [[j hs] (map-indexed vector hss)] ^{:key (str i j)} [qi hs])
      ])])


(defn nayin [{jiazi :jiazi}]
  (let [{:keys [element description]} (bu/nayin (quot jiazi 2))]
    [:div {:class (styles/nayin element)} description]))

(defn slug [{s :slug}]
  [:div {:style {:grid-row-start 1
                 :background-color
                 "black"
                 :color "white"
                 :text-align :center}}
   s])


(defn selectable [func {:keys [palace id] :as p} grid-row]
  [:div {:style {:grid-row grid-row}
         :on-click #(rf/dispatch [:select-pillar palace id])}
   [func p]
   ])


(defn pillar1 [add-select {slugg :slug sid :stem palace :palace id :id :as p}]
  (let [settings @(rf/subscribe [:settings])]
    (if sid
      [
       ^{:key (str slugg 1)}(if add-select [selectable slug p 1] [slug p])
       ^{:key (str slugg 2)}(if add-select [selectable stem p 2] [stem p])
       ^{:key (str slugg 3)}(if add-select [selectable branch p 3] [branch p])
       (if (:Hidden-stems settings) ^{:key (str slugg 4)} [hstems p])
       (if (:Nayin settings) ^{:key (str slugg 5)} [nayin p])
       (if (:Relations settings) ^{:key (str slugg 6)} [relations p])
       (if (:Sha settings) ^{:key (str slugg 7)}[shas p])
       (if (:Natal-qi-stages settings) ^{:key (str slugg 8)} [qi-stage1 (:qi-stages p) 8])
       (if (:Time-qi-stages settings) ^{:key (str slugg 9)}  [qi-stage1 (:r-qi-stages p) 9])
       ]
      
      [^{:key (str slugg 1)} (if add-select [selectable slug p 1] [slug p])
       ^{:key (str slugg 2)} (if add-select
                               [:div {:style {:grid-row "2 / 10"
                                              :background-color :grey}
                                      :on-click #(rf/dispatch [:select-pillar palace id])}]
                               [:div {:style {:grid-row "2 / 10"}}])])))


(defn pillars1 [ps add-select]
  (let [settings @(rf/subscribe [:settings])]
    (into [:div {:class (styles/pillars)}] cat (map (partial pillar1 add-select) ps)
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
