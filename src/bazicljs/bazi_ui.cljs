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




(defn palace-bg-style [setting element usefull is-day-p]
  (case setting
    "element" (styles/element-color element)
    "usefull" (if is-day-p (styles/none-palace) (styles/usefull-color usefull))
    "none"    (styles/none-palace)))

(defn stem [{sid :stem dm :dm palace :palace {[st-qi x] 0} :pillar-qi} col]
  (let [element  (bu/stem-element sid)
        usefulls @(rf/subscribe [:usefull-elem])
        usefull  (usefulls element)
        bg-setting (:palace-bg @(rf/subscribe [:settings]))
        pillar-qi  (:Pillar-qi @(rf/subscribe [:settings]))
        s-names    (:Stem-branch-names @(rf/subscribe [:settings]))
        s-name     (bu/STEM-NAMES sid)
        is-day-p (= palace :D)
        bg-style (palace-bg-style bg-setting element usefull is-day-p)
        ]
    [:div {:class [  (styles/palace col) bg-style]}
     (bu/STEM-HTML sid)
     (if s-names [:div {:class (styles/palace-names)} s-name])
     (if (not is-day-p) [:div {:class (styles/palace-god)} (bu/GOD-NAMES (bu/stem-god dm sid))])
     (if pillar-qi [:div {:class (styles/palace-qi)} st-qi])]))


(defn branch [{bid :branch dm :dm void :void harmony :harmony} col]
  (let [element      (bu/branch-element bid)
        usefulls     @(rf/subscribe [:usefull-elem])
        harmony-bc   (:Harmony-branch-color @(rf/subscribe [:settings]))
        element      (if harmony-bc harmony element)
        usefull      (usefulls element)
        bg-setting   (:palace-bg @(rf/subscribe [:settings]))
        b-names      (:Stem-branch-names @(rf/subscribe [:settings]))
        b-name       (bu/BRANCH-NAMES bid)
        bg-style     (palace-bg-style bg-setting element usefull false)
        ]
    [:div {:class [(styles/palace col) bg-style]}
     (bu/BRANCH-HTML bid)
     (if b-names [:div {:class (styles/palace-names)} b-name])
     [:div {:class (styles/palace-god)} (bu/GOD-NAMES (bu/branch-god dm bid))]
     (if void [:div {:class (styles/void)} "DE"])]))


(defn hstem [sid order dm qi]
  (let [element (bu/stem-element sid)
        usefulls @(rf/subscribe [:usefull-elem])
        usefull (usefulls element)
        bg-setting (:palace-bg @(rf/subscribe [:settings]))
        bg-style (palace-bg-style bg-setting element usefull false)
        s-names    (:Stem-branch-names @(rf/subscribe [:settings]))
        s-name     (bu/STEM-NAMES sid)
        shtml (bu/STEM-HTML sid)
        pillar-qi  (:Pillar-qi @(rf/subscribe [:settings]))]
    [:div {:class [(styles/hstem order) bg-style]} shtml
     (if s-names [:div {:class (styles/hs-god)} s-name])
     [:div {:class (styles/hs-god)} (bu/GOD-NAMES (bu/stem-god dm sid))]
     (if pillar-qi [:div {:class (styles/hs-god)} qi])]))


(defn hstems [{bid :branch dm :dm {hss-qi 3} :pillar-qi} col]
  (let [stems         (bu/HIDDEN-STEMS bid)
        order         (if (= (count stems) 3) [1 0 2] [0 1])
        qis           (map first hss-qi)
        indexed-stems (map vector order stems qis)
        ]
    [:div {:class (styles/hstems col)}
     (for [[i hs qi] indexed-stems] ^{:key i} [hstem hs i dm qi])
     ]))


(defn symbol-str [s]
  (str (rest (str s))))

(contains? #{1 2} 3)

(defn relation-palace-name [rel pillar]
  (let [s-or-b (:idtype rel)
        id (s-or-b pillar)
        element (if (= s-or-b :stem)
                  (bu/stem-element id)
                  (bu/branch-element id))
        name (:palace pillar)
        ]
    [:span
     " "
     [:span {:class (styles/relation (styles/element-colors element))} name]]))

(defn relations [rels]
  (for [[i {:keys [rtype palaces pillars element rel]}] (map-indexed vector rels)]
    ^{:key i}
    [:div
     [:span {:class (styles/relation (if element  (styles/element-colors element)))}
      (str (name rtype))]
     (for [pillar pillars] ^{:key (:palace pillar)} (relation-palace-name rel pillar))
     ]))

(defn n-relations [{ rels :n-relations} col]
  [:div {:class [(styles/grid-base) (styles/sha col)]}
   (relations rels)
   ])

(defn p-relations [{rels :p-relations} col]
  [:div {:class [(styles/grid-base)(styles/relations col)]} (relations rels)])


(defn shas [{ss :shas css :cshas} col]
  [:div {:class [(styles/grid-base)(styles/sha col)]}
   (for [[i name] (map-indexed vector (concat ss css))]
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

(defn qi-stage-pillar [ix [p s b j hss :as plr]]
  (if plr
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
      )))


(defn qi-stage1 [stages col]
  (into [:div {:class [(styles/grid-base) (styles/qi-stages col)]} (doall (apply concat (cons (qi-stage-legend) (map-indexed qi-stage-pillar stages))))]))


(defn nayin [{jiazi :jiazi} col]
  (let [{:keys [element description]} (bu/nayin (quot jiazi 2))]
    [:div {:class [(styles/grid-base) (styles/nayin col element)]} description]))

(defn off-12-const-28 [{off-12 :off-12 const-28 :const-28 palace :palace weekday :weekday} col]
  (if (contains? #{:d :D} palace)
    (let [off-12-name   (bu/OFFICERS off-12)
          const-28-name (bu/CONSTELLATIONS const-28)
          weekday-name  (cal/WEEKDAYS weekday)]
      [:div {:class [(styles/grid-base) (styles/off-12-const-28 col)]}
       [:div weekday-name]
       [:div (str (+ 1 off-12) " " off-12-name)]
       [:div (str (+ 1 const-28) " " const-28-name)]])))

(defn slug [{s :slug} col]
  [:div {:class (styles/slug col)} s])


(defn selectable [func {:keys [palace id] :as p} col]
  [:div {:style {:grid-column-start col :cursor :pointer}
         :on-click #(rf/dispatch [:select-pillar palace id])}
   [func p col]
   ])

(defn empty-luck [p col]
  [:div {:class (styles/empty-luck col)}])

(defn cond-select [add-select component pillar col]
  (if add-select [selectable component pillar col] [component pillar col]))

(defn pillar1 [add-select i {slugg :slug sid :stem palace :palace id :id :as p}]
  (let [col      (+ i 1)
        settings @(rf/subscribe [:settings])]
    (if sid
      [
       ^{:key (str col "sl")} [cond-select add-select slug p col]
       ^{:key (str col "st")} [cond-select add-select stem p col]
       ^{:key (str col "br")} [cond-select add-select branch p col]
       (if (:Hidden-stems settings) ^{:key (str col "hs")}
         [cond-select add-select hstems p col])
       (if (:Nayin settings) ^{:key (str col "ny")}
         [cond-select add-select nayin p col])
       (if (:N-relations settings) ^{:key (str col "nr")}
         [n-relations p col])
       (if (:P-relations settings) ^{:key (str col "pr")}
         [p-relations p col])
       (if (:Sha settings) ^{:key (str col "sh")}
         [shas p col])
       (if (:Natal-qi-stages settings) ^{:key (str col "nq")}
         [qi-stage1 (:qi-stages p) col])
       (if (:Time-qi-stages settings) ^{:key (str col "tq")}
         [qi-stage1 (:r-qi-stages p) col])
       (if (:Off-12-const-28 settings) ^{:key (str col "o12c28")}
         [off-12-const-28 p col])
       ]
      
      [^{:key (str col "sl")}  (if add-select [selectable slug p col] [slug p col])
       ^{:key (str col "sl2")} (if add-select [selectable empty-luck p col] [empty-luck p col])])))


(defn pillars1 [ps add-select]
  (let [settings @(rf/subscribe [:settings])]
    (into [:div {:class (styles/pillars)}] cat (map-indexed (partial pillar1 add-select) ps)
          )))


(defn norm-scores [v]
  (let [total (apply + v)
        norm-total (/ total 100)]
    (map #(/ % norm-total) v)))


(defn score [scs]
  [:table {:style {:margin-right "3em"}}
   [:tbody
    (for [[s ns n1 st1 n2 st2] scs]
      ^{:key n1}
      [:tr
       [:td  {:class st1} n1]
       (if n2 [:td {:class st2} n2])
       [:td (str (.toFixed ns 1) "%")]
       [:td (str "(" (.toFixed s 0) ")")]
       ]
      )]])


(defn god-scores [{dm :dm g-scores :god-scores}]
  (let [norm-score  (norm-scores g-scores)
        sids        (bu/god-stems dm)
        e-ids       (map bu/stem-element sids)
        stem-styles (map styles/element-text-color e-ids)
        stem-names  (map bu/STEM-HTML sids)

        usefulls    @(rf/subscribe [:usefull-elem])
        usefullss   (map usefulls e-ids)
        god-styles  (map styles/useful-text-color usefullss)

        rows        (map vector g-scores norm-score stem-names stem-styles bu/GOD-NAMES god-styles)
        sorted-rows (reverse (sort-by first rows))
        ]
    [score sorted-rows])
  )

(defn element-scores [{dm :dm e-scores :elem-scores}]
  (let [norm-score (norm-scores e-scores)
        e-ids      (bu/factor-elements dm)
        e-styles   (map styles/element-text-color e-ids)
        e-names    (map bu/ELEMENT-NAMES e-ids)

        usefulls   @(rf/subscribe [:usefull-elem])
        usefullss  (map usefulls e-ids)
        f-styles   (map styles/useful-text-color usefullss)

        rows       (map vector
                        e-scores norm-score
                        e-names e-styles
                        bu/FACTOR-NAMES f-styles)
        sorted-rows (reverse (sort-by first rows))]
    [score sorted-rows]))

(defn useful-scores [{sw-scores :strong-weak-scores}]
  (let [[s w]       sw-scores
        norm-score (norm-scores sw-scores)
        sw-names   ["Supporting" "Weakening"]
        s-is-u     (< s w)
        u-names    (if s-is-u ["Useful" "Unuseful"])
        u          (if s-is-u [true false] [false true])
        u-styles   (map styles/useful-text-color u)
        rows       (map vector sw-scores norm-score sw-names u-styles)
        sorted-rows (reverse (sort-by first rows))
        ]
    [score sorted-rows])
  )

(defn scores [p]
  [:div {:style {:display "flex" :align-items :start}}
   [god-scores p]
   [element-scores p]
   [useful-scores p]
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
    [:button {:class [ (if active (styles/active))]
              :disabled (if disabled true false )
              :on-click #(rf/dispatch [:tab-change tab-name])} text]
    ))


(defn tabs []
  (let [chart       @(rf/subscribe [:chart])
        current-tab @(rf/subscribe [:current-tab])]
    (case current-tab
      :score [scores chart]
      :life  [:div [:p [:i "Warning: this is an experimental feature. The calculation might not be correct."]] [pillars1 (:life-pillars chart) true]]
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
        ^{:key :life} [tab-button :life "Life pillars"]
        ^{:key :l} [tab-button :l "Luck pillars"]
        ^{:key :y} [tab-button :y "Year pillars"]
        ^{:key :m} [tab-button :m "Month pillars"]
        ^{:key :d} [tab-button :d "Day pillars"]
        ^{:key :h} [tab-button :h "Hour pillars"]
        ]

       [tabs]])))
{}
