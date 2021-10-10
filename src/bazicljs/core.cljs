(ns bazicljs.core
  (:require-macros
   [cljs.core.async.macros :refer [go]])  
  (:require
   [bazicljs.config :as config]   

   [re-frame.core :as rf]   
   [reagent.dom :as rdom]
   [reagent.core :as r]

   [bazicljs.styles :as styles]
   [bazicljs.calendar :as cal]
   [bazicljs.bazi-util :as bu]
   [bazicljs.bazi :as b]
   [bazicljs.re-frame :as brf]
   [bazicljs.bazi-ui :as bui]   

   [cljs-time.core :as tc]
   [cljs-time.format :as tf]
   [cljs-http.client :as http]   

   [clojure.string :as string]
   [struct.core :as st]

   [lambdaisland.uri :as uri]

   [goog.string :as gstring]
   [goog.string.format]
   ))

(def url-path (:path (uri/uri (-> js/window .-location .-href))))

(defn num-input [id placeholder width mx-length min max fields hour-field]
  [:input {:id id
           :type :number
           :placeholder placeholder
           :style {:width width }
           :maxLength mx-length
           :min min
           :max max
           :disabled (if hour-field (:time-unknown fields) false)
           :value (id fields)
           :on-change #(rf/dispatch [:set-date-fields id (.. % -target -value)])
           }])

(def date-schema [[:year
                   [st/required :message "Year is required"]
                   [st/string :coerce int]
                   [st/in-range 1600 2200 :message "Year must be in range 1600-2200"]]
                  [:month
                   [st/required :message "Month is required"]
                   [st/string :coerce int]
                   [st/in-range 1 12 :message "Month must be in range 1-12"]]
                  [:day
                   [st/required :message "Day is required"]
                   [st/string :coerce int]
                   [st/in-range 1 31 :message "Day must be in range 1-31"]]
                  ])

(def time-schema [[:hour
                   [st/required :message "Hour is required"]
                   [st/string :coerce int]
                   [st/in-range 0 23 :message "Hour must be in range 0-23"]]
                  [:minutes
                   [st/required :message "Minutes is required"]
                   [st/string :coerce int]
                   [st/in-range 0 59 :message "Minutes must be in range 0-59"]]])

(def datetime-schema (vec (concat date-schema time-schema)))

(defn parse-date [fields]
  (tf/parse (tf/formatter "yyyy-MM-dd")
            (str (:year fields) "-"
                 (:month fields) "-"
                 (:day fields))))

(defn parse-datetime [fields]
  (tf/parse (tf/formatter "yyyy-MM-dd HH:mm")
            (str (:year fields) "-"
                 (:month fields) "-"
                 (:day fields) " "
                 (:hour fields) ":"
                 (:minutes fields))))


(defn validate-input [fields]
  (let [schema (if (:time-unknown fields) date-schema datetime-schema)
        parser (if (:time-unknown fields) parse-date parse-datetime)
        input-errors (st/validate fields schema)
        gender (:gender fields)]
    (if (not (first input-errors))
      (try
        [nil (-> fields
                 (assoc :date (parser fields))
                 (assoc :pgender (if (= gender "m") true false))
                 )]
        (catch js/Error e [{:date (ex-message e)}])
        )
      input-errors)
    ))


(defn map->input-qs [fields]
  (let [y (:year fields)
        m (gstring/format "%02d" (:month fields))
        d (gstring/format "%02d" (:day fields))
        h (gstring/format "%02d" (:hour fields))
        ms (gstring/format "%02d" (:minutes fields))
        
        date (str y "-" m "-" d)
        time (str h "-" ms)

        gender (:gender fields)

        map (if (:time-unknown fields)
              {:date date :time false :gender gender}
              {:date date :time time :gender gender})
        
        uri (uri/map->query-string map)]
    (str url-path "?" uri)
    ))

(defn calculate! [fields]
  (let [[input-errors pfields] (validate-input fields)]
    (if input-errors
      (js/alert (string/join "\n"  (vals input-errors)))
      (do
        (. (. js/window -history) pushState "" "new calculation" (map->input-qs fields))
        (rf/dispatch [:calculate (:date pfields) (:pgender pfields) (:time-unknown pfields)])))
    ))


(def log (.-log js/console))

(defn handler [event]
  (log "popstate")
  )
(.addEventListener js/window "popstate" handler)


(defn date-picker []
  (fn []
    (let [fields @(rf/subscribe [:date-fields])] 
      [:div {:style {:display :flex :gap "0.5em" :align-items :baseline :flex-wrap :wrap}} 
       [:label  "Date"]
       [num-input :year "y" "3.5em" "4" "1600" "2200" fields]
       [num-input :month "m" "2.7em" "2" "1" "12" fields]
       [num-input :day "d" "2.7em" "2" "1" "31" fields]
       [:label "Time"]
       [num-input :hour "h" "2.7em" "2" "0" "23" fields true]
       [num-input :minutes "m" "2.7em" "2" "0" "59" fields true]

       [:label {:for "time-unknown"} "Time unknown"]
       [:input {:id "time-unknown"
                :type "checkbox"
                :checked (:time-unknown fields false)
                :on-change #(do
                              (rf/dispatch [:set-date-fields :time-unknown (.. % -target -checked)])
                              (rf/dispatch [:set-date-fields :hour ""])
                              (rf/dispatch [:set-date-fields :minutes ""]))}]

       [:label {:for "gender"} "Gender"]
       [:select {:id "gender"
                 :type "select"
                 :value (:gender fields "m")
                 :on-change #(rf/dispatch [:set-date-fields :gender (.. % -target -value)])
                 }
        [:option {:value "m"} "male"]
        [:option {:value "f"} "female"]]

       
       [:input {:type :submit
                :on-click #(calculate! fields)
                :value "calculate"
                :disabled @(rf/subscribe [:calendar-loaded?])}]])))

(defn checkbox [name settings & display-name]
  [:div {:style {:display :flex :gap "0.3em" :align-items "baseline"}}
   [:input {:id name
            :type :checkbox
            :checked (name settings)
            :on-change #(rf/dispatch [:set-settings name (.. % -target -checked)])
            }]
   [:label {:for name} (if display-name display-name name)]])

(defn settings-spacer []
  [:div {:style {:height "0.5em"}}])

(defn settings-widget []
  (let [show (r/atom false)]
    (fn []
      (let [settings @(rf/subscribe [:settings])]
        (if @show
          [:div {:style {:display :flex :gap "0.2em" :align-items :baseline :flex-direction :column}}
           [:button {:on-click #(reset! show false)} "Hide options"]

           [checkbox :Stem-branch-names settings "Stem and branch names"]
           [checkbox :Hidden-stems settings "Hidden stems"]
           [checkbox :Nayin settings]
           [checkbox :Pillar-qi settings "Pillar qi stage"]
           [:div {:style {:display :flex :gap "0.2em" :align-items :baseline}}
            [:label {:for :palace-bg} "Palace background"]
            [:select {:id :palace-bg
                      :type "select"
                      :value (:palace-bg settings)
                      :on-change #(rf/dispatch [:set-settings :palace-bg (.. % -target -value)])}
             [:option {:value :element} "5 element"]
             [:option {:value :usefull} "(Un)useful"]
             [:option {:value :none} "None"]
             ]]
           [settings-spacer]
           
           [checkbox :N-relations settings "Negative relations"]
           [checkbox :P-relations settings "Positive relations"]
           [checkbox :Sha settings "Stars"]
           [settings-spacer]

           [checkbox :Natal-qi-stages settings "Natal pillar qi stage (time pillar as reference branch)"]
           [checkbox :Time-qi-stages settings "Time pillar qi stage (natal pillar as reference branch)"]
           [:ul {:style {:margin-block "0em 0em"}}
            [checkbox :Jiazi-qi-stage settings "Jiazi qi stage"]
            [checkbox :Stem-qi-stage settings "Stem qi stage"]
            [checkbox :Branch-qi-stage settings "Branch qi stage"]
            [checkbox :Hstems-qi-stage settings "Hidden stems qi stage"]]
           [settings-spacer]
           
           ]
          [:button {:on-click #(reset! show true)} "Show options"]
          )))))


(defn home []
  [:div
   [:div {:style {:display :flex :gap "0.5em" :flex-direction "column" :align-items :flex-start}}
    [date-picker]
    [settings-widget]]
   [bui/chart]
   ])


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

;; ^:dev/after-load
(defn ^:dev/after-load mount-root []
  (let [root-el (.getElementById js/document "app")]
    (rf/clear-subscription-cache!)    
    (rdom/unmount-component-at-node root-el)
    (rdom/render [home] root-el)
    ))

(defn uri-date-fields [qm]
  (let [date   (string/split (:date qm) "-")
        y      (get date 0)
        m      (get date 1)
        d      (get date 2)
        
        time   (string/split (:time qm) "-")
        h      (get time 0 "")
        ms     (get time 1 "")
        time-unknown (if (and (int? (js/parseInt h)) (int? (js/parseInt ms))) false true)
        h      (if time-unknown "" h)
        ms     (if time-unknown "" ms)
        
        gender (:gender qm)]
    {:year  y
     :month m
     :day   d
     :hour  h
     :minutes ms
     :time-unknown time-unknown
     :gender gender}
    ))

(def now-date-fields
  (let [n (tc/time-now)]
    {:year     (str (tc/year n))
     :month    (str (tc/month n))
     :day      (str (tc/day n))
     :hour     (str (tc/hour n))
     :minutes  (str (tc/minute n))
     :time-unknown  false
     :gender   "m"}))

(defn init []
  (let [uri-qm     (uri/query-map (-> js/window .-location .-href))
        uri-fields (uri-date-fields uri-qm)]
    (dev-setup)
    (rf/dispatch [:app/initialize (if uri-qm uri-fields now-date-fields)])
    (mount-root)
    (go
      (let [;;c (:body (<! (http/get "calendar.json")))
            c2 (:body (<! (http/get "calendar2.json")))]
        ;;(reset! cal/cal c)
        (cal/loadcal! c2)
        (rf/dispatch [:calendar-loaded])
        (println "calendar loaded")
        (calculate! (if uri-qm uri-fields now-date-fields))))))

