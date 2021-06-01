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
   ))

(defn num-input [id placeholder width mx-length min max fields hour-field]
  [:input {:id id
           :type :number
           :placeholder placeholder
           :style {:width width }
           :maxLength mx-length
           :min min
           :max max
           :disabled (if hour-field (:no-hour @fields) false)
           :value (id @fields)
           :on-change #(swap! fields
                              assoc id (-> % .-target .-value))}])

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
  (let [schema (if (:no-hour fields) date-schema datetime-schema)
        parser (if (:no-hour fields) parse-date parse-datetime)
        input-errors (st/validate fields schema)
        gender (:gender fields)]
    (if (not (first input-errors))
      (try
        [nil (-> fields
                 (assoc :date (parser fields))
                 (assoc :pgender (if (= gender "male") true false))
                 )]
        (catch js/Error e [{:date (ex-message e)}])
        )
      input-errors)
    ))


(defn calculate! [fields]
  (let [[input-errors pfields] (validate-input @fields)]
    (if input-errors
      (js/alert (string/join "\n"  (vals input-errors)))
      (rf/dispatch [:calculate (:date pfields) (:pgender pfields) (:no-hour pfields)]))
    ))


(defn date-picker []
  (let [fields (r/atom {:gender "male"
                        :year "1990"
                        :month "9"
                        :day "6"
                        :hour "23"
                        :minutes "10"
                        :no-hour false})]
    (fn []
      [:div {:style {:display :flex :gap "0.5em" :align-items :baseline :flex-wrap :wrap}} 
       [:label  "Date"]
       [num-input :year "y" "3.5em" "4" "1600" "2200" fields]
       [num-input :month "m" "2.7em" "2" "1" "12" fields]
       [num-input :day "d" "2.7em" "2" "1" "31" fields]
       [:label "Time"]
       [num-input :hour "h" "2.7em" "2" "0" "23" fields true]
       [num-input :minutes "m" "2.7em" "2" "0" "59" fields true]
       
       [:label {:for "hour-unknown"} "Hour unknown"]
       [:input {:id "hour-unknown"
                :type "checkbox"
                :checked (:no-hour @fields)
                :on-change #(do
                              (swap! fields assoc :hour "")
                              (swap! fields assoc :minutes "")
                              (swap! fields assoc :no-hour (.. % -target -checked)))}]
       
       [:label {:for "gender"} "Gender"]
       [:select {:id "gender"
                 :type "select"
                 :value (:gender @fields)
                 :on-change #(swap! fields assoc :gender (.. % -target -value))}
        [:option {:value "male"} "male"]
        [:option {:value "female"}"female"]]
       
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
   [:label {:for :nayin} (if display-name display-name name)]])

(defn settings-widget []
  (let [show (r/atom false)]
    (fn []
      (let [settings @(rf/subscribe [:settings])]
        (if @show
          [:div {:style {:display :flex :gap "0.2em" :align-items :baseline :flex-direction :column}}
           [:button {:on-click #(reset! show false)} "Hide settings"]
           [checkbox :Nayin settings]
           [checkbox :Hidden-stems settings "Hidden stems"]
           [checkbox :Sha settings]
           [checkbox :Natal-qi-stages settings "Natal pillar qi stage"]
           [checkbox :Time-qi-stages settings "Time pillar qi stage"]
           [:ul {:style {:margin-block "0em 0em"}}
            [checkbox :Jiazi-qi-stage settings "Jiazi qi stage"]
            [checkbox :Stem-qi-stage settings "Stem qi stage"]
            [checkbox :Branch-qi-stage settings "Branch qi stage"]
            [checkbox :Hstems-qi-stage settings "Hidden stems qi stage"]]
           
           [checkbox :P-relations settings]
           [checkbox :N-relations settings]

           [:div {:style {:display :flex :gap "0.2em" :align-items :baseline}}
            [:label {:for :palace-bg} "Palace background"]
            [:select {:id :palace-bg
                      :type "select"
                      :value (:palace-bg settings)
                      :on-change #(rf/dispatch [:set-settings :palace-bg (.. % -target -value)])}
             [:option {:value :element} "5 element"]
             [:option {:value :none} "None"]
             [:option {:value :usefull} "Usefull"]]]
           ]
          [:button {:on-click #(reset! show true)} "Show settings"]
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

(defn init []
  (dev-setup)
  (rf/dispatch [:app/initialize])
  (mount-root)
  (go
    (let [;;c (:body (<! (http/get "calendar.json")))
          c2 (:body (<! (http/get "calendar2.json")))]
      ;;(reset! cal/cal c)
      (cal/loadcal! c2)
      (rf/dispatch [:calendar-loaded])
      (println "calendar loaded"))))

