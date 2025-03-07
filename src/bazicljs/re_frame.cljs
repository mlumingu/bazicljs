(ns bazicljs.re-frame
  (:require
   [re-frame.core :as rf]   

   [bazicljs.bazi :as b]

   [cljs-time.core :as tc]
   [cljs-time.format :as tf]
   [clojure.string :as string]
   ))




(rf/reg-event-fx
 :app/initialize
 (fn [_ [_ date-fields]]
   {:db {:calendar-loaded? true
         :current-tab      :score
         :settings         {:Nayin true
                            :Hidden-stems true
                            :Sha true
                            :Natal-qi-stages false
                            :Time-qi-stages false
                            :P-relations true
                            :N-relations true
                            :palace-bg "element"
                            :Jiazi-qi-stage false
                            :Stem-qi-stage true
                            :Branch-qi-stage false
                            :Hstems-qi-stage true
                            :Pillar-qi false
                            :Stem-branch-names false
                            :Harmony-branch-color false
                            :Off-12-const-28 true}
         :date-fields       date-fields
         }}))

(rf/reg-sub
 :calendar-loaded?
 (fn [db _]
   (:calendar-loaded? db)))


(rf/reg-event-db
 :calendar-loaded
 (fn [db [_ _]]
   (assoc db :calendar-loaded? false)))

(rf/reg-event-db
 :tab-change
 (fn [db [_ tab-name]]
   (assoc db :current-tab tab-name)
   ))

(rf/reg-sub
 :current-tab
 (fn [db _]
   (:current-tab db)))

(rf/reg-sub
 :chart
 (fn [db _]
   (:chart db)))

(rf/reg-sub
 :selected-pillars
 (fn [db _]
   (:selected-pillars db)))

(rf/reg-sub
 :usefull-elem
 (fn [db _]
   (:usefull-elem (:chart db))))

(rf/reg-sub
 :dm
 (fn [db _]
   (:dm (:chart db))))

(rf/reg-event-db
 :set-settings
 (fn [db [_ name value]]
   (assoc-in db
             [:settings name] value)))

(rf/reg-sub
 :settings
 (fn [db _]
   (:settings db)))

(rf/reg-event-db
 :set-date-fields
 (fn [db [_ name value]]
   (assoc-in db
             [:date-fields name] value)))


(rf/reg-sub
 :date-fields
 (fn [db _]
   (:date-fields db)))



(rf/reg-event-db
 :calculate
 (fn [db [_ dt is-male no-hour0]]
   (assoc db
          :birth-date dt
          :chart (b/chart dt is-male no-hour0)
          :selected-pillars {}
          :current-tab :score)))

(rf/reg-event-db
 :select-pillar
 (fn [db [_ palace id]]
   (let [pillar         (get-in db [:chart palace id])

         n-pillars      (get-in db [:chart :natal-pillars])
         sub-palace     (case palace
                          :l :y
                          :y :m
                          :m :d
                          :d :h
                          :h nil)
         sub-palaces    (case palace
                          :l {:y nil :m nil :d nil :h nil}
                          :y {:m nil :d nil :h nil}
                          :m {:d nil :h nil}
                          :d {:h nil}
                          :h {})
         nested-pillars (case palace
                          :l (b/year-pillars pillar)
                          :y (b/month-pillars pillar)
                          :m (b/day-pillars pillar)
                          :d (b/hour-pillars pillar)
                          :h nil)]
     (-> db
         (update-in [:chart] merge sub-palaces)         
         (assoc-in [:chart sub-palace] nested-pillars)
         (assoc-in [:selected-pillars palace] pillar)
         (update-in [:selected-pillars] merge sub-palaces)
         )))
 )
