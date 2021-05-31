(ns bazicljs.calendar
  (:require
   [cljs-http.client :as http]
   [cljs-time.core :as time]
   [cljs-time.coerce :as ctime]   
   [cljs-time.format :as ftime]
   [clojure.set :refer [map-invert]]
   ))

(def cal (atom nil))
(def cal2 (atom nil))


(defn parse-time [dt]
  (ftime/parse (ftime/formatter "yyyy-MM-dd HH:mm") dt))


(defn next-pillar [p]
  (let [s (p 0)
        b (p 1)]
    [(rem (+ s 1)
          10)
     (rem (+ b 1)
          12)]))


(defn gen-days [first-dp m-start]
  (let [end1   (time/plus (time/at-midnight m-start) (time/days 1))
        ends   (iterate #(time/plus % (time/days 1)) end1)
        starts (cons m-start ends)
        dps    (iterate next-pillar first-dp)
        days   (map hash-map (repeat :start) starts (repeat :end) ends (repeat :dp) dps)
        ]
    days))


(defn parse-month [m1 m2]
  (let [m-start (parse-time (first m1))
        m-end   (parse-time (first m2))
        mst     (m1 1)
        mbr     (m1 2)
        dst     (m1 3)
        dbr     (m1 4)
        days    (gen-days [dst dbr] m-start)]
    {:start m-start
     :end   m-end
     :mp    [mst mbr]
     :days  days}))


(defn floor-to-year [dt]
  (time/floor dt time/year))
(defn floor-to-month [dt]
  (time/floor dt time/month))
(defn compare-date [d1 d2]
  (- (ctime/to-long d1) (ctime/to-long d2)))

(defn map-months [ms]
  (->> ms
       (map :start)
       (map floor-to-month)
       (map #(vector %2 %1) ms)
       (into (sorted-map-by compare-date))))

(defn map-years [ys]
  (->> ys
       (map :start)
       (map floor-to-year)
       (map #(vector %2 %1) ys)
       (into (sorted-map-by compare-date))))

(defn parse-year [y1 y2]
  (let [y-start    (parse-time (first y1))
        y-end      (parse-time (first y2))
        tail-m     [(first y2)]
        ms         (conj (y1 3) tail-m)
        p-ms       (map parse-month ms (rest ms))
        ms-map     p-ms
        ;;ms-map     (map-months p-ms)
        yst        (y1 1)
        ybr        (y1 2)]
    {:start  y-start
     :end    y-end
     :yp     [yst ybr]
     :months ms-map}))

(defn loadcal! [cal]
  (->> (map parse-year cal (rest cal))
       ;;(map-years)
       (reset! cal2)))


(defn get-from-cal [cal floor-func minus-func dt]
  (let [fdt1 (floor-func dt)
        fdt2 (time/minus fdt1 (minus-func 1))
        y1   (cal fdt1)
        y2   (cal fdt2)]
    (if (time/before? dt (:start y1))
      y2
      y1)))


(defn cal-year [dt]
  (get-from-cal @cal2 floor-to-year time/years dt))
(defn cal-month [cal dt]
  (get-from-cal cal floor-to-month time/months dt))


(defn gregorian-to-solar-ymd [dt]
  (let [y (first (drop-while #(not (time/after? (:end %) dt)) @cal2))
        m (first (drop-while #(not (time/after? (:end %) dt)) (:months y)))
        d (first (drop-while #(not (time/after? (:end %) dt)) (:days m)))

        [ys yb] (:yp y)
        [ms mb] (:mp m)
        [ds db] (:dp d)]
    [ds db ms mb ys yb]
    ))


;;(def d0 (time/date-time 1990 9 6 23 10))
;;(def m (gtsymd d0))


(defn gregorian-to-solar-ymd0 [dt]
  (let  [y (cal-year dt)
         m (cal-month (:months y) dt)
         
         first-dp (:dp m)         
         mstart   (time/plus (time/at-midnight (:start m)) (time/days 1))
         
         ts  (iterate #(time/plus % (time/days 1)) mstart)           
         dps (iterate next-pillar first-dp)
         c   (map vector ts dps)
         dp  (second (first (filter #(time/after? (% 0) dt) c)))

         [ys yb]  (:yp y)
         [ms mb]  (:mp m)
         [ds db]  dp]
    [ds db ms mb ys yb]))


(defn hour-branch [h]
  (rem (Math/ceil (/ h 2)) 12))


(defn hour-stem [ds h]
  (let [hb (hour-branch h)]
    (rem (+ hb
            (* ds 2)
            (* (Math/floor (/ h 23))
               2))
         10)
    ))


(defn hour-pillar [ds h]
  [(hour-stem ds h) (hour-branch h)])


(defn previous-pillar [p]
  (let [s (p 0)
        b (p 1)]
    [(rem (+ (- s 1) 10)
          10)
     (rem (+ (- b 1) 12)
          12)]))


(defn days-to-next-prev-month-1 [delta-date delta ms days-diff]
  (let [delta-pillars (gregorian-to-solar-ymd delta-date)
        delta-ms (delta-pillars 2)]
    (if (= ms delta-ms)
      (days-to-next-prev-month-1 (time/plus delta-date (time/days delta))
                                 delta
                                 ms
                                 (inc days-diff))
      days-diff)))

(defn days-to-next-prev-month [date delta]
  (let [pillars (gregorian-to-solar-ymd date)
        ms (pillars 2)]
    (days-to-next-prev-month-1 date delta ms 0)))


(defn use-forward-direction? [is-male yang-year]
  (or (and is-male yang-year)
      (and (not is-male)
           (not yang-year))))


(defn luck-pillars [date is-male]
  (let [natal-pillars (gregorian-to-solar-ymd date)
        ys (natal-pillars 4)
        mp [(natal-pillars 2) (natal-pillars 3)]
        yang-year (= (rem ys 2) 0)]
    (if (use-forward-direction? is-male yang-year)
      (iterate next-pillar (next-pillar mp))
      (iterate previous-pillar (previous-pillar mp)))))

(defn luck-pillar-start-delta [is-male yang-year]
  (if (use-forward-direction? is-male yang-year)
    1
    -1))

(defn luck-pillar-start-ages [date is-male]
  (let [ys ((gregorian-to-solar-ymd date) 4)
        yang-year (= (rem ys 2) 0)
        delta (luck-pillar-start-delta is-male yang-year)
        days-diff (days-to-next-prev-month date delta)
        start-age (Math/floor (/ days-diff 3))]
    (iterate (partial + 10) start-age)))


(defn gregorian-to-solar [date]
  (let [solar (gregorian-to-solar-ymd date)
        ds (solar 0)
        h (time/hour date)
        hb (hour-branch h)
        hs (hour-stem ds h)]
    (into [hs hb] solar)))

(defn natal-slugs [date]
  (let [y (time/year date)
        m (time/month date)
        d (time/day date)
        h (str (time/hour date) ":" (time/minute date))]
    [h d m y]))


(defn filter-pillars [ps start end]
  (let [ps (drop-while #(not (time/after? (:end %) start)) ps)
        ps (take-while #(time/before? (:start %) end) ps)

        ps (map #(assoc % :pstart (% :start)) ps)
        ps (map #(assoc % :pend (% :end)) ps)
        ps (into [] ps)
        ps (assoc-in ps [0 :pstart] start)
        ps (assoc-in ps [(- (count ps) 1) :pend] end)
        
        ;;ps (assoc-in ps [0 :start] start)
        ;;ps (assoc-in ps [(- (count ps) 1) :end] end)
        ]
    ps))

(defn year-pillars [start end]
  (filter-pillars @cal2 start end))


(defn month-pillars [start end]
  (let [y  (first (year-pillars start end))
        ms (:months y)]
    (filter-pillars ms start end)))


(defn day-pillars [start end]
  (let [m  (first (month-pillars start end))
        ds (:days m)]
    (filter-pillars ds start end)))

(defn hour-pillars [start end ds]
  (let [midnight (time/at-midnight start)
        starts   (->> (iterate (partial + 2) 1)
                      (cons 0)
                      (map #(time/plus midnight (time/hours %))))
        ends     (rest starts)
        first-hp (hour-pillar ds 0)
        hps      (iterate next-pillar first-hp)
        packs    (map hash-map (repeat :start) starts (repeat :end) ends (repeat :hp) hps)
        ]
    (filter-pillars packs start end)
    ))











