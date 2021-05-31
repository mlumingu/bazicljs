(ns bazicljs.bazi
  (:require
   [bazicljs.calendar :as bc]
   [bazicljs.bazi-util :as bu]
   [bazicljs.sha :as bs]
   [cljs-time.core :as time]
   [cljs-time.format :as ftime]
   ))

(defn get-palace [pillars palace]
  (some #(if (= (:palace %) palace) %) pillars))


(defn cart [colls]
  (if (empty? colls)
    '(#{})
    (for [more (cart (rest colls))
          x (first colls)]
      (conj more x))))

(defn remove-one [elem coll]
  (let [[m n] (split-with (partial not= elem) coll)]
    (concat m (rest n))))


(defn relation-instances [relation pillars]
  (letfn [(instance [rel pillar]
            {:rtype (:rtype rel) :palaces (list (:palace pillar)) :element (:element rel)}
            )]
    (if (:pairs? relation)
      (map (partial instance relation) pillars)
      (list {:rtype (:rtype relation) :palaces (map :palace pillars) :element (:element relation)}))))


(defn relation [pillar pillars relation]
  (let [idtype (:idtype relation)
        id     (idtype pillar)
        rids   (:ids relation)
        id-in-r (some #{id} rids)]
    (if id-in-r
      (let [other-rids (remove-one id rids)
            matches (filter #(some #{(idtype %)} other-rids)  pillars)
            groups (group-by idtype matches)
            full-relation? (= (count other-rids) (count groups))]
        (if full-relation?
          (relation-instances relation matches))
        )
      ()
      )))


(defn separate-all [coll]
  (letfn [(separate-at [coll pos]
            (let [[a b] (split-at pos coll)]
              [(first b) (concat a (rest b))]))]
    (map (partial separate-at coll) (range (count coll)))))


(defn relations2 [rels pillars pillar]
  (mapcat (partial relation pillar pillars) rels))


(defn relations [relations pillars]
  (for [[p ps] (separate-all pillars)]
    (mapcat (partial relation p ps) relations)))


(defn jiazis [stems branches]
  (map vector
       stems
       branches))


(defn stems-qi [reverse-qi
                {t-bid :branch tp :palace :as time-pillar}
                {sid :stem bid :branch p :palace :as pillar}]
  (if (and t-bid sid bid)
    (let [bsid   (bu/STEM-FROM-BRANCH bid)
          hsids  (bu/HIDDEN-STEMS bid)
          s-qi   (bu/stem-qi t-bid sid)
          b-qi   (bu/stem-qi t-bid bsid)
          j-qi   (bu/jiazi-qi t-bid [sid bid])
          hss-qi (map (partial bu/stem-qi t-bid) hsids)
          tag    (if reverse-qi tp p)]
      (vector tag [s-qi sid] [b-qi bsid] [j-qi sid] (mapv vector hss-qi hsids)))
    nil))


(defn pillars-stems-qi [pillars time-pillar]
  (let [stages    (map (partial stems-qi false time-pillar) pillars)
        r-stages  (map #(stems-qi true % time-pillar) pillars)]
    {:qi-stages stages :r-qi-stages r-stages}))


(defn stem-score [id pos total]
  (if (= pos 0)
    [id 100]
    (case [pos total]
      [1 1] [id 100]
      
      [1 2] [id 80]
      [2 2] [id 20]
      
      [1 3] [id 60]
      [2 3] [id 20]
      [3 3] [id 20])))


(defn pillar-stem-scores [{sid :stem bid :branch palace :palace}]
  (let [hsids      (bu/HIDDEN-STEMS bid)
        num-hstems (count hsids)
        sids       (cons sid hsids)
        triples    (map list sids (range) (repeat num-hstems))
        triples2   (if (= palace :D) (rest triples) triples)]
    (map (partial apply stem-score) triples2)))


(defn update-scores [scores [i score]]
  (let [cur-score (nth scores i)
        new-score (+ cur-score score)]
    (assoc scores i new-score)))


(defn stem-scores [pillars]
  (->> pillars
       (mapcat pillar-stem-scores)
       (reduce update-scores (into [] (take 10 (repeat 0))))
       ))


(defn god-scores [pillars]
  (let [dm          (:stem (get-palace pillars :D))
        mbid        (:branch (get-palace pillars :M))
        s-scores    (stem-scores pillars)
        multipliers (bu/score-multipliers mbid)
        stem-gods   (bu/stem-gods dm)
        empty-god-scores (into [] (take 10 (repeat 0)))
        m-s-scores (map * s-scores multipliers)]
    (->> m-s-scores
         (map vector (bu/stem-gods dm))
         (reduce update-scores empty-god-scores)
                                        ;(mapv * bu/dm-score-correction-multipliers)
         )
    ))


(defn element-scores [god-scores]
  (mapv +
        (take-nth 2 god-scores)
        (take-nth 2 (rest god-scores))))


(defn strong-weak-scores [elem-scores]
  (let [p (elem-scores 0)
        o (elem-scores 1)
        w (elem-scores 2)
        i (elem-scores 3)
        r (elem-scores 4)]
    [(+ p r) (* 0.7 (+ o w i))]
    ))

(defn usefull-elem [[s-score w-score] dm]
  (let [elem (- 5 (quot dm 2))
        usefull-factors (if (< s-score w-score)
                          [true false false false true]
                          [false true true true false])]
    (into [] (take 5 (drop elem (cycle usefull-factors))))))

(take 3 (cycle [true false false true]))

(defn has-sha [{ds :stem db :branch} {pb :branch palace :palace} {:keys [stype bs]}]
  (if (= stype :b)
    (if (not= palace :D)
      (= pb (bs db))
      false
      )
    (= pb (bs ds))))

(defn calc-sha1 [dp relations p]
  (->> relations
       (filter (partial has-sha dp p))
       (map :name))
  )

(defn calc-sha [dp pillars relations]
  (map (partial calc-sha1 dp relations) pillars)
  )

(defn pillars-to-map [pillars]
  (into (sorted-map) (map #(vector (% :id) %) pillars)))


(defn natal-pillars [date no-hour]
  (let [solar     (if no-hour (bc/gregorian-to-solar-ymd date) (bc/gregorian-to-solar date))
        stems     (take-nth 2 solar)
        branches  (take-nth 2 (rest solar))
        slugs     (bc/natal-slugs date)
        slugs     (if no-hour (rest slugs) slugs)
        jiazis    (jiazis stems branches)
        jiazis    (map bu/jiazi-id jiazis)
        palaces   (take 4 bu/palace-keys)
        palaces   (if no-hour (rest palaces) palaces)
        d-jiazi   (if no-hour (first jiazis) (second jiazis))
        dm        (if no-hour (first stems) (second stems))
        voids     (map (partial bu/is-void? d-jiazi) branches)
        
        n-pillars (map hash-map
                       (repeat :id)     palaces
                       (repeat :stem)   stems
                       (repeat :branch) branches
                       (repeat :jiazi)  jiazis
                       (repeat :slug)   slugs
                       (repeat :palace) palaces
                       (repeat :void)   voids
                       (repeat :dm) (repeat dm)
                       )

        dp        (if no-hour (first n-pillars) (second n-pillars))
        shas      (calc-sha dp n-pillars bs/shas)
        shas      (map hash-map (repeat :shas) shas)
        
        relations (relations bu/natal-relations n-pillars)
        relations (map hash-map (repeat :relations) relations)
        qi-stages (map (partial pillars-stems-qi n-pillars) n-pillars)]
    
    (mapv merge n-pillars relations qi-stages shas)))


(defn day-pillar [n-pillars]
  (first (filter #(= (% :palace) :D) n-pillars)))


(defn luck-pillars [date is-male n-pillars]
  (let [dp           (day-pillar n-pillars)
        start-ages   (take 12 (bc/luck-pillar-start-ages date is-male))
        add-empty    (not= 0 (first start-ages))
        
        pillars      (take 12 (bc/luck-pillars date is-male))

        stems        (map first pillars)
        branches     (map second pillars)
        jiazis       (jiazis stems branches)
        jiazis       (map bu/jiazi-id jiazis)
        
        stems        (if add-empty (cons nil stems) stems)
        branches     (if add-empty (cons nil branches) branches)
        jiazis       (if add-empty (cons nil jiazis) jiazis)

        voids        (map (partial bu/is-void? (:jiazi dp)) branches)

        start-dates  (map #(time/plus date (time/years %)) start-ages)
        end-dates    (map #(time/plus % (time/years 10)) start-dates)

        end-dates    (if add-empty (cons (first start-dates) end-dates) end-dates)
        start-ages   (if add-empty (cons 0 start-ages) start-ages)
        start-dates  (if add-empty (cons date start-dates) start-dates)
        
        start-years  (map #(time/year %) start-dates)

        slugs        (map str start-ages (repeat " - ") start-years)
        
        l-pillars    (map hash-map
                          (repeat :id)   start-ages
                          (repeat :stem) stems
                          (repeat :branch) branches
                          (repeat :jiazi) jiazis
                          (repeat :slug) slugs
                          (repeat :palace) (repeat :l)
                          (repeat :start-age) start-ages
                          (repeat :start-date) start-dates
                          (repeat :end-date) end-dates
                          (repeat :pstart) start-dates
                          (repeat :pend) end-dates

                          (repeat :void) voids
                          (repeat :dm) (repeat (:stem dp))
                          )
        
        relations    (map (partial relations2 bu/relations n-pillars) l-pillars)
        relations    (map hash-map (repeat :relations) relations)        
        qi-stages    (map (partial pillars-stems-qi n-pillars) l-pillars)

        shas      (calc-sha dp l-pillars bs/shas)
        shas      (map hash-map (repeat :shas) shas)

        l-pillars    (map merge l-pillars relations qi-stages shas)]
    
    (pillars-to-map l-pillars)
    ))


(defn year-pillars [n-pillars l-pillar]
  (let [l-start     (l-pillar :pstart)
        l-end       (l-pillar :pend)
        dp          (day-pillar n-pillars)
        years       (bc/year-pillars l-start l-end)
        start-dates (map :start years)
        end-dates   (map :end years)
        pstart      (map :pstart years)
        pend        (map :pend years)
        pillars     (map :yp years)
        stems       (map first pillars)
        branches    (map second pillars)
        jiazis      (map bu/jiazi-id pillars)        
        slugs       (map time/year start-dates)
        nl-pillars  (conj n-pillars l-pillar)
        ids         (map #(ftime/unparse (ftime/formatter "yyyy-MM-dd") %) start-dates)
        voids       (map (partial bu/is-void? (:jiazi dp)) branches)

        y-pillars   (map hash-map
                         (repeat :id)  ids
                         (repeat :stem) stems
                         (repeat :branch) branches
                         (repeat :jiazi) jiazis
                         (repeat :slug) slugs
                         (repeat :palace) (repeat :y)
                         (repeat :start-date) start-dates
                         (repeat :end-date) end-dates
                         (repeat :pstart) pstart
                         (repeat :pend) pend

                         (repeat :void) voids
                         (repeat :dm) (repeat (:stem dp))
                         )
        
        relations  (map (partial relations2 bu/relations nl-pillars) y-pillars)
        relations  (map hash-map (repeat :relations) relations)        
        qi-stages  (map (partial pillars-stems-qi nl-pillars) y-pillars)

        shas      (calc-sha dp y-pillars bs/shas)
        shas      (map hash-map (repeat :shas) shas)

        y-pillars  (map merge y-pillars relations qi-stages shas)]
    
    (pillars-to-map y-pillars)
    ))

(defn month-pillars [nl-pillars y-pillar]
  (let [y-start     (y-pillar :pstart)
        y-end       (y-pillar :pend)
        dp          (day-pillar nl-pillars)
        months      (bc/month-pillars y-start y-end)
        start-dates (map :start months)
        end-dates   (map :end months)
        pstart      (map :pstart months)
        pend        (map :pend months)
        pillars     (map :mp months)
        stems       (map first pillars)
        branches    (map second pillars)
        jiazis      (map bu/jiazi-id pillars)        
        slugs       (map time/month start-dates)
        ids         (map #(ftime/unparse (ftime/formatter "yyyy-MM-dd") %) start-dates)
        voids       (map (partial bu/is-void? (:jiazi dp)) branches)
        
        m-pillars   (map hash-map
                         (repeat :id)  ids
                         (repeat :stem) stems
                         (repeat :branch) branches
                         (repeat :jiazi) jiazis
                         (repeat :slug) slugs
                         (repeat :palace) (repeat :m)
                         (repeat :start-date) start-dates
                         (repeat :end-date) end-dates
                         (repeat :pstart) pstart
                         (repeat :pend) pend

                         (repeat :void) voids
                         (repeat :dm) (repeat (:stem dp))
                         )
        
        relations  (map (partial relations2 bu/relations nl-pillars) m-pillars)
        relations  (map hash-map (repeat :relations) relations)        
        qi-stages  (map (partial pillars-stems-qi nl-pillars) m-pillars)

        shas      (calc-sha dp m-pillars bs/shas)
        shas      (map hash-map (repeat :shas) shas)
        
        m-pillars  (map merge m-pillars relations qi-stages shas)]
    
    (pillars-to-map m-pillars)
    ))

(defn day-pillars [nl-pillars m-pillar]
  (let [m-start     (m-pillar :pstart)
        m-end       (m-pillar :pend)
        dp          (day-pillar nl-pillars)
        days        (bc/day-pillars m-start m-end)
        start-dates (map :start days)
        end-dates   (map :end days)
        pstart      (map :pstart days)
        pend        (map :pend days)

        pillars     (map :dp days)
        jiazis      (map bu/jiazi-id pillars)        
        stems       (map first pillars)
        branches    (map second pillars)
        ids         (map #(ftime/unparse (ftime/formatter "yyyy-MM-dd") %) start-dates)
        voids       (map (partial bu/is-void? (:jiazi dp)) branches)

        slugs       (map #(ftime/unparse (ftime/formatter "M-d") %) start-dates)
        
        pillars     (map hash-map
                         (repeat :id) ids
                         (repeat :stem) stems
                         (repeat :branch) branches
                         (repeat :jiazi) jiazis
                         (repeat :slug) slugs
                         (repeat :palace) (repeat :d)
                         (repeat :start-date) start-dates
                         (repeat :end-date) end-dates
                         (repeat :pstart) pstart
                         (repeat :pend) pend

                         (repeat :void) voids
                         (repeat :dm) (repeat (:stem dp))
                         )
        
        relations  (map (partial relations2 bu/relations nl-pillars) pillars)
        relations  (map hash-map (repeat :relations) relations)        
        qi-stages  (map (partial pillars-stems-qi nl-pillars) pillars)

        shas      (calc-sha dp pillars bs/shas)
        shas      (map hash-map (repeat :shas) shas)
        
        pillars  (map merge pillars relations qi-stages shas)]
    
    (pillars-to-map pillars)
    ))


(defn hour-pillars [nl-pillars d-pillar]
  (let [d-start     (d-pillar :pstart)
        d-end       (d-pillar :pend)
        ds          (d-pillar :stem)
        dp          (day-pillar nl-pillars)
        hours       (bc/hour-pillars d-start d-end ds)
        start-dates (map :start hours)
        end-dates   (map :end hours)
        pstart      (map :pstart hours)
        pend        (map :pend hours)
        pillars     (map :hp hours)
        jiazis      (map bu/jiazi-id pillars)        
        stems       (map first pillars)
        branches    (map second pillars)
        slugs       (map #(ftime/unparse (ftime/formatter "HH:mm") %) start-dates)
        ids         (map #(ftime/unparse (ftime/formatter "yyyy-MM-dd HH:mm") %) start-dates)
        voids       (map (partial bu/is-void? (:jiazi dp)) branches)
        
        pillars     (map hash-map
                         (repeat :id) ids
                         (repeat :stem) stems
                         (repeat :branch) branches
                         (repeat :jiazi) jiazis
                         (repeat :slug) slugs
                         (repeat :palace) (repeat :h)
                         (repeat :start-date) start-dates
                         (repeat :end-date) end-dates
                         (repeat :pstart) pstart
                         (repeat :pend) pend

                         (repeat :void) voids
                         (repeat :dm) (repeat (:stem dp))
                         )
        
        relations  (map (partial relations2 bu/relations nl-pillars) pillars)
        relations  (map hash-map (repeat :relations) relations)        
        qi-stages  (map (partial pillars-stems-qi nl-pillars) pillars)

        shas      (calc-sha dp pillars bs/shas)
        shas      (map hash-map (repeat :shas) shas)
        
        pillars  (map merge pillars relations qi-stages shas)]
    
    (pillars-to-map pillars)
    ))


(defn chart [date is-male no-hour]
  (let [natal      (natal-pillars date no-hour)
        luck       (luck-pillars date is-male natal)
        g-scores   (god-scores natal)
        e-scores   (element-scores g-scores)
        sw-scores  (strong-weak-scores e-scores)
        dm         (:stem (day-pillar natal))
        usefull-elem (usefull-elem sw-scores dm)]
    {:natal-pillars natal
     :l luck
     :y nil
     :m nil
     :d nil
     :h nil
     :god-scores    g-scores
     :elem-scores   e-scores
     :strong-weak-scores sw-scores
     :usefull-elem usefull-elem
     :dm dm}))


;;(def dd (time/date-time 1990 9 6 23 10))
;;(def nps (natal-pillars dd true))
;;(:stem (day-pillar nps))

;;(def lps (luck-pillars dd true nps))
;;nps
;;lps
;;(lps 0)
;;((lps 0) :start-date)
;;((lps 0) :end-date)


;;l0
;;(l0 :id)

;;(l0 :start-date)
;;(l0 :end-date)
;;;(bc/year-pillars1 (l0 :start-date) (l0 :end-date))

;;(stem-scores nps)
;;(god-scores nps)
;;(strong-weak-scores (element-scores (god-scores nps)))


