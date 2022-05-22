(ns bazicljs.bazi
  (:require
   [bazicljs.calendar :as bc]
   [bazicljs.bazi-util :as bu]
   [bazicljs.sha :as bs]
   [cljs-time.core :as time]
   [cljs-time.format :as ftime]
   [clojure.set :as cs]
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


(defn full-relation [pillar pillars relation]
  (let [idtype (:idtype relation)
        id     (idtype pillar)
        rids   (:ids relation)
        id-in-r (some #{id} rids)]
    (if id-in-r
      (let [other-rids (into #{} (remove-one id rids))
            matches (filter #(some #{(idtype %)} other-rids)  pillars)
            groups (group-by idtype matches)
            found-rids (into #{} (keys groups))
            full-relation? (= found-rids other-rids)]
        (if full-relation?
          (relation-instances relation matches))
        )
      ()
      )))



(defn relation [pillar pillars relation]
  (let [idtype (:idtype relation)
        id     (idtype pillar)
        rids   (:ids relation)
        id-in-r (some #{id} rids)]
    (if id-in-r
      (let [other-rids (into #{} (remove-one id rids))
            matches (filter #(some #{(idtype %)} other-rids)  pillars)
            groups (group-by idtype matches)
            found-rids (into #{} (keys groups))
            full-relation? (= found-rids other-rids)]
        (if (not-empty found-rids)
          (relation-instances relation matches))
        )
      ()
      )))



(defn relation-w-nlps [pillar pillars relation]
  ;; relation with natal or luck pillars in it
  (let [idtype (:idtype relation)
        id     (idtype pillar)
        rids   (:ids relation)
        id-in-r (some #{id} rids)
        ]
    (if id-in-r
      (let [other-rids (into #{} (remove-one id rids))
            matches (filter #(some #{(idtype %)} rids)  pillars)
            groups (group-by idtype matches)
            found-rids (into #{} (keys groups))

            full-relation (cs/subset? other-rids found-rids)
            
            all-pillars (cons pillar matches)
            nl-pillars (filter #(some #{(:palace %)} bu/nl-palace-keys) all-pillars)
            has-nl-ps (seq nl-pillars)]
        (if (and  (not-empty found-rids) has-nl-ps)
          (relation-instances relation matches))
        )
      ()
      )))


(defn separate-all [coll]
  (letfn [(separate-at [coll pos]
            (let [[a b] (split-at pos coll)]
              [(first b) (concat a (rest b))]))]
    (map (partial separate-at coll) (range (count coll)))))



(defn full-relations [relations pillars]
  (for [[p ps] (separate-all pillars)]
    (mapcat (partial full-relation p ps) relations)))

(defn relations [relations pillars]
  (for [[p ps] (separate-all pillars)]
    (mapcat (partial relation p ps) relations)))

(defn relations-w-nlps [rels pillars pillar]
  (mapcat (partial relation pillar pillars) rels))





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

(defn pillars-qi [pillars]
  (for [p pillars] (into [] (rest (stems-qi false p p)))))

(defn br-harmony [br]
  (filter #(contains? (:ids %) br) bu/three-harmonies))

(defn branch-harmonies [brs]
  (->> brs
       (map br-harmony)
       (map first)
       (map :element)))


(defn natal-pillars [date no-hour date-info]
  (let [day-info   (:day date-info)
        d-weekday  (:weekday day-info)
        d-off-12   (:off-12 day-info)
        d-const-28 (:const-28 day-info)

        solar     (if no-hour (bc/gregorian-to-solar-ymd date) (bc/gregorian-to-solar date))
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
        harmonies (branch-harmonies branches)
        
        n-pillars (map hash-map
                       (repeat :id)     palaces
                       (repeat :stem)   stems
                       (repeat :branch) branches
                       (repeat :jiazi)  jiazis
                       (repeat :slug)   slugs
                       (repeat :palace) palaces
                       (repeat :void)   voids
                       (repeat :dm) (repeat dm)
                       (repeat :harmony) harmonies
                       )

        dp        (if no-hour (first n-pillars) (second n-pillars))
        dp        (merge dp {:weekday  d-weekday
                             :off-12   d-off-12
                             :const-28 d-const-28})

        n-pillars (if no-hour
                    (cons dp (rest n-pillars))
                    (concat [(first n-pillars) dp] (drop 2 n-pillars)))

        plrs-qi     (pillars-qi n-pillars)
        shas        (calc-sha dp n-pillars bs/shas)
        n-rels      (relations bu/neg-relations n-pillars)
        p-pair-rels (relations bu/pos-relations n-pillars)
        p-hars      (relations bu/pos-natal-harmonies n-pillars)
        cross-hars  (full-relations bu/cross-harmonies n-pillars)        
        p-rels      (map concat p-pair-rels p-hars cross-hars)
        qi-stages   (map (partial pillars-stems-qi n-pillars) n-pillars)
        all         (map hash-map
                         (repeat :pillar-qi) plrs-qi
                         (repeat :n-relations) n-rels
                         (repeat :p-relations) p-rels
                         (repeat :shas) shas
                         (repeat :cshas) (repeat (list)))]
    
    (mapv merge n-pillars all qi-stages)))


(defn day-pillar [n-pillars]
  (first (filter #(= (% :palace) :D) n-pillars)))
(defn month-pillar [n-pillars]
  (first (filter #(= (% :palace) :M) n-pillars)))
(defn hour-pillar [n-pillars]
  (first (filter #(= (% :palace) :H) n-pillars)))
(defn year-pillar [n-pillars]
  (first (filter #(= (% :palace) :Y) n-pillars)))


(defn calc-sha-rel-qi [dp pillars t-pillars st-pillars]
  (let [all-pillars (concat pillars st-pillars)
        n-rels      (map (partial relations-w-nlps bu/neg-relations pillars) t-pillars)
        p-pair-rels (map (partial relations-w-nlps bu/pos-relations pillars) t-pillars)
        p-hars      (map (partial relations-w-nlps bu/pos-harmonies all-pillars) t-pillars)
        p-rels      (map concat p-pair-rels p-hars)
        shas        (calc-sha dp t-pillars bs/shas)
        cshas       (calc-sha dp t-pillars bs/cshas)
        qi-stages   (map (partial pillars-stems-qi pillars) t-pillars)
        t-plrs-qi   (pillars-qi t-pillars)
        all         (map hash-map
                         (repeat :pillar-qi)  t-plrs-qi
                         (repeat :n-relations) n-rels
                         (repeat :p-relations) p-rels
                         (repeat :shas) shas
                         (repeat :cshas) cshas)]
    (map merge all qi-stages)))


(defn ca-palace [{ds :stem db :branch}]
  (let [s-combo (first (filter #(contains? % ds) (map :ids bu/s-combos)))
        b-combo (first (filter #(contains? % db) (map :ids bu/b-combos)))
        s (first (clojure.set/difference s-combo #{ds}))
        b (first (clojure.set/difference b-combo #{db}))
        ]
    [s b]))


(defn c-palace [{ms :stem mb :branch}]
  (let [mp [ms mb]
        ps (iterate bc/next-pillar mp)]
    (nth ps 51)))


(defn l-palace [{ys :stem} {mb :branch} {hb :branch} {born-after-zhong :born-after-zhong}]
  (let [lb (mod (+ (- 5 mb hb) 24) 12)
        ls0 (mod ys 5)
        lb0 (if (> lb 1) (- lb 2) lb)
        ls (mod (+ (* 2 ls0) 2 lb0) 10)
        
        ]
    [ls lb]))


(defn b-palace [{ys :stem} {mb :branch} {hb :branch} {born-after-zhong :born-after-zhong}]
  (let [lb (mod (+ 1 mb hb) 12)
        ls0 (mod ys 5)
        lb0 (if (> lb 1) (- lb 2) lb)
        ls (mod (+ (* 2 ls0) 2 lb0) 10)
        
        ]
    [ls lb]))


(defn life-pillars [n-pillars date-info]
  (let [dp (day-pillar n-pillars)
        mp (month-pillar n-pillars)
        no-hour (:no-hour date-info)
        hp (if no-hour nil (hour-pillar n-pillars))
        yp (year-pillar n-pillars)

        [cas cab] (ca-palace dp)
        [cs cb]   (c-palace mp)
        [ls lb]   (if no-hour [nil nil] (l-palace yp mp hp date-info))
        [bs bb]   (if no-hour [nil nil] (b-palace yp mp hp date-info))

        stems     (if no-hour [cs cas] [cs ls bs cas])
        branches  (if no-hour [cb cab] [cb lb bb cab])
        slugs     (if no-hour ["conception" "conc aura"] ["conception" "life" "body" "con aura"])
        palaces   (if no-hour [:c :ca][:c :li :b :ca])
        
        jiazis    (map bu/jiazi-id (jiazis stems branches))
        voids     (map (partial bu/is-void? (:jiazi dp)) branches)
        harmonies (branch-harmonies branches)        

        l-pillars (map hash-map
                       (repeat :id)      palaces
                       (repeat :stem)    stems
                       (repeat :branch)  branches
                       (repeat :jiazi)   jiazis
                       (repeat :slug)    slugs
                       (repeat :palace)  palaces
                       (repeat :void)    voids
                       (repeat :dm)      (repeat (:stem dp))
                       (repeat :harmony) harmonies                       
                       )

        sha-rels-qi  (calc-sha-rel-qi dp n-pillars l-pillars [])
        l-pillars    (map merge l-pillars sha-rels-qi)]
    l-pillars))


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
        harmonies (branch-harmonies branches)        
        
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
                          (repeat :n-pillars) (repeat n-pillars)
                          (repeat :t-pillars) (repeat [])
                          (repeat :harmony) harmonies                                               
                          )

        sha-rels-qi  (calc-sha-rel-qi dp n-pillars l-pillars [])
        
        l-pillars    (map merge l-pillars sha-rels-qi)
        ]
    
    (pillars-to-map l-pillars)))


(defn year-pillars [l-pillar]
  (let [n-pillars   (l-pillar :n-pillars)
        nl-pillars  (conj n-pillars l-pillar)
        l-start     (l-pillar :pstart)
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
        ids         (map #(ftime/unparse (ftime/formatter "yyyy-MM-dd") %) start-dates)
        voids       (map (partial bu/is-void? (:jiazi dp)) branches)
        harmonies (branch-harmonies branches)        

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
                         (repeat :nl-pillars) (repeat nl-pillars)
                         (repeat :t-pillars) (repeat [])
                         (repeat :harmony) harmonies                                                                        
                         )

        sha-rels-qi  (calc-sha-rel-qi dp nl-pillars y-pillars [])
        y-pillars    (map merge y-pillars sha-rels-qi)
        ]
    (pillars-to-map y-pillars)
    ))


(defn month-pillars [y-pillar]
  (let [nl-pillars  (y-pillar :nl-pillars)
        st-pillars  [y-pillar]
        y-start     (y-pillar :pstart)
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
        harmonies (branch-harmonies branches)        
        
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
                         (repeat :nl-pillars) (repeat nl-pillars)
                         (repeat :st-pillars) (repeat st-pillars)
                         (repeat :harmony) harmonies                                                                        
                         )

        sha-rels-qi  (calc-sha-rel-qi dp nl-pillars m-pillars st-pillars)
        m-pillars    (map merge m-pillars sha-rels-qi)
        ]
    (pillars-to-map m-pillars)))


(defn day-pillars [m-pillar]
  (let [nl-pillars  (m-pillar :nl-pillars)
        st-pillars  (conj (m-pillar :st-pillars) m-pillar)
        m-start     (m-pillar :pstart)
        m-end       (m-pillar :pend)
        dp          (day-pillar nl-pillars)
        days        (bc/day-pillars m-start m-end)
        start-dates (map :start days)
        end-dates   (map :end days)
        pstart      (map :pstart days)
        pend        (map :pend days)
        officers    (map :off-12 days)
        consts      (map :const-28 days)
        weekdays    (map :weekday days)

        pillars     (map :dp days)
        jiazis      (map bu/jiazi-id pillars)        
        stems       (map first pillars)
        branches    (map second pillars)
        ids         (map #(ftime/unparse (ftime/formatter "yyyy-MM-dd") %) start-dates)
        voids       (map (partial bu/is-void? (:jiazi dp)) branches)

        slugs       (map #(ftime/unparse (ftime/formatter "M-d") %) start-dates)
        harmonies (branch-harmonies branches)        
        
        d-pillars     (map hash-map
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
                           (repeat :off-12) officers
                           (repeat :const-28) consts
                           (repeat :weekday) weekdays

                           (repeat :void) voids
                           (repeat :dm) (repeat (:stem dp))
                           (repeat :nl-pillars) (repeat nl-pillars)
                           (repeat :st-pillars) (repeat st-pillars)
                           (repeat :harmony) harmonies
                           )

        sha-rels-qi  (calc-sha-rel-qi dp nl-pillars d-pillars st-pillars)
        d-pillars    (map merge d-pillars sha-rels-qi)
        ]
    
    (pillars-to-map d-pillars)))


(defn hour-pillars [d-pillar]
  (let [nl-pillars  (d-pillar :nl-pillars)
        st-pillars  (conj (d-pillar :st-pillars) d-pillar)

        d-start     (d-pillar :pstart)
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
        harmonies   (branch-harmonies branches)
        
        h-pillars   (map hash-map
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
                         (repeat :nl-pillars) (repeat nl-pillars)
                         (repeat :st-pillars) (repeat st-pillars)
                         (repeat :harmony) harmonies                         
                         )

        sha-rels-qi  (calc-sha-rel-qi dp nl-pillars h-pillars st-pillars)
        h-pillars    (map merge h-pillars sha-rels-qi)
        ]
    (pillars-to-map h-pillars)
    ))


(defn chart [date is-male no-hour]
  (let [date-info  (bc/date-info date no-hour)
        natal      (natal-pillars date no-hour date-info)
        life       (life-pillars natal date-info)
        luck       (luck-pillars date is-male natal)
        g-scores   (god-scores natal)
        e-scores   (element-scores g-scores)
        sw-scores  (strong-weak-scores e-scores)
        dm         (:stem (day-pillar natal))
        usefull-elem (usefull-elem sw-scores dm)]
    {:natal-pillars natal
     :life-pillars  life
     :l luck
     :y nil
     :m nil
     :d nil
     :h nil
     :god-scores    g-scores
     :elem-scores   e-scores
     :strong-weak-scores sw-scores
     :usefull-elem usefull-elem
     :dm dm
     :date-info date-info}))


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
;;nps
;;(def dp (day-pillar nps))
;;dp



