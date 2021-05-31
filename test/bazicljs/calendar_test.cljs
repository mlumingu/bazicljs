(ns bazicljs.calendar-test
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.test :refer-macros [deftest testing is use-fixtures async]]
   [bazicljs.calendar :as cal]
   [cljs.reader :refer [read-string]]
   [cljs-http.client :as http]
   [cljs-time.core :as time]   
   ))


(use-fixtures :once
  {:before
   #(async done
           (go
             (reset! cal/cal
                     (:body (<! (http/get "/calendar.edn"))))
             (done)
             ))})


(deftest gregorian-to-solar-ymd
  (is (= (cal/gregorian-to-solar-ymd (time/date-time 1990 9 6)) '[0 10 0 8 6 6]))
  )

(deftest hour-branch
  (is (= (cal/hour-branch 0) 0))
  (is (= (cal/hour-branch 1) 1))
  (is (= (cal/hour-branch 2) 1))
  (is (= (cal/hour-branch 22) 11))
  (is (= (cal/hour-branch 23) 0))
  )

(deftest hour-stem
  (is (= (cal/hour-stem 0 0) 0))
  (is (= (cal/hour-stem 0 23) 2))
  (is (= (cal/hour-stem 3 2) 7))
  (is (= (cal/hour-stem 0 5) 3))
  
  (is (= (cal/hour-stem 5 0) 0))
  (is (= (cal/hour-stem 5 23) 2))

  (is (= (cal/hour-stem 4 14) 5))
  )

(deftest next-pillar
  (is (= (cal/next-pillar [0 8]) [1 9]))
  (is (= (cal/next-pillar [9 11]) [0 0]))
  (is (= (cal/next-pillar [8 11]) [9 0]))
  (is (= (cal/next-pillar [9 10]) [0 11]))
  
  (is (= (cal/previous-pillar [0 0]) [9 11]))
  (is (= (cal/previous-pillar [9 0]) [8 11]))
  (is (= (cal/previous-pillar [0 11]) [9 10]))    
  )

(deftest days-to-next-prev-month
  (let [date1 (time/date-time 1990 9 6 23 10)
        date2 (time/date-time 1988 10 19 2 30)]
    
    (is (= (cal/days-to-next-prev-month date1 -1) 30))
    (is (= (cal/days-to-next-prev-month date1 1) 2))
    (is (= (cal/days-to-next-prev-month date2 -1) 12))    
    ))

(deftest luck-pillars
  (let [date1 (time/date-time 1990 9 6 23 10)]
    
    (is (= (take 1 (cal/luck-pillars date1 true)) [[1 9]]))
    (is (= (take 1 (cal/luck-pillars date1 false)) [[9 7]]))    
    ))

(deftest luck-pillar-start-ages
  (let [date1 (time/date-time 1990 9 6 23 10)
        date2 (time/date-time 1988 10 19 2 30)]
    (is (= (take 1 (cal/luck-pillar-start-ages date1 true)) [0]))
    (is (= (take 1 (cal/luck-pillar-start-ages date2 false)) [4]))))

(deftest luck-pillars1
  (let [date1 (time/date-time 1990 9 6 23 10)]
    (is (= (take 2 (cal/luck-pillars1 date1 true)) [[[1 9] 0] [[2 10] 10]])))
  )

(deftest gregorian-to-solar
  (let [date1 (time/date-time 1990 9 6 23 10)
        out1  [2 0 0 10 0 8 6 6]

        date2 (time/date-time 1988 10 19 2 30)
        out2  [7 1 3 7 8 10 4 4]

        date3 (time/date-time 1962 4 26 5 30)
        out3 [3 3 0 6 0 4 8 2]]

    (is (= (cal/gregorian-to-solar date1) out1))
    (is (= (cal/gregorian-to-solar date2) out2))
    (is (= (cal/gregorian-to-solar date3) out3))))
