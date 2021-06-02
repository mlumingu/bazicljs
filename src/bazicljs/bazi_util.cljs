(ns bazicljs.bazi-util)

(def pillar-types ["H" "D" "M" "Y" "l" "y" "m" "d" "h"])
(def palace-keys [:H :D :M :Y :l :y :m :d :h])
(def nl-palace-keys [:H :D :M :Y :l])

(def STEM-NAMES ["Jia"  "Yi"  "Bing"  "Ding"  "Wu"  "Ji"  "Geng"  "Xin"  "Ren"  "Gui"])

(def STEM-HTML ["\u7532"  "\u4e59"  "\u4e19"  "\u4e01"  "\u620a"  "\u5df1"  "\u5e9a"  "\u8f9b"  "\u58ec"  "\u7678"])

(def BRANCH-NAMES ["Zi"  "Chou"  "Yin"  "Mao"  "Chen"  "Si"  "Wu"  "Wei"  "Shen"  "You"  "Xu"  "Hai"])

(def BRANCH-HTML ["\u5b50"  "\u4e11" 
                  "\u5bc5" "\u536f"  "\u8fb0" 
                  "\u5df3"  "\u5348"  "\u672a" 
                  "\u7533"  "\u9149"  "\u620c" 
                  "\u4ea5"])

(def POLARITY-NAMES ["Yin"  "Yang"])

(def ELEMENT-NAMES ["Wood"  "Fire"  "Earth"  "Metal"  "Water"])

(def GOD-NAMES ["FR"  "RW"  "EG"  "HO"  "IW"  "DW"  "7K"  "DO"  "IR"  "DR"])

(def FACTOR-NAMES ["Parallel"  "Output"  "Wealth"  "Influence"  "Resource"])

(def HIDDEN-STEMS [[9] [5 7 9] 
                   [0 4 2] [1] [4 9 1] 
                   [2 4 6] [3 5] [5 3 1] 
                   [6 4 8] [7] [4 3 7] 
                   [8 0]])

(def STEM-FROM-BRANCH [8 5  0 1 4  3 2 5  6 7 4  9])


(defn rel [rtype ids idtype pairs? & {:keys [element] :or {element nil}}]
  {:rtype rtype :ids ids :idtype idtype :pairs? pairs? :element element})

(def neg-relations [(rel :clash #{0 6} :branch true)
                    (rel :clash #{1 7} :branch true)
                    (rel :clash #{2 8} :branch true)
                    (rel :clash #{3 9} :branch true)
                    (rel :clash #{4 10} :branch true)
                    (rel :clash #{5 11} :branch true)

                    (rel :harm #{2 5}  :branch true)
                    (rel :harm #{8 11} :branch true)
                    (rel :harm #{3 4}  :branch true)
                    (rel :harm #{9 10} :branch true)
                    (rel :harm #{6 1}  :branch true)
                    (rel :harm #{0 7}  :branch true)                

                    (rel :destruction #{2 5}  :branch true)
                    (rel :destruction #{4 11}  :branch true)
                    (rel :destruction #{6 1}  :branch true)
                    (rel :destruction #{8 3}  :branch true)
                    (rel :destruction #{10 7}  :branch true)
                    (rel :destruction #{0 9}  :branch true)

                    (rel :ug-pun [2 8] :branch true)
                    (rel :ug-pun [2 5] :branch true)
                    (rel :ug-pun [8 5] :branch true)

                    (rel :bu-pun [7 1] :branch true)
                    (rel :bu-pun [1 10] :branch true)
                    (rel :bu-pun [10 7] :branch true)
                    
                    (rel :uc-pun [0 3] :branch true)

                    (rel :s-pun [4 4] :branch true)
                    (rel :s-pun [6 6] :branch true)
                    (rel :s-pun [9 9] :branch true)
                    (rel :s-pun [11 11] :branch true)
                    
                    ])

(def pos-harmonies
  [(rel :harmony #{2 6 10} :branch false :element 1)
   (rel :harmony #{8 0 4} :branch false :element 4)
   (rel :harmony #{11 3 7} :branch false :element 0)
   (rel :harmony #{5 9 1} :branch false :element 3)

   (rel :d-combo #{2 3 4} :branch false :element 0)
   (rel :d-combo #{5 6 7} :branch false :element 1)
   (rel :d-combo #{8 9 10} :branch false :element 3)
   (rel :d-combo #{11 0 1} :branch false :element 4)])

(def pos-relations
  [(rel :s-combo #{0 5} :stem true :element 2)
   (rel :s-combo #{1 6} :stem true :element 3)
   (rel :s-combo #{2 7} :stem true :element 4)
   (rel :s-combo #{3 8} :stem true :element 0)
   (rel :s-combo #{4 9} :stem true :element 1)

   (rel :b-combo #{0 1} :branch true :element 2)
   (rel :b-combo #{2 11} :branch true :element 0)
   (rel :b-combo #{3 10} :branch true :element 1)
   (rel :b-combo #{4 9} :branch true :element 3)
   (rel :b-combo #{5 8} :branch true :element 4)
   (rel :b-combo #{7 6} :branch true :element 1)])

(def pos-natal-harmonies (concat pos-harmonies
                                 [(rel :ls-cross #{0 6 9 3} :branch false)
                                  (rel :al-cross #{1 7 10 4} :branch false)
                                  (rel :t-cross #{2 5 8 11} :branch false)]))


(defn hidden-stems [branch]
  (STEM-FROM-BRANCH branch))

(defn stem-polarity [stem]
  (rem stem 2))

(defn branch-polarity [branch]
  (stem-polarity (STEM-FROM-BRANCH branch)))

(defn stem-element [stem]
  (quot stem 2))

(defn branch-element [branch]
  (let [stem (STEM-FROM-BRANCH branch)]
    (quot stem 2)))


(defn stem-god [daymaster stem]
  (let [s-elem    (stem-element stem)
        s-pol     (stem-polarity stem)
        dm-elem   (stem-element daymaster)
        dm-pol    (stem-polarity daymaster)
        factor    (rem (+ s-elem (- 5 dm-elem)) 5)
        delta-pol (rem (+ s-pol (- 2 dm-pol)) 2)]
    (+ (* 2 factor) delta-pol)))

(defn stem-gods [dm]
  (map #(stem-god dm %) (range 10)))


(defn god-stems [dm]
  (map second (sort-by first (map vector (stem-gods dm) (range 10)))))


(defn branch-god [daymaster branch]
  (let [stem (STEM-FROM-BRANCH branch)]
    (stem-god daymaster stem)))


(defn stem-qi [branch stem]
  (let [stem-phases [1 6 10 9 10 9 7 0 4 3]
        s-phase     (stem-phases stem)
        s-pol       (stem-polarity stem)
        op          (if (= s-pol 0) + -)]
    (+ 1 (rem (+ (op s-phase branch) 12) 12))))


(defn stem-element-qi [branch stem]
  (let [stem-phases [2 2 11 11 11 11 8 8 5 5]
        s-phase     (- (stem-phases stem) 1)]
    (+ 1 (rem (+ s-phase branch) 12))))


(defn score-multiplier [bid sid]
  (let [earth-multiplier [0.5 1  0.5 0.5 1  1 1 1  0.5 0.5 1  0.5]
        multiplier       [nil  0.8 0.5 0.5 1 1 0.8 0.5 0.5 0.8 0.5 0.5 0.5]
        s-elem           (stem-element sid)]
    ;;2 == earth
    (if (= s-elem 2)
      (earth-multiplier bid)
      (multiplier (stem-element-qi bid sid))
      ;;(multiplier (stem-qi bid sid))
      )
    ))


(defn score-multipliers [bid]
  (map #(score-multiplier bid %) (range 10)))


(def dm-score-correction-multipliers [1 1 0.7 0.7 0.7 0.7 0.7 0.7 1 1])


(def jiazi-qi-stages
  {

   ;;jia
   [0 0]   [2   3   4   5   6   7   8   9   10   11   12   1]
   [0 10]  [2   3   4   5   6   7   8   9   10   11   12   1]
   [0 8]   [2   3   4   5   6   7   8   9   10   11   12   1]
   [0 6]   [2   3   4   5   6   7   8   9   10   11   12   1]
   [0 4]   [2   3   4   5   6   7   8   9   10   11   12   1]
   [0 2]   [1   3   4   5   6   7   8   9   10   11   12   5]

   ;;yi
   [1 1]   [7   6   5   4   3   2   1   12   11   10   9   8]
   [1 11]  [7   6   5   4   3   2   1   12   11   10   9   8]
   [1 9]   [7   6   5   4   3   2   1   12   11   10   9   8]
   [1 7]   [7   6   5   4   3   2   1   12   11   10   9   8]
   [1 5]   [7   6   5   4   3   2   1   12   11   10   9   1]
   [1 3]   [5   6   5   4   3   2   1   12   11   10   9   1]

   ;;bing
   [2 2]  [11   12   1   2   3   4   5   6   7   8   9   10]
   [2 0]  [11   12   1   2   3   4   5   6   7   8   9   10]
   [2 10] [11   12   1   2   3   4   5   6   7   8   9   10]
   [2 8]  [11   12   1   2   3   4   5   6   7   8   9   10]
   [2 6]  [11   12   1   2   3   4   5   6   7   8   9   8]
   [2 4]  [4    12   1   2   3   4   5   6   7   8   9   8]

   ;;ding
   [3 3]  [10   9   8   7   6   5   4   3   2   1   12   11]
   [3 1]  [10   9   8   7   6   5   4   3   2   1   12   11]
   [3 11] [10   9   8   7   6   5   4   3   2   1   12   11]
   [3 9]  [10   9   8   7   6   5   4   3   2   1   12   11]
   [3 7]  [2    9   8   7   6   5   4   3   2   1   12   10]
   [3 5]  [2    9   8   7   6   5   4   3   2   1   12   10]

   ;;wu
   [4 4]   [11   12   1   2   3   4   5   6   7   8   9   10]
   [4 2]   [11   12   1   2   3   4   5   6   7   8   9   10]
   [4 0]   [11   12   1   2   3   4   5   6   7   8   9   10]
   [4 10]  [11   12   1   2   3   4   5   6   7   8   9   10]
   [4 8]   [7    12   1   2   3   4   5   6   7   8   9   11]
   [4 6]   [7    12   1   2   3   4   5   6   7   8   9   11]

   ;;ji
   [5 5]   [10   9   8   7   6   5   4   3   2   1   12   11]
   [5 3]   [10   9   8   7   6   5   4   3   2   1   12   11]
   [5 1]   [10   9   8   7   6   5   4   3   2   1   12   11]
   [5 11]  [10   9   8   7   6   5   4   3   2   1   12   11]
   [5 9]   [11   9   8   7   6   5   4   3   2   1   12   10]
   [5 7]   [11   9   8   7   6   5   4   3   2   1   12   10]

   ;;geng
   [6 6]   [8   9   10   11   12   1   2   3   4   5   6   7]
   [6 4]   [8   9   10   11   12   1   2   3   4   5   6   7]
   [6 2]   [8   9   10   11   12   1   2   3   4   5   6   7]
   [6 0]   [8   9   10   11   12   1   2   3   4   5   6   7]
   [6 10]  [10  9   10   11   12   1   2   3   4   5   6   11]
   [6 8]   [10  9   10   11   12   1   2   3   4   5   6   11]

   ;;xin
   [7 7]  [1   12   11   10   9   8   7   6   5   4   3   2]
   [7 5]  [1   12   11   10   9   8   7   6   5   4   3   2]
   [7 3]  [1   12   11   10   9   8   7   6   5   4   3   2]
   [7 1]  [1   12   11   10   9   8   7   6   5   4   3   2]
   [7 11] [11  12   11   10   9   8   7   6   5   4   3   7]
   [7 9]  [11  12   11   10   9   8   7   6   5   4   3   7]

   ;;ren
   [8 8]   [5   6   7   8   9   10   11   12   1   2   3   4]
   [8 6]   [5   6   7   8   9   10   11   12   1   2   3   4]
   [8 4]   [5   6   7   8   9   10   11   12   1   2   3   4]
   [8 2]   [5   6   7   8   9   10   11   12   1   2   3   4]
   [8 0]   [10  6   7   8   9   10   11   12   1   2   3   2]
   [8 10]  [10  6   7   8   9   10   11   12   1   2   3   2]

   ;;gui
   [9 9]  [4   3   2   1   12   11   10   9   8   7   6   5]
   [9 7]  [4   3   2   1   12   11   10   9   8   7   6   5]
   [9 5]  [4   3   2   1   12   11   10   9   8   7   6   5]
   [9 3]  [4   3   2   1   12   11   10   9   8   7   6   5]
   [9 1]  [8   3   2   1   12   11   10   9   8   7   6   4]
   [9 11] [8   3   2   1   12   11   10   9   8   7   6   4]

   }
  )


(defn jiazi-qi [branch pillar]
  ((jiazi-qi-stages pillar) branch)
  )


(defn jiazi-id [[sid bid]]
  (+ sid (* 10 (/ (mod (- sid bid) 12) 2))))


(defn void-branches [jiazi-id]
  (let [voids [#{10 11} #{8 9} #{6 7} #{4 5} #{3 2} #{0 1}]
        group (quot jiazi-id 10)]
    (voids group)))


(defn is-void? [dm bid]
  (contains? (void-branches dm) bid))


(def nayin
  [
   {:element 3 :description "metal into the sea"}
   {:element 1 :description "fire in the cauldron"}
   {:element 0 :description "wood of the forest"}
   {:element 2 :description "earth around the road"}
   {:element 3 :description "metal of the sword"}
   {:element 1 :description "fire on the mountain"}
   {:element 4 :description "water in the stream"}
   {:element 2 :description "earth on the fortress"}
   {:element 3 :description "metal melted to white"}
   {:element 0 :description "willow tree"}
   
   {:element 4 :description "spring water"}
   {:element 2 :description "earth on the house"}
   {:element 1 :description "lightning fire"}
   {:element 0 :description "pinewood"}
   {:element 4 :description "water of the origin"}
   {:element 3 :description "metal in the sand"}
   {:element 1 :description "quiet fire"}
   {:element 0 :description "wood in the valey"}
   {:element 2 :description "earth on the wall"}
   {:element 3 :description "gold leaves"}
   
   {:element 1 :description "fire of the lamp"}
   {:element 4 :description "water of heaven"}
   {:element 2 :description "earth on the road"}
   {:element 3 :description "gold of the jewelry"}
   {:element 0 :description "blackberry tree"}
   {:element 4 :description "water of the river"}
   {:element 2 :description "earth in the sand"}
   {:element 1 :description "fire in the sky"}
   {:element 0 :description "gojav tree"}
   {:element 4 :description "water of the ocean"}
   ])

