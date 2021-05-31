(ns bazicljs.sha)

;;stem
(def clash-s-0
  ["Worry"               :Hai :Zi :Yin :Mao :Yin :Mao :Si :Wu :Shen :You
   "Backstabbing"        :Chen :Wu :Si :Si :Chen :Wu :Chen :Zi :You :You
   "Backstabbing"        :Xu :Yin :Mao :Mao :Xu :Yin :Xu :Shen :Hai :Hai])

;;branch
(def clash-b-0
  ["Emotional Branch"    :Shen :Si :Yin :Hai :Shen :Si :Yin :Hai :Shen :Si :Yin :Hai])

;;branch
(def sha-b1-0
  ["Gold Safe"           :Zi     :You     :Wu     :Mao     :Zi     :You     :Wu      :Mao      :Zi      :You     :Zi     :Mao 
   "Red Phoenix"         :Mao    :Yin    :Chou     :Zi     :Hai    :Xu      :You     :Shen    :Wei      :Wu      :Si     :Chen 
   "Heavenly Joy"        :You    :Shen    :Wei    :Wu      :Si     :Chen    :Mao     :Yin     :Chou      :Zi     :Hai    :Xu 
   "Dragon Virtue"       :Wei    :Shen    :You    :Xu      :Hai     :Zi     :Chou    :Yin     :Mao      :Chen    :Si     :Wu 
   "Fortune Virtue"      :You     :Xu     :Hai     :Zi     :Chou   :Yin     :Mao     :Chen     :Si      :Wu      :Wei    :Shen 
   "Funeral Gate"        :Yin    :Mao    :Chen     :Si     :Wu     :Wei     :Shen    :You      :Xu      :Hai     :Zi     :Chou 
   "Hook Edge"           :Mao    :Chen    :Si     :Wu      :Wei    :Shen    :You     :Xu       :Hai     :Zi     :Chou   :Yin 
   "Five Ghost"          :Chen    :Si     :Wu     :Wei     :Shen   :You      :Xu     :Hai      :Zi      :Chou    :Yin    :Mao 
   "Shatter"             :Wu     :Wei     :Shen   :You     :Xu     :Hai      :Zi     :Chou     :Yin     :Mao     :Chen    :Si 
   "Major Squander"      :Wu     :Wei     :Shen   :You     :Xu      :Hai     :Zi     :Chou     :Yin     :Mao     :Chen    :Si 
   "White Tiger"         :Shen    :You    :Xu      :Hai    :Zi     :Chou     :Yin    :Mao      :Chen     :Si     :Wu     :Wei 
   "Heaven Dog"          :Xu      :Hai    :Zi     :Chou    :Yin    :Mao     :Chen     :Si      :Wu      :Wei     :Shen   :You 
   "Peach Blossom"       :You     :Wu     :Mao     :Zi     :You     :Wu     :Mao      :Zi      :You      :Wu     :Mao     :Zi 
   "Blood Edge"          :Xu      :You    :Shen   :Wei     :Wu       :Si    :Chen    :Mao      :Yin     :Chou     :Zi     :Hai 
   "Deja Vu"             :Zi    :Chou    :Yin    :Mao     :Chen     :Si     :Wu     :Wei      :Shen     :You     :Xu     :Hai 
   "Robbery Tragic"      :Si     :Yin    :Hai    :Shen     :Si     :Yin     :Hai    :Shen      :Si      :Yin    :Hai    :Shen 
   "Disaster Tragic"     :Wu      :Mao     :Zi     :You    :Wu      :Mao     :Zi     :You      :Wu      :Mao      :Zi     :You 
   "6 Adversity"         :Mao      :Zi    :You     :Wu     :Mao      :Zi     :You     :Wu      :Mao      :Zi     :You     :Wu])

;;branch, palace important
(def sha-b2-0
  ["General Star"        :Zi    :You   :Wu     :Mao    :Zi    :You    :Wu    :Mao     :Zi     :You    :Wu    :Mao 
   "Travelling Horse"    :Yin   :Hai   :Shen   :Si     :Yin   :Hai   :Shen    :Si     :Yin    :Hai   :Shen    :Si
   "Imperial Canopy"     :Chen  :Chou   :Xu     :Wei   :Chen   :Chou   :Xu    :Wei    :Chen   :Chou    :Xu    :Wei 
   "Separate Edge"       :Yin  :Mao    :Chen   :Si     :Wu    :Wei   :Shen   :You     :Xu     :Hai    :Zi    :Chou 
   "Deceased God"        :Hai  :Shen    :Si    :Yin    :Hai   :Shen   :Si     :Yin    :Hai    :Shen   :Si    :Yin 
   "Solitary Star"       :Yin   :Yin    :Si    :Si     :Si    :Shen  :Shen   :Shen    :Hai    :Hai    :Hai   :Yin 
   "Widow Lodge"         :Xu    :Xu    :Chou  :Chou   :Chou   :Chen  :Chen   :Chen   :Wei     :Wei    :Wei   :Xu 
   "Day Breaker"         :Wu    :Wei   :Shen   :You    :Xu    :Hai    :Zi    :Chou    :Yin    :Mao   :Chen    :Si])

;; stem
(def sha-s-0
  ["Heavenly Noble"      :Chou    :Zi    :Hai    :Hai   :Chou    :Zi    :Chou    :Wu      :Mao     :Mao
   "Heavenly Noble"      :Wei   :Shen    :You   :You    :Wei     :Shen   :Wei    :Yin     :Si      :Si
   "Literary"            :Si     :Wu    :Shen   :You    :Shen    :You    :Hai    :Zi      :Yin     :Mao
   "Learning Hall"       :Hai    :Wu     :Yin   :You     :Yin    :You     :Si     :Zi     :Shen    :Mao
   "Golden Carriage"     :Chen    :Si    :Wei   :Shen    :Wei    :Shen   :Xu     :Hai     :Chou    :Yin
   "Fortune Star"        :Yin    :Mao    :Si     :Wu     :Si     :Wu    :Shen    :You     :Hai     :Zi
   "Bath"                :Zi     :Si    :Mao   :Shen    :Mao    :Shen   :Wu     :Hai     :You     :Yin
   "Red Envy"            :Wu     :Shen   :Yin   :Wei    :Chen    :Chen    :Xu    :You      :Zi     :Shen
   "Goat Blade"          :Mao    :Chen   :Wu    :Wei     :Wu     :Wei    :You    :Xu       :Zi     :Chou
   "Flying Dagger"       :You     :Xu    :Zi    :Chou    :Zi     :Chou   :Mao    :Chen    :Wu      :Wei
   "Tomb Storage"        :Wei     :Xu    :Xu    :Chou    :Xu     :Chou  :Chou    :Chen    :Chen    :Wei
   "Red Clouds"          :You     :Xu    :Wei   :Shen    :Si     :Wu     :Chen   :Mao     :Yin     :Hai])


(def br-keys [:Zi :Chou :Yin :Mao :Chen :Si :Wu :Wei :Shen :You :Xu :Hai])
(def br-map (into {} (map vector br-keys (range))))

(defn load-sha [row-size stype shas]
  (->> shas
       (map (fn [x] (if (br-map x) (br-map x) x)))
       (partition (+ 1 row-size))
       (map #(hash-map :name (first %) :stype stype :bs (apply vector (rest %))))
       ))


(def clash-s (load-sha 10 :s clash-s-0))
(def clash-b (load-sha 12 :b clash-b-0))
(def sha-b1  (load-sha 12 :b sha-b1-0))
(def sha-b2  (load-sha 12 :b sha-b2-0))
(def sha-s   (load-sha 10 :s sha-s-0))

(def cshas (concat clash-s clash-b))
(def shas (concat sha-s sha-b1 sha-b2))

