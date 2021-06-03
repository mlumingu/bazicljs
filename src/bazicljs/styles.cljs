(ns bazicljs.styles
  (:require-macros
   [garden.def :refer [defcssfn]])
  (:require
   [garden.stylesheet :refer [at-media]]
   [spade.core   :refer [defglobal defclass]]
   [garden.units :refer [deg px]]
   [garden.color :refer [rgba]]))

(def element-colors ["yellowgreen" "tomato" "gold" "darkgray" "lightblue"])
(def text-element-colors ["green" "red" "#ffc700" "grey" "royalblue"])
(def text-useful-colors ["royalblue" "red"])
(defclass element-color [element]
  {:background-color (element-colors element)})
(defclass usefull-color [usefull]
  {:background-color (if usefull "lightblue" "lightsalmon")})
(defclass element-text-color [element]
  {:color (text-element-colors element)})
(defclass useful-text-color [useful]
  {:color (if useful "darkblue" "red")})




(defglobal defaults
  [:html {:font-family ["ubuntu" :sans-serif]}
   (at-media {:max-width "700px"}
             [:body {:font-size "10px"}])
   (at-media {:min-width "700px" :max-width "1050px"}
             [:body {:font-size "14px"}])
   :body {:margin  "0.5em 0.1em 1em 0.2em"
          :padding "0em 0em 0em 0em"
          :font-size "16px"
          }])




(defclass active []
  {:background-color "black"
   :color :white
   :border-radius "2px"
   :border "none"
   })




(defclass pillars []
  {:display :grid
   :grid-auto-columns "7em"
   :grid-auto-flow :column
   :justify-items :stretch
   :gap "0.2em 0.2em"
   :overflow-x :auto
   })

(defclass grid-base []
  {:border-radius "3px"
   :background "#efefef"
   :min-height "1em"
   :padding "0.2em"})

(defclass slug [col]
  {:grid-column-start col
   :background-color "black"
   :color "white"
   :text-align :center
   :border-radius "3px"})

(defclass empty-luck [col]
  {:grid-column-start col
   :grid-row "2 / 4"
   :background-color :grey
   :min-height "2em"
   :height "100%"
   })

(defclass palace [col]
  {:grid-column-start col
   :position "relative"
   :font-size "2em"
   :color "black"
   :text-align :center
   :padding "0.3em"
   :border-radius "3px"
   })

(defclass hstems [col]
  {:grid-column-start col
   :display "flex "
   :font-size "1.5em"
   :justify-content :center
   :gap "0.2em"})

(defclass nayin [element col]
  {:background-color (element-colors element)
   :color "black"
   :font-size "0.6em"
   :grid-column-start col
   :text-align :center
   :padding "0.2em 0em 0.2em 0em"
   })




(defclass relations [col]
  {:grid-column-start col
   :font-size "0.8em"
   :display "flex"
   :flex-direction "column"
   :align-items "flex-start"
   :gap "0.2em"
   })

(defclass sha [col]
  {:grid-column-start col
   :font-size "0.8em"
   :display "flex"
   :flex-direction "column"
   :gap "0em"
   })

(defclass qi-stages [col]
  {:grid-column-start col
   :display "grid"
   :gap "0.3em 0.3em"
   :font-size     "0.6em"
   :grid-auto-flow :column
   :justify-content :start
   :align-content :start
   :padding "0.4em"
   })




(defclass none-palace []
  {:border "solid 1px"})

(defclass palace-names []
  {:font-size "0.4em"})

(defclass palace-god []
  {:position "absolute"
   :top "0.1em"
   :right "0.3em"
   :font-size "0.4em"
   :color "black"})

(defclass palace-qi []
  {:position "absolute"
   :top "0.1em"
   :left "0.3em"
   :font-size "0.4em"
   :color "black"})

(defclass void []
  {:position "absolute"
   :bottom "0.4em"
   :left "0.4em"
   :font-size "0.3em"
   :color "white"
   :background-color "black"
   :padding "0.1em"
   :border-radius "3px"
   })




(defclass hstem [order]
  {:order order
   :flex 1
   :color "black"
   :text-align :center
   :border-radius "3px"
   })

(defclass hs-god []
  {:font-size "0.4em"
   :color "black"})




(defclass relation [element]
  {:display "inline-block"
   :border-radius "3px"
   :background-color element
   :padding "0.1em"})






(defclass qi [element col]
  {:grid-column-start col
   :background-color (element-colors element)
   :display          "inline-block"
   :border-radius    "0.3em"
   :width            "1.5em"
   :text-align       "center"
   :padding-top "0.2em"
   :padding-bottom "0.2em"
   })

(defclass qi-base [col]
  {:grid-column-start col
   :display "inline-block"
   :border-radius "0.3em"
   :width "1.5em"
   :text-align "center"
   :padding-top "0.2em"
   :padding-bottom "0.2em"
   :background-color :lightsteelblue
   :color :black})


