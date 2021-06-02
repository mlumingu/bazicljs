(ns bazicljs.styles
  (:require-macros
   [garden.def :refer [defcssfn]])
  (:require
   [spade.core   :refer [defglobal defclass]]
   [garden.units :refer [deg px]]
   [garden.color :refer [rgba]]))

(def element-colors ["yellowgreen" "tomato" "khaki" "lightgrey" "lightblue"])
(def text-element-colors ["green" "red" "#ffc700" "grey" "royalblue"])

(defclass element-color [element]
  {:background-color (element-colors element)})

(defclass usefull-color [usefull]
  {:background-color (if usefull "lightblue" "lightsalmon")})



(defglobal defaults
  [:html {:font-family ["ubuntu" :sans-serif]}
   :body {:margin  "0.5em 0.1em 1em 0.2em"
          :padding "0em 0em 0em 0em"}])

(defclass pillars []
  {:display :grid
   :grid-auto-columns "7em"
   :grid-auto-flow :column
   :justify-items :stretch
   :gap "0.2em 0.2em"
   :overflow-x :auto
   })

(defclass relation [element]
  {:display "inline-block"
   :border-radius "3px"
   :background-color element
   :padding "0.1em"})

(defclass active []
  {:background-color :black :color :white})

(defclass palace [col]
  {:grid-column-start col
   :position "relative"
   :font-size "2em"
   :color "black"
   :text-align :center
   :padding "0.3em"
   })


(defclass palace-god []
  {:position "absolute"
   :top "0em"
   :right "0.1em"
   :font-size "0.4em"
   :color "black"})

(defclass palace-qi []
  {:position "absolute"
   :top "0em"
   :left "0.1em"
   :font-size "0.4em"
   :color "black"})

(defclass void []
  {:position "absolute"
   :bottom "0.2em"
   :left "0.2em"
   :font-size "x-small"
   :color "white"
   :background-color "black"
   :padding "0.1em"
   :border-radius "3px"
   })

(defclass hs-god []
  {:font-size "0.4em"
   :color "black"})

(defclass hstem [order]
  {:order order
   :flex 1
   :color "black"
   :text-align :center
   })

(defclass relations [col]
  {:grid-column-start col
   :font-size "small"
   :display "flex"
   :flex-direction "column"
   :align-items "flex-start"
   :gap "0.2em"
   :background-color "lavender"
   :padding "0.2em"
   })

(defclass nayin [element col]
  {:background-color (element-colors element)
   :color "black"
   :font-size "x-small"
   :grid-column-start col
   :text-align :center
   :padding "0.2em 0em 0.2em 0em"
   })

(defclass qi [element col]
  {:grid-column-start col
   :background-color (element-colors element)
   :display          "inline-block"
   ;;:border-radius    "0.3em"
   :width            "1.5em"
   :text-align       "center"
   :padding-top "0.2em"
   :padding-bottom "0.2em"
   })

(defclass qi-base [col]
  {:grid-column-start col
   :display "inline-block"
   ;;:border-radius "0.3em"
   :width "1.5em"
   :text-align "center"
   :padding-top "0.2em"
   :padding-bottom "0.2em"
   :background-color :lightsteelblue
   :color :black})

(defclass qi-stages [col]
  {:grid-column-start col
   :display "grid"
   :gap "0.3em 0.3em"
   :font-size     "x-small"
   :grid-auto-flow :column
   :justify-content :start
   :align-content :start
   :background-color :lavender
   })
