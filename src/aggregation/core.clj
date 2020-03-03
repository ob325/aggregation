(ns aggregation.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn square [x] (* x x)) 
(def new-stuck-color (atom 0))
(def particle-size 7)
(def sq-particle-size (square particle-size))
(def n-free-particles 1000)
(def base-hue 90)
(def stuck-color [base-hue 200 20 255])
(def unstuck-color [base-hue 200 20 255])
(def max-step 8)

(defn add-particles 
  "Create the initial particles"
  [state]
  (let [n-to-add (- n-free-particles (count state))]
    (reduce (fn [s n]
              (cons {:stuck false
                     :x-position 0
                     :y-position 0
                     :size particle-size
                     :color unstuck-color} s))
            state
            (range n-to-add))))


(defn setup
  "Quil setup function"
  []
  (q/frame-rate 120)
  (q/color-mode :hsb)
  ;; initial seed(s)
  (add-particles (reduce (fn [a b]
                           (cons {:stuck true
                                  :x-position (* (* 0.45 (q/width)) (q/sin b))
                                  :y-position (* (* 0.45 (q/height)) (q/cos b))
                                  :size particle-size
                                  :color stuck-color}
                                 a))
                         [] (range 0 6.28 (/ 6.28 13)))))
               
  
(defn random-move 
  "Return a random integer between -1 and 1 
   (multiplied by jumpiness parameters), inclusive"
  [ ]
  (+ (* -1 max-step)
     (rand-int (- max-step (* -1 (inc max-step))))))


(defn valid-next-position 
  "If random move would take particle out-of-bounds, don't move it"
  [current]
  (let [next (+ current (random-move))]
    (if (<= (* -1/2 (q/width)) next (* 1/2 (q/width)))
      next
      current)))


(defn dist 
  "Returns squared Euclidean distance between two maps with x and y keys"
  [a b]
  (+ (square (- (:x-position a) (:x-position b)))
     (square (- (:y-position a) (:y-position b)))))


(defn move-particles 
  "Jitter the positions of free particles"
  [state]
  (map (fn [particle]
         (if (= true (:stuck particle))
           particle
           (-> particle
               (assoc :x-position (valid-next-position (:x-position particle)))
               (assoc :y-position (valid-next-position (:y-position particle))))))
       state))


(defn color-picker
  "Returns a color for newly stuck particles"
  [old new curr]
  (if (and (not= old true)
           (= new true))
    [base-hue 200 (deref new-stuck-color) 255]
    curr))


(defn flag-if-stuck 
  "If any free particle is now touching a stuck particle, 
   set that particle's :stuck to true, and color based on current number of stuck"
  [state]
  (let [stuck (filter #(= true (:stuck %)) state)
        new-stuck (->> (for [all state
                             stuck stuck]
                         (dist all stuck))
                       (partition (count stuck))
                       (map (fn [x]
                              (some #(<= % sq-particle-size) x))))
        current-color (map :color state)
        new-colors (map #(assoc %1 :color (color-picker (:stuck %1) %2 %3))
                        state new-stuck current-color)]
    (map #(assoc %1 :stuck %2) new-colors new-stuck)))

(defn update-state 
  "Quil update function"
  [state]
  ;; value of atom based on current # stuck, adjusted to map to range of color values
  ;; needs improvement!
  (reset! new-stuck-color (+ 20
                         (int (* 0.1 (count (filter #(= true (:stuck %)) state))))))
    (->> state
       (move-particles)
       (flag-if-stuck)))

(defn draw-state
  "Quil draw function"
  [state]
  (q/background 10)
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    (doseq [s state]
      (apply q/fill (:color s))
      (q/no-stroke)
      (q/ellipse (:x-position s)
                 (:y-position s)
                 (:size s)
                 (:size s)))))

(q/defsketch aggregation
  :title "Diffusion-limited aggregation"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
