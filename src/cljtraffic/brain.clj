(ns cljtraffic.brain
  (:import (javax.swing JFrame JPanel)
	   (java.awt Color Graphics)
	   (java.awt.image BufferedImage)))


(defn fmap [f coll] (doall (map f coll)))
(defn fpmap [f coll] (doall (pmap f coll)))
(def dim-board [60 60])
(def dim-screen [400 400])
(def dim-scale (vec (map / dim-screen dim-board)))

(defn render-cell [#^Graphics g cell]
  (let [{state :state x :x y :y} cell
	x (inc (* x (dim-scale 0)))
	y (inc (* y (dim-scale 1)))]
    (doto g
      (.setColor (if (= state :dying) Color/GRAY Color/WHITE))
      (.fillRect x y (dec (dim-scale 0)) (dec (dim-scale 1))))))

(defn render [#^Graphics g img #^Graphics bg stage]
  (.setColor bg Color/BLACK)
  (.fillRect bg 0 0 (dim-screen 0) (dim-screen 1))
  (fmap (fn [col]
	  (fmap #(when (not= :off (:state %))
		   (render-cell bg %)) col)) stage)
  (.drawImage g img 0 0 nil))


(def board
     (apply vector
	    (map (fn [x]
		   (apply vector
			  (map (fn [y]
				 {:state (if (< 40 (rand-int 100)) :on :off) :x x :y y})
			       (range (dim-board 1)))))
		 (range (dim-board 0)))))
				      

(defn torus-window [coll]
  (partition 3 1 (concat [(last coll)] coll [(first coll)])))

(defn activate-neighbors [above [left _ right] below]
  (count
   (filter #(= :on (:state %))
	   (concat above [left right] below))))
		   
(defn rules [above current below]
  (let [cell (second current)
	num-neighbors (activate-neighbors above current below)]
    (assoc cell :state
	   (cond
	    (< num-neighbors 2) :off
	    (> num-neighbors 3) :off
	    (and (= :off (:state cell)) (= num-neighbors 3)) :on 
	    :else (:state cell)))))


(defn rules-x [above current below]
  (let [cell (second current)
	self (:state cell)
	num-neighbors (activate-neighbors above current below)]
    (assoc cell :state
	   (cond
	    (= :on self) :dying
	    (= :dying self) :off
	    (= 2 num-neighbors) :on
	    :else :off))))

(defn rules-sim [above current below]
  (second current))

(defn step1 [board]
  (doall
   (map (fn [window]
          (apply #(doall (apply map rules %&))
                 (doall (map torus-window window))))

	(torus-window board))))

(defn step [board]
  (doall
   (vec
    (pmap (fn [window]
	    (apply #(doall (apply map rules %&))
		   (doall (map torus-window window))))
	  (torus-window board)))))

(def cont-sim (atom true))
(defn brain-loop [#^JPanel surface stage]
  (while @cont-sim
    (swap! stage step)
    (.repaint surface)))

(defn start []
  (let [stage (atom board)
	frame (JFrame.)
	img (BufferedImage. (dim-screen 0) (dim-screen 1) (BufferedImage/TYPE_INT_ARGB))
	bg (.getGraphics img)
	panel (doto (proxy [JPanel] [] (paint [g] (render g img bg @stage))))]
    (doto frame (.add panel) .pack (.setSize (+ 10 (dim-screen 0)) (+ 20 (dim-screen 1))) .show
	  )

    (future (brain-loop panel stage))))