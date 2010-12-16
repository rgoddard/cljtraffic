(ns cljtraffic.core (:gen-class)
    (:import (javax.swing JFrame JPanel JOptionPane)
	   (java.awt Color Graphics)
	   (java.awt.image BufferedImage)))

(defn -main [& args]
  (println (str "Available Processors: " (.availableProcessors (Runtime/getRuntime)))))

;;Probability a person will randomly slow down
(def p-dec 0.25)
(def dim-screen [800 30])

(defstruct vehicle :vel :pref-vel :pos :id)
(defstruct cell :car)

(defn fmap [f coll] (doall (map f coll)))
(defn create-car [pos id]
  (struct vehicle 0 5 pos id))

(defn create-lane [length]
  (let [id (atom 0)]
    (apply vector (map
		   (fn [n] (if (< (rand) 0.35) (create-car n (swap! id inc)) nil))
		   (range length)))))

(defn gap [pos num-ahead coll]
  (let [size (count coll)
	end (inc num-ahead)
	g (first (drop-while 
		  (fn [n] (and 
			   (< n end) 
			   (nil? (nth coll (mod (+ pos n) size)))))
		  (range 1 end)))]
    (if (nil? g) num-ahead (dec g))))

(defn update-velocity [car coll]
  (let [v1 (if (< (:vel car) (:pref-vel car)) (inc (:vel car)) (:vel car))]
    (let [g (gap (:pos car) (:pref-vel car) coll)
	  v2 (if (> v1 g) g v1)]
      (let [v3 (if (and (> 0 v2) (< (rand) p-dec)) (dec v2) v2)]
	(assoc car :vel v3 :pos (mod (+ (:pos car) v3) (count coll)))))))

(defn update-position [car]
  (assoc car :pos (+ (:pos car) (:vel car))))

(defn update-lane-velocities [coll]
  (apply vector (map (fn [cell] (if (nil? cell)
		    cell
		    (update-velocity cell coll)))
		     coll)))

(defn get-car-at [coll pos max-vel]
  (if (> 0 max-vel) nil
      (let [car (nth coll (mod (- pos max-vel) (count coll)))]
	(if (and (not (nil? car)) (= pos (:pos car)))
	  car
	  (recur coll pos (dec max-vel))))))
      
(defn update-lane-positions [coll]
  (apply vector (map #(get-car-at coll % 5) (range (count coll)))))

(defn update-lane [coll]
   (doall (update-lane-positions (update-lane-velocities coll))))
		      
(defn render-cell [#^Graphics g car scale frame-scale]
  (let [pos (:pos car)
	vel (:vel car)
	id (:id car)
	x (inc (* (- pos (* vel (- 1 frame-scale))) scale))
	y 0
	c (cond (= 0 (mod id 4)) Color/WHITE
		(= 1 (mod id 4)) Color/BLUE
		(= 2 (mod id 4)) Color/YELLOW
		(= 3 (mod id 4)) Color/RED)]
    (doto g
      (.setColor c)
      (.fillRect x y (dec scale) (dec (dim-screen 1))))))

(defn render [g img bg lane scale frame-scale]
  (.setColor bg Color/BLACK)
  (.fillRect bg 0 0 (dim-screen 0) (dim-screen 1))
  (fmap #(when (not (nil? %))
	   (render-cell bg % scale frame-scale)) lane)
  (.drawImage g img 0 0 nil))

(defn render-frame [surface fps pos frame-num]
  (Thread/sleep (/ 1000 fps))
  (swap! frame-num (fn [_] (/ (* 1.0 pos) fps)))
  (.repaint surface))
		
(defn render-update [surface fps frame-num]
  (doall (for [i (range 1 (inc fps))]
    (render-frame surface fps i frame-num))))
  
(def cont (atom true))
(defn activity-loop [surface lane fps frame-num]
  (while @cont
    (swap! lane update-lane)
    (render-update surface fps frame-num)))

(defn start-cars
  ([length] (start-cars length 1))
  ([length fps]
  (let [lane (atom (create-lane length))
	frame-num (atom 0.0)
	frame (JFrame.)
	img (BufferedImage. (dim-screen 0) (dim-screen 1) (BufferedImage/TYPE_INT_ARGB))
	bg (.getGraphics img)
	scale (/ (dim-screen 0) length)
	panel (doto (proxy [JPanel] [] (paint [g] (render g img bg @lane scale @frame-num))))]
    (doto frame (.add panel) .pack (.setSize 800 80) .show)
    (future (activity-loop panel lane fps frame-num)))))