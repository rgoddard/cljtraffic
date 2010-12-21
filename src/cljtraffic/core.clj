(ns cljtraffic.core (:gen-class)
    (:use clojure.contrib.profile)
    (:import (javax.swing JFrame JPanel JOptionPane)
	   (java.awt Color Graphics)
	   (java.awt.image BufferedImage)))

(defn -main [& args]
  (println (str "Available Processors: " (.availableProcessors (Runtime/getRuntime)))))


(def num-threads (atom 1))
(defn set-num-threads [n]
  (swap! num-threads (fn [_] n)))

(defn omp-pmap 
  "works similar to pmap, except number of threads given by num_threads, instead of num procs + 2.
To change the number of threads call set_num_threads"
  [f coll]
  (let [n @num-threads
         rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets))))

(defn omp-pmap-part
  "Takes a function f and sequence that has already been partitioned, and for each chunk in the sequence,
maps over the chunk with f, returns as a single sequence"
  [f coll]
  (apply concat (omp-pmap (fn [chunk] (doall (map f chunk))) coll)))

;;Probability a person will randomly slow down
(def p-dec 0.25)
(def dim-screen [800 30])
(def my-map map)

(defstruct vehicle :vel :pref-vel :pos :id)

(defn create-car [pos id]
  (struct vehicle 0 5 pos id))

(defn create-lane-cond [length cond-create]
  ;creates a lane of the given length, a cell will only contain a car if cond-create evaluates to true
  (let [id (atom 0)]
    (apply vector (doall (map
     (fn [n] (if (cond-create n) (create-car n (swap! id inc)) nil))
     (range length))))))

(defn create-lane [length]
  (create-lane-cond length (fn [_] (< (rand) 0.35))))

(defn create-lane-third [length]
  (create-lane-cond length (fn [n] (= 0 (mod n 3)))))

(defn gap [pos num-ahead lane size]
  (let [end (inc num-ahead)
	g (first (drop-while 
		  (fn [n] (and 
			   (< n end) 
			   (prof :gap-nth (nil? (nth lane (mod (+ pos n) size))))))
		  (range 1 end)))]
    (if (nil? g) num-ahead (dec g))))

(defn update-velocity [car pos lane size]
  (let [vel (:vel car) pref-vel (:pref-vel car)
	v1 (prof :v1 (if (< vel pref-vel) (inc vel) vel))]
    (let [g (prof :gap (gap pos pref-vel lane size))
	  v2 (prof :v2 (if (> v1 g) g v1))]
      (let [v3 (prof :v3 (if (and (> 0 v2) (< (rand) p-dec)) (dec v2) v2))]
	(prof :assoc-vel (assoc car :vel v3))))))


(defn update-velocities
  ([lane]
     (update-velocities lane (range (count lane)) (count lane)))
  ([lane map-coll size]
     (vec (my-map (fn [pos]
	       (let [cell (nth lane pos)]
		 (if (nil? cell)
		   cell
		   (update-velocity cell pos lane size))))
	     map-coll))))

(defn update-array-velocities [arr]
  (let [size (count arr)]
    (loop [ix 0]
      (if (= ix size)
	arr
	(do 
	  (if (not (nil? (nth arr ix)))
	      (aset arr ix (update-velocity (nth arr ix) arr size)))
	    (recur (inc ix)))))))

(defn get-car-at [coll pos max-vel]
  (if (> 0 max-vel) nil
      (let [car (nth coll (mod (- pos max-vel) (count coll)))]
	(if (and (not (nil? car)) (= max-vel (:vel car)))
	  car
	  (recur coll pos (dec max-vel))))))
      
(defn update-positions
  ([lane]
     (update-positions lane (range (count lane))))
  ([lane map-coll]
     (vec (my-map #(get-car-at lane % 5) map-coll))))

(defn update-lane
  ([lane]
     (update-lane lane (range (count lane)) (count lane)))
  ([lane map-coll size]
     (doall (update-positions (update-velocities lane map-coll size) map-coll))))

(defn lane-app [size num-partitions]
  (let [lane (atom (create-lane-third size))
	[map-coll map-fn] (if (and num-partitions (> num-partitions 1))
			    [(doall (partition-all (/ size num-partitions) (range size))) omp-pmap-part]
			    [(doall (range size)) map])]
    {:lane lane
     :map-coll map-coll
     :map-fn map-fn
     :update-lane (fn [] (swap! lane 
				(fn [l]
				  (binding [my-map map-fn] (update-lane l map-coll size)))))}))

(defn benchmark
  ([]
     (benchmark [10 100 200] [1 2 4] 1000))
  ([sizes threads iter]
     (map (fn [size]
	    (map (fn [num-threads]
		   (let [test-app (lane-app size num-threads)
			 start (. System (nanoTime))
			 ret (dotimes [n iter] ((:update-lane test-app)))]
		     {:time-secs (/ (double (- (. System (nanoTime)) start)) 1000000000.0)
		      :size size
		      :iter iter
		      :threads num-threads}))
		 threads))
	  sizes)))

(defn display-results [res]
  (do
    (doall
     (map (fn [threads]
	    (doall (map (fn [trial]
		   (println (str trial)))
		 threads)))
	  res))
    nil))
	   
					;Timing and testing map

(def part-size 200)
(defn pmap-part
  ([f coll]
     (pmap-part f coll part-size))
  ([f coll size]
     (pmap-part f coll part-size (partition-all size coll)))
  ([f coll size part]
     (apply concat (pmap (fn [chunk] (map f chunk)) part))))

(defn time-test [lane map-fn]
  (time (first (binding [my-map map-fn] (update-lane lane)))))

(defn time-test-map [lane]
  (time-test lane map))

(defn time-test-pmap [lane]
  (time-test lane pmap))

(defn time-test-part [lane]
  (time-test lane pmap-part))


					;For graphical display...

(defn fmap [f coll] (doall (map f coll)))
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
(defn reset-cont []
  (swap! cont (fn [_] true)))
(defn stop-cont []
  (swap! cont (fn [_] nil)))

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