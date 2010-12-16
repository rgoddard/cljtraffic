(ns cljtraffic.hw3
  (:use [clojure.contrib.generic.math-functions :only (pow sin sqrt)]))

(def threshold (pow 10 -5))

(defn print-mat [mat]
  (doseq [row mat] (println row)))

(defn iterate-mat [size f]
  (map
   (fn [y] (map
	    (fn [x] (f x y))
	    (range size)))
   (range size)))


(defn get-val [mat x y]
  (nth (nth mat y) x))

(defn calc-next-val [mat x y]
  (let [top (get-val mat x (dec y))
	bottom (get-val mat x (int y))
	left (get-val mat (dec x) y)
	right (get-val mat (inc x) y)]
    (/ (+ top bottom left right) 4.0)))

(defn calc-next [cur-mat size]
  (let [bound (dec size)]
    (iterate-mat
     size
     (fn [x y]
       (if (or (= 0 x) (= 0 y) (= bound x) (= bound y))
	 (get-val cur-mat x y)
	 (calc-next-val cur-mat x y))))))

(defn norm-mat [mat-a mat-b size]
  (iterate-mat
   size
   (fn [x y]
     (pow (- (get-val mat-a x y)  (get-val mat-b x y)) 2))))
  

(defn norm [mat-a mat-b size]
  (sqrt
   (apply +
	  (map (fn[r] (apply + r))
	       (norm-mat mat-a mat-b size)))))

(defn converged? [mat-a mat-b size]
  (< (norm mat-a mat-b size) threshold))

(defn run-calc [cur-mat next-mat size num-steps]
  (if (and (= 0 (mod num-steps 40)) (converged? cur-mat next-mat size))
    [num-steps next-mat]
    (recur next-mat (calc-next next-mat size) size (inc num-steps))))


(defn init-mat [size]
  (let [delta (/ 1.0 (dec size))]
    (iterate-mat size
		 (fn [x y]
		   (cond
		     (= 0 x) 0
		     (= (dec size) x) 0
		     (= 0 y) (sin (* x delta Math/PI))
		     (= (dec size) y) (* (sin (* x delta Math/PI)) (pow Math/E (- Math/PI)))
		     :else 0.5)
		   ))))

(defn start [size]
  (let [mat (init-mat size)]
    (run-calc mat (calc-next mat size) size 0)))
