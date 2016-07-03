(ns derp)

(defn num-seq
  ([dx] (num-seq 0 dx))
  ([n dx] (lazy-seq (cons n (num-seq (+ n dx) dx)))))

(defn- amp-on-curve
  [t mn-amp mx-amp end-t curve]
  {:pre [(<= mx-amp 1) (>= mn-amp 0) (<= mn-amp mx-amp)
	 (> curve 0)
	 (<= t end-t) (<= 0 t)]}
  (+ mn-amp (* (- mx-amp mn-amp) (Math/pow (/ t end-t) curve))))

(defn- make-offsets
  [i maximum base-freq interval temperament]
  (let [dx (Math/pow 2 (- (* 4 i)))
	nums (take-while #(<= % (* 0.5 maximum)) (num-seq dx))
	freqs (->> nums (map-indexed (fn [i j] (* base-freq (Math/pow 2 (/ (* i interval) temperament))))))
	mirrored-nums (->> nums (map #(- maximum %)) reverse (concat nums))
	mirrored-freqs (->> freqs reverse (concat freqs))
	n-count (loop [[num & rem] (partition 2 (interleave mirrored-nums mirrored-freqs)) h {}]
		  (let [h2 (if (h num) (assoc h num (inc (h num))) (assoc h num 1))]
		    (if rem
		      (recur rem h2) h2)))]
    (->> n-count keys (sort-by first) butlast)))
    