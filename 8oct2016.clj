(ns basic.core
  (:use overtone.live))

(def fractions
  (atom {:index1 0
         :index2 0
         :index3 0
         :fracts (let [a (->> (for [i (range 10)] (for [j (range i)] (/ j i)))
                              (apply concat) set (into []))]
                   (sort (concat a (mapv / (remove zero? a)))))}))



(defn set-f-index
  [index val]
  (when (and (->> @fractions :fracts count (<= val))
             (-> val neg? not)
             (#{:index1 :index2 :index3} index))
    (swap! fractions assoc index val)))

(defn inc-f-index
  [index]
  (when (and (#{:index1 :index2 :index3} index)
             (< (inc (index @fractions)) (count (:fracts @fractions))))
    (swap! fractions update index inc)))

(defn dec-f-index
  [index]
  (when (and (#{:index1 :index2 :index3} index)
             (-> @fractions index neg? not))
    (swap! fractions update index dec)))

(definst synth1
  [frac 2/3 amp 1.0 bf 300 decay 0.2 bal 0]
  (let [src (saw (* bf (+ 1 (* frac (sin-osc (* bf frac))))))
        src2 (lf-tri (* bf (+ 1 (* frac (sin-osc (* bf frac))))))
        env (env-gen (perc 0 decay 1 -1) :action FREE)]
    (hpf (* (+ (* (- 1 bal) src) (* bal src2)) env amp) 400)))


(definst kick
  [decay 0.4 bf 300 amp 0.1]
  (let [hit (lpf (* (env-gen (perc 0 0.01 1 2))
                    (brown-noise)) 3000)
        body (* (env-gen (perc 0.01 decay -1) :action FREE)
                (sin-osc (+ 40 (* bf 5/16 (env-gen (perc 0 0.15 1 0))))))]
    (* amp (+ hit body))))

(def m (metronome 130))

(defn play-synth
  [beat]
  (let [next-beat (+ beat 1/4)]
    (at (m beat) (set-f-index :index1 (mod (int (* beat 4)) 32)))
    (dotimes [i 4]
      (at (m (+ beat (* 3 (/ 2) i))) (synth1 :frac (nth (:fracts @fractions) (:index1 @fractions))
                         :amp (* 1 (Math/pow (/ (- 3 i) 3) 0.8))
                         :decay 0.2
                         :bal 1)))
    (apply-by (m next-beat) #'play-synth [next-beat])))

(defn play-kick
  [beat]
  (let [next-beat (+ beat 1/4)]
    (if (#{0 3} (mod (int (* beat 4)) 8))
      (if (>= (rand) 0.1)
        (at (m beat) (kick :amp 3))))
    (apply-by (m next-beat) #'play-kick [next-beat])))
