(ns basic.core
  (:use overtone.live))

(def m (metronome 132))
;(m :bpm 150)

(definst sq-lead
  [freq 200 amp 1]
  (let [src (square [freq (* 0.30 freq)])
        env (env-gen (perc 0 0.8 1 -1) :action FREE)]
    (* src env amp 2)))

(definst kick1 
  [freq 100 amp 1.0 fm-f1 100/2 fm-a1 10]
  (let [env (env-gen (perc 0 0.30 1 -1) :action FREE)
        src (pulse (+ freq (* fm-a1 (sin-osc fm-f1))) 0.5)]
    (lpf (hpf (* amp env src) 200) 10000)))

(definst kick2
  [freq 60 amp 1]
  (let [src (pulse (+ freq (* 100 (env-gen (perc 0 0.3 1 -2)))))
        env (env-gen (perc 0 0.5 1 0) :action FREE)]
    (* amp src env)))

(definst snare
  [amp 1]
  (let [src (pink-noise)
        env (env-gen (perc 0 0.1 1 10) :action FREE)]
    (hpf (* src env amp) 1000)))

(def settings (atom 
               {:k1-fm-a1 100
               :kick-beats #{0 3}
                :ascend-on true
                :ascend-dur 4
                :ascend-int 0.5
                :ascend-freq 500
                :k1-on true
                :kick-on true
                :snare-on true}))

;    (if (pos? rem)
;      (at (m next-beat) (ascend next-beat (dec rem) next-freq)))))

(defn play 
  [beat]
  (let []
    (if (#{28 31} (int (mod (* beat 4) 32)))
      (swap! settings assoc :k1-fm-a1 
             (* (:k1-fm-a1 @settings) (Math/pow 2 (/ 
                                                   (rand-nth (if (<= (:k1-fm-a1 @settings) 60) [17 25] [-3 -5 2 4 ])) 12)))))
    (if (zero? (int (mod (* beat 4) 64)))
      (at (m beat)
          (swap! settings assoc o:kick-beats
                 (set (map #(mod (+ 1 %) 8) (:kick-beats @settings))))))
    (if (and (:kick-on @settings)
             ((:kick-beats @settings) (int (mod (* beat 4) 8))))
      (at (m beat) (kick2 :amp 2)))
    (if (zero? (int (mod (+ 2(* beat 4)) 4)))
      (at (m beat) (kick1 :freq 5000 :fm-f1 250 :fm-a1 2000)))
    (if (#{0 3} (int (mod (* beat 4) 8)))
      (at (m beat) (kick1 :freq 3000 :fm-f1 250 :fm-a1 10000 :amp 0.5)))

    (if (:k1-on @settings)
      (at (m beat) (kick1 :fm-f1 100
                          :fm-a1 (:k1-fm-a1 @settings)
                          :amp 1.4
                          :freq 100)))
    (if (and (:snare-on @settings) (= 8 (int (mod (* 8 beat) 32))))
      (at (m beat) (snare 10)))
    (if (and (:ascend-on @settings)
             (zero? (int (mod (* 4 beat) 32))))
      (loop [len (:ascend-dur @settings) i 0 b beat]
        (if (<= i len)
          (let [i2 (+ i (rand-nth [0.25 0.5]))]
            (at (m (+ i b)) (sq-lead :freq (* (:ascend-freq @settings) (Math/pow 2 (/ (* (:ascend-int @settings) i) 12)))
                                     :amp 1.2))
            (recur len i2 (+ b (rand-nth [0.25 0.5])))))))
    (apply-by (m (+ beat 0.25)) #'play [(+ beat 0.25)])))
