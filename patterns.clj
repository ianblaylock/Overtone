(ns basic.core
  (:use overtone.live))

(def m (metronome 120))
;(m :bpm 145)

(def track-config
  (atom {:count 0
         :pulse-offset 1.00
         :pw-diff [0.4 0 0.45 0.49]
         :pw-diff-mult 1
         :pulse-amp 0.7
         :pulse-decay 0.2
         :pulse-curve -3
         :snare-hpf 200
         :hh-on true
         :hh-decay 0.02
         :kick2-pitch-amp 50
         :kick2-amp 0.8
         :tom-int 3
         :tom-freq 150
         :tom-mult 1.4
         :tom-on nil
         :tom-amp 0.8
         :layer-1-color-scale 0.0
         :layer-1-dim 1
         :layer-2-color-scale 0.0
         :layer-2-dim 1}))

(defn repeating-pulse
  [beat]
  (let [next-beat (+ beat 1/4)
        pw-amp (if (zero? (mod beat 8))
                 (-> @track-config :pulse-amp (* 0.2))
                 (-> @track-config :pulse-amp))
        snare-hpf (-> @track-config :snare-hpf)]

    (when (and (<= 2 (mod beat 8))
             (>= 6 (mod beat 8)))
      (at beat (kick2 :pitch-amp (-> @track-config :kick2-pitch-amp)
                      :amp (-> @track-config :kick2-amp))))
    
    (let [i (mod (* beat 4) 8)
          i2 (mod beat 8)
          dim (:layer-1-dim @track-config)
          dim2 (:layer-2-dim @track-config)
          color-toggle-1 (:layer-1-color-scale @track-config)]
      (doseq [[x y] (for [x (range 0 dim) y (range 0 dim)] [x y])]
        
        (future (paint-bright-rect (+ (* (.getWidth image-label) (/ x dim))
                              (* (.getWidth image-label) (/ i (* 16 dim))))
                           (+ (* (.getHeight image-label) (/ y dim))
                              (* (.getHeight image-label) (/ i (* 16 dim))))
                           (* (.getWidth image-label) (/ (- 8 i) (* (inc i) dim)))
                           (* (.getHeight image-label) (/ (- 8 i) (* (inc i) dim)))
                           color-toggle-1
                           (- 1 color-toggle-1))))
      (if (and (<= 2 (mod beat 8))
               (>= 6 (mod beat 8)))
        (doseq [[x y] (for [x (range 0 dim2) y (range 0 dim2)] [x y])]
          (future (paint-splat (/ (.getWidth image-label) dim2)
                               (/ (.getHeight image-label) dim2)
                               (* (.getWidth image-label) (/ x dim2))
                               (* (.getHeight image-label) (/ y dim2))
                               color-toggle-1
                               (- 1 color-toggle-1)
                               (:splat-size @track-config)))))
      (.repaint image-label))

    (if (-> @track-config :kick-on)
      (if (zero? (mod (* beat 4) 4))
        (at beat (kick1 :amp 2 :pitch-curve 2))))
    (if (zero? (mod beat 8))
      (at beat (snare :amp 8 :hp-f snare-hpf)))
    (if (= 31 (mod (* 2 beat) 32))
      (at beat (snare :amp 3 :hp-f (/ snare-hpf 8) :lp-f 2000)))

    (if (-> @track-config :hh-on)
      (at beat (hh :decay (-> @track-config :hh-decay))))
    
    (at beat (pulse-synth :amp pw-amp
                          :pw (+ 0.5 (* (-> @track-config :pw-diff-mult)
                                        (-> @track-config :pw-diff (nth (mod (int (/ beat 2)) 4)))))
                          :decay (-> @track-config :pulse-decay)
                          :offset (-> @track-config :pulse-offset)
                          :base-freq (cond (#{4 12} (mod (* 2 beat) 16))
                                           (* 64 (* (rand-nth [0.5 1 2 ]) 2))
                                           (#{3 11} (mod (* 2 beat) 16))
                                           (* 64 (* (rand-nth [0.5 1 2 ]) 1.8))
                                           (#{14 15 0 1} (mod (* 2 beat) 16))
                                           (* 64 (rand-nth [0.9 1.05]))
                                           :else 64)
                          :curve (-> @track-config :pulse-curve)))
    (apply-at (m next-beat) #'repeating-pulse [next-beat])))


(defn arp-fn
  [n]
  (+ (nth [-2 0 5 7] (mod n 4)) (* (int (/ n 4)) 12)))
  

(def p2config (atom {:kick-lpf 100.0,
                     :synth2-cycle-length 3,
                     :synth-pw 0.9685039222240448,
                     :kick-beats #{0},
                     :synth-amp 0.11968504190444947,
                     :snare-on nil,
                     :synth-curve 2,
                     :kick-on nil,
                     :hh-on true,
                     :hh-decay 0.01464566946029663,
                     :synth2-decay 0.0,
                     :kick-slope -15,
                     :synth2-mod 0.0,
                     :base-freq2 180.20806431368825,
                     :base-freq1 180,
                     :synth-decay 0.2948818922042847,
                     :synth-pitch-mod 20,
                     :kick-amp 1.118110179901123}))


(defn- pattern2
  [beat]
  (let [next-beat (+ beat 1/4)
        kick-modulo (mod (* beat 4) 12)
        synth2-mod (mod (mod (* beat 4) 12)  (:synth2-cycle-length @p2config))]
    (at (m beat) (pulse-synth2 :base-freq (:base-freq1 @p2config)
                               :pw (:synth-pw @p2config)
                               :decay (:synth-decay @p2config)
                               :amp (:synth-amp @p2config)))
    (at (m beat) (pulse-synth2 :base-freq (:base-freq2 @p2config)
                               :pw (:synth-pw @p2config)
                               :decay (:synth-decay @p2config)
                               :amp (:synth-amp @p2config)))

                                        ;(at (m beat) (sin-synth :freq (* 120 (Math/pow 2 (/ (sin-synth-hash synth2-mod) 12)))))
    (at (m beat) (sin-synth :freq (* 120 (Math/pow 2 (/ (arp-fn synth2-mod) 12)))
                            :amp 0.6
                            :p1 (:synth2-mod @p2config)
                            :decay (:synth2-decay @p2config)))

    (if ((:kick-beats @p2config) kick-modulo)
      (do (at (m beat) (kick3 :amp (if (zero? kick-modulo) (* 1.3 (:kick-amp @p2config)) (:kick-amp @p2config))                              
                              :lpf-freq (:kick-lpf @p2config))))
      (do (if (:hh-on @p2config) (at (m beat) (hh :decay (:hh-decay @p2config) :amp 0.4)))))

    (if (and (#{6} kick-modulo) (:snare-on @p2config))      
      (if (>= 0.5 (rand))
        (do (at (m beat) (snare :amp 4 :decay 0.26 :hp-f 1000)))
        (do (at (m beat) (snare :amp 3 :decay 0.03))
           (dotimes [i (int (* 6 (rand)))]
             (at (m (+ beat (/ (inc i) 4))) (snare :amp 3 :decay (+ 0.03 (* 0.015 i))))))))

;    (if (#{11} kick-modulo)
;      (do (at (m beat) (kick2 :amp 1.1))))

    (apply-at (m next-beat) #'pattern2 [next-beat])))


   
;(defn start
;  []
;  (repeating-pulse (m))
;  (repeating-snare (m))
;  (pulse-mod (m)))
