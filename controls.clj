(ns basic.core
  (:use overtone.live))

(on-event [:midi :note-on]
          (fn [e]
            (let [[ctrl value] (map #(% e) [:data1 :velocity-f])]
              (case ctrl
                60 (if (-> @track-config :hh-on)
                     (swap! track-config assoc :hh-on nil)
                     (swap! track-config assoc :hh-on true))
                62 (if (-> @track-config :kick-on)
                     (swap! track-config assoc :kick-on nil)
                     (swap! track-config assoc :kick-on true))
                64 (if (-> @track-config :tom-on)
                     (swap! track-config assoc :tom-on nil)
                     (swap! track-config assoc :tom-on true))
                65 (swap! track-config assoc :pattern (-> @track-config :pattern reverse))
                76 (stop)
                77 (repeating-pulse (m))
                nil)))
          :keyboard2)

(on-event [:midi :control-change]
          (fn [e]
            (let [[ctrl value] (map #(% e) [:data1 :velocity-f])]

              (case ctrl
                12 (swap! track-config assoc :pulse-amp (* 3 value))
                13 (do
                     (swap! track-config assoc :pulse-decay (* 1.0 value))
                     (swap! track-config assoc :layer-1-dim (inc (int (* 20 (Math/pow value 6))))))
                14 (do
                     (swap! track-config assoc :pulse-offset (+ 0.96 (* 0.08 value)))
                     (swap! track-config assoc :layer-1-color-scale (* 2 (Math/abs (- value 1/2)))))
                15 (swap! track-config assoc :pw-diff-mult (+ -1 (* 2 value)))
                16 (swap! track-config assoc :pulse-curve (- 5 (* 10 value)))

                18 (do
                     (swap! track-config assoc :kick2-pitch-amp (* 100 (Math/pow 2 (* 6 value))))
                     (swap! track-config assoc :layer-2-dim (inc (int (* 10 (Math/pow value 3))))))
                19 (do
                     (swap! track-config assoc :kick2-amp (* value 0.7))
                     (swap! track-config assoc :splat-size (Math/pow value 2)))
                22 (swap! track-config assoc :hh-decay (* 0.09 value))
                23 (swap! track-config assoc :snare-hpf (* 50 (Math/pow 2 (* 8 value))))
                24 (swap! track-config assoc :tom-int (+ 3 (* 2 (int (* 3 value)))))
                25 (swap! track-config assoc :tom-freq (* 150 (Math/pow 2 (inc value))))
                26 (swap! track-config assoc :tom-mult (- 2 (* 2 value)))
                27 (do
                     (swap! track-config assoc :tom-amp (+ 0.8 value))
)
                nil)))
          :keyboard)

(on-event [:midi :control-change]
          (fn [e]
            (let [[ctrl value] (map e [:data1 :velocity-f])]
              (case ctrl
                12 (swap! p2config assoc :kick-amp (* 2 value))
                13 (swap! p2config assoc :synth-pw (+ 0.5 (* 0.5 value)))
                14 (swap! p2config assoc :synth-decay (+ 0.15 (* 0.4 value)))
                15 (swap! p2config assoc :synth-amp (* 0.8 value))
                16 (swap! p2config assoc :synth2-cycle-length (+ 1 (int (* 11 value))))
                17 (swap! p2config assoc :synth2-decay (* 0.2 value))
                18 (swap! p2config assoc :synth2-mod (* 0.98 value))
                19 (swap! p2config assoc :kick-lpf (* 100 (Math/pow 2 (* 5 value))))
                22 (swap! p2config assoc :hh-decay (* 0.06 value))
                nil)))
          :keyboard)

(on-event [:midi :note-on]
          (fn [e]
            (let [[ctrl value] (map #(% e) [:data1 :velocity-f])]
              (case ctrl
                60 (swap! p2config assoc :hh-on (if (:hh-on @p2config) nil true))
                62 (swap! p2config assoc :snare-on (if (:snare-on @p2config) nil true))
                64 (swap! p2config assoc :kick-on (if (:kick-on @p2config) nil true))
                65 (swap! p2config assoc :base-freq2 (* (:base-freq1 @p2config) (Math/pow 2 (/ (rand-nth [-4 -2 0.02 3 5 6 7]) 12))))
                67 (swap! p2config assoc :kick-beats
                          (set (conj (take (int (* 5 (rand)))(shuffle [1 4 3 9 10])) 0)))
                76 (stop)
                77 (pattern2 (m))
                nil)))
          :keyboard2)
