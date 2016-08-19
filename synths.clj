(ns basic.core
  (:use overtone.live))

(definst pulse-synth
  [amp 0.8 base-freq 60 pw 0.5 offset 1.01 curve -3 decay 0.3]
  (let [;src (pulse (* base-freq) (+ 0.5 (* 0.5 (square 50) pw)))
        src (pulse base-freq pw)
        src2 (pulse (* base-freq offset) pw)
        env (env-gen (perc 0 decay 1 curve) :action FREE)]
    (* amp (* 0.5 (+ src src2)) env)))

(definst pulse-synth2
  [amp 0.8 base-freq 60 pw 0.5 offset 1.01 curve 2 decay 0.3 pitch-mod 1]
  (let [src (hpf (pulse (+ base-freq (env-gen (perc 0 0.3 pitch-mod -3))) pw) 60);(* (sin-osc 0.3 (rand)) 0.5))
        env (env-gen (perc 0 decay 1 curve) :action FREE)]
    (* amp src env)))


;    (* amp src env)))
(definst sin-synth
  [amp 0.8 freq 240 p1 0.9 decay 0.2]
  (let [pw (+ 0.5 (* p1 2))
        src (* p1 (pulse [freq (* freq (+ 0.99 (* 0.02 (rand))))] pw))
        src2 (* (- 1 p1) (lf-tri [freq (* freq (+ 0.99 (* 0.02 (rand))))]))
        env (env-gen (perc 0 decay 1 2) :action FREE)]
    (* amp (+ src src2) env)))

(definst snare
  [amp 0.8 hp-f 200 lp-f 10000 decay 0.25]
  (let [src (pink-noise)
        env (env-gen (perc 0 decay 1 3) :action FREE)]
    (* amp 2 (* env (lpf (hpf src hp-f) lp-f)))))

(definst snare2
  [amp 0.8 attack 0.00 decay 0.1]
  (let [src (hpf (brown-noise) 2000)
        env (env-gen (perc attack decay 1 3) :action FREE)]
    (* src amp 5 env)))
        
(definst hh
  [decay 0.05 amp 0.8]
  (let [src (white-noise)
        env (env-gen (perc 0 decay 1 100) :action FREE)]
    (* amp src env)))

(definst tom
  [pitch-decay 0.05 pitch-curve -3 amp 0.8 freq 300 decay 0.4]
  (let [src (square :freq (+ freq (* freq (env-gen (perc 0 pitch-decay 1 pitch-curve)))))
        env (env-gen (perc 0 decay 1 -8))]
    (* amp src env)))

(definst kick1
  [pitch-decay 0.2 pitch-curve -3 pitch-amp 2
   amp 0.8 freq 120 decay 0.2 curve -1 mod-amp 0.80]
  (let [src (lf-tri :freq (* (+ freq (* freq pitch-amp (env-gen (perc 0 pitch-decay 1 pitch-curve))))
                             (* mod-amp (sin-osc 50))))
        env (env-gen (perc 0 decay 1 curve) :action FREE)]
    (* src env amp)))

(definst kick2
  [pitch-decay 0.3 pitch-curve -5 pitch-amp 50 amp 0.8]
  (let [src (square (+ 60 (* pitch-amp (env-gen (perc 0 pitch-decay 1 pitch-curve)))))
        env (env-gen (perc 0 0.2 1 4) :action FREE)]
    (* src env amp)))


(definst kick3
  [freq 60 lpf-freq 100 amp 0.7 slope -15]
  (let [src (lpf (square (mapv #(+ % (* 10 (env-gen (perc 0 0.4 100 slope)))) [freq freq])) lpf-freq)
        src2 (* 1.2 (sin-osc [freq freq]))
        env (env-gen (perc 0 0.6 5 -3) :action FREE)]
    (* 0.5 (+ src2 src) env amp)))
