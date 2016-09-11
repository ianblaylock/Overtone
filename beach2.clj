(ns basic.core
  (:use overtone.live))

(def image-x 1000)
(def image-y 600)

(import '(java.awt.image VolatileImage))
(import '(java.awt.geom Path2D$Double AffineTransform Area))
(import '(java.awt GraphicsConfiguration GraphicsEnvironment Color BasicStroke))
(import '(javax.swing JDialog JScrollPane JLabel ImageIcon ))

(def image 
  (let [ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
        gc (.getDefaultConfiguration (.getDefaultScreenDevice ge))
        vi-drawing (.createCompatibleVolatileImage gc image-x image-y)]
    vi-drawing))

(def image-label (-> image ImageIcon. JLabel.))
(def g (.createGraphics image))
(def dialog
  (let [d (javax.swing.JFrame.)
        cp (doto (.getContentPane d)
             (.setLayout (java.awt.GridLayout. 1 1))
             (.add image-label))]
    (doto d
      (.setSize image-x image-y)
      (.setVisible true))))

(def m (metronome 114))
(m :bpm 140)

(defn mean [& a] (apply / ((juxt #(reduce + %) count) a)))

(defn fill-rect
  []
  (.setPaint g Color/BLACK)
  (.fillRect g 0 0 image-x image-y))

(defn draw-poly
  [& {:keys [sides r x y theta]}]
  {:pre [(every? number? [sides r x y theta])
         (>= r 0) (> sides 0)]}
  (fill-rect)
  (.setPaint g (Color/getHSBColor (float (rand)) (float 0.5) (float 1)))
  (.setStroke g (BasicStroke. 1))
  (let [points (vec (doall (map (fn [theta] 
                             [(+ x (* r (Math/cos theta)))
                              (+ y (* r (Math/sin theta)))]) 
                           (take (int sides)
                                 (iterate #(+ % (* Math/PI 2 (/ (int sides)))) theta)))))
        path (let [p (Path2D$Double.)]
               (.moveTo p (get-in points [0 0]) (get-in points [0 1]))
               (doseq [[x y] (rest points)]
                 (.lineTo p x y)) (.closePath p) p)
        paths-lv1
        (map (fn [[[x1 y1] [x2 y2] [x3 y3]]]
               (doto (Path2D$Double.)
                 (.moveTo x y) (.lineTo x1 y1) (.lineTo x2 y2)
                 (.closePath)))
             (take (int (:sides @settings)) (iterate #(conj (vec (rest %)) (first %)) points)))]
                  
    (.draw g path)
    (let [h (atom 0)]
      (doseq [p paths-lv1]
        (doseq [i (range -40 41)]
          (let [a (doto (AffineTransform.)
                    (.scale (Math/pow 0.98 i) (Math/pow 0.99 i)))
                area (Area. p)
                color (Color/getHSBColor (swap! h #(+ 0.03 %)) (float 0.7) (float 1))]
            (.setColor g color)
            (.transform area a)
            (.transform area (doto (AffineTransform.)
                               (.translate (- (-> p .getBounds2D bean :x)
                                              (-> area .getBounds2D bean :x))
                                           (- (-> p .getBounds2D bean :y)
                                              (-> area .getBounds2D bean :y)))))
            (.draw g area)))))
    (.repaint image-label)))
    
    
;               (prn points))]
;    (.draw g path)))

;    (doseq [[[x1 y1] [x2 y2]]
;            (partition 2 
;                       (interleave points (conj (vec (rest points))

;      (let [m1 (/ (- y2 y1) (- x2 x1))
;            b1 (- y1 (* m1 x1))
;            m2 (/ (- y (mean y1 y2)) (- x (mean x1 x2)))
;            b2 (- y (* m2 x))
;            x3 (/ (- b2 b1) (- m1 m2))
;            y3 (+ (* m1 x3) b1)]
;        (.drawString g "*" (float x3) (float y3))
;        (.drawLine g x y (mean x1 x2) (mean y1 y2))
;        (.drawLine g x1 y1 x2 y2)))    
;    (.repaint image-label)))

(definst fm-burst
  [amp 1.0 fm-f1 400 fm-a1 40 freq 400 decay 0.4]
  (let [src (pulse (+ freq (* fm-a1 (pulse fm-f1 (+ 0.5 (* 0.5 (env-gen (perc 0 0.2 1 -3))))))))
        env (env-gen (perc 0 decay 1 -4) :action FREE)]
    (* amp env src)))

(definst hh
  [amp 1.0 decay 0.03]
  (let [src (white-noise)
        env (env-gen (perc 0 decay 1 -1) :action FREE)]
    (* amp 2 src env)))

(definst kick
  [amp 1.0]
  (let [src (pulse (+ 60 (* 500 (env-gen (perc 0 0.2 1 -5))))
                   (+ 0.5 (env-gen (perc 0 0.2 1 0))))
        env (env-gen (perc 0 0.5 1 -1) :action FREE)]
    (* src env amp)))

(def settings (atom
               {:fm-f1 500
                :freq 400
                :decay 0.3
                :fm-a1 500
                :spin-speed1 (/ Math/PI 60)
                :sides 3
                :r 200
                :x0 (int (* 0.5 image-x))
                :y0 (int (* 0.5 image-y));;radians per beat
}))

;;; cool settings 
;{:fm-f1 1000, :freq 500, :fm-a1 1500}
(defn- play
  [beat]
  (let [next-beat (+ beat 1/4)]
    (at (m beat) (javax.swing.SwingUtilities/invokeLater
                  (draw-poly :sides (:sides @settings)
                             :r (:r @settings)
                             :x (:x0 @settings) :y (:y0 @settings)
                             :theta (* beat (:spin-speed1 @settings)))))
    (if (#{0 3 6} (mod (* beat 4) 8))
      (at (m beat) (kick :amp 1.3)))
    (if (#{2} (mod (* beat 4) 4))
      (at (m beat) (fm-burst :freq 3000 :fm-a1 2000 :fm-f1 5000 :decay 0.3)))
    (case (mod (* beat 4) 16)
      0 (dotimes [i (rand-nth (into [] (range 3 7)))]
         (at (m (+ beat (* 1/4 i))) (hh :amp 0.8)))
      8 (let [a (rand-nth [2 3 4 6])]
           (doseq [n (drop (int (+ 1 (* a (rand)))) (for [b (range (* 2 a))]  (/ b a)))]
             (at (m (+ beat n)) (hh :amp 0.8))))
             
      nil)
    (at (m beat) (fm-burst
                  :fm-f1 (* (:fm-f1 @settings) (Math/pow 2 (/ (mod (int beat) 4) 12)))
                  :freq (* (:freq @settings) (Math/pow 2 (/ (mod (int (/ beat 2)) 4) 24)))
                  :decay (:decay @settings)
                  :fm-a1 (:fm-a1 @settings)))
;                  :fm-a1 (cond (int (mod beat 4))
;                               0 400 1 400 2 400 3 400 (:fm-a1 @settings))))
    (at (m beat) (case (int (mod beat 4))
                       0 (:fm-a1 @settings)
                       1 (:fm-a1 @settings)
                       2 (* (:fm-a1 @settings) (Math/pow 2 (/ -3 12)))
                       3 (* (:fm-a1 @settings) (Math/pow 2 (/ 1 12)))))
    (apply-by (m next-beat) #'play [next-beat])))
