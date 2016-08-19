(ns basic.core)

(import '(java.nio ByteBuffer ByteOrder))
(import '(java.nio.file Files Path))
(import '(java.io File))
(import '(java.util Date Calendar TimeZone))
(import '(java.text SimpleDateFormat ParsePosition FieldPosition))
(import '(javax.swing JFrame JScrollPane SpringLayout JMenuBar JMenu UIManager JCheckBoxMenuItem JViewport
		      JLabel ImageIcon JPanel JPopupMenu JMenuItem JDialog JSpinner SpinnerNumberModel JSpinner$NumberEditor
		      BoxLayout JCheckBox JComboBox JButton BorderFactory JTextField SwingUtilities
		      InputVerifier JSlider JSeparator ButtonGroup JRadioButton JFileChooser JCheckBoxMenuItem))
(import '(java.awt Color BasicStroke Point Font MouseInfo Window EventQueue Dimension GridLayout FlowLayout
		   GraphicsEnvironment GraphicsConfiguration))
(import '(java.awt.geom Ellipse2D$Double Path2D$Double Rectangle2D$Double Point2D$Double AffineTransform Area))
(import '(java.awt.event MouseAdapter MouseListener MouseMotionListener ActionListener MouseEvent MouseWheelListener
			 ComponentAdapter WindowAdapter))
(import '(javax.swing.event ChangeListener DocumentListener))
(import '(java.awt.image BufferedImage VolatileImage))


(def image
     (let [ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
	   gc (.getDefaultConfiguration (.getDefaultScreenDevice ge))
	   [x-dim y-dim] [1200 675]
           img (doto (.createCompatibleVolatileImage gc x-dim y-dim)
		 (.setAccelerationPriority 1.0))]
       img))

(def image-label (-> image ImageIcon. JLabel.))

(def dialog
     (let [dialog (JDialog.)
	   cp (.getContentPane dialog)]
       (.add cp image-label)
       (doto dialog
	 (.setSize 1200 675)
	 (.setVisible true))))

(defn paint-bright-rect
  [x y w h sat bri]
  (let [g (.createGraphics image)]
    (.setPaint g (Color. (Color/HSBtoRGB (rand) (double sat) (- 1 (* (rand) bri)))))
    (.fillRect g x y w h)
    (.dispose g)))

    
(defn- paint-splat
  [w h x-offset y-offset sat bri rad-mult]
  (let [points (atom [])
	control-points (atom [])
	path (Path2D$Double.)
	g (.createGraphics image)]
    (loop [theta 0]
      (let [next-theta (+ theta (+ 10 (* 30 (rand))))
	    x0 (+ x-offset (* 0.5 w))
	    y0 (+ y-offset (* 0.5 h))
            radius (* 0.25 rad-mult (+ w h))
	    x (+ x0 (* radius (Math/cos (* (/ Math/PI 180) theta))))
	    y (+ y0 (* radius (Math/sin (* (/ Math/PI 180) theta))))
	    rad2 (* radius (+ 0.2 (* (rand) 0.6)))
	    cx (+ x0 (* rad2 0.5 (Math/cos (* (/ Math/PI 180) (+ theta (* (rand) (- next-theta theta)))))))
	    cy (+ y0 (* rad2 0.5 (Math/sin (* (/ Math/PI 180) (+ theta (* (rand) (- next-theta theta)))))))]
	(swap! points conj [x y])
	(swap! control-points conj [cx cy])
	(if (<= next-theta 360) (recur next-theta))))
    (swap! points conj (first @points))
    (swap! control-points conj (first @control-points))
    (.moveTo path (-> @points first first) (-> @points first second))
    (doseq [[[x y] [cx cy]] (partition 2 (interleave (rest @points) @control-points))]
      (.quadTo path cx cy x y))
    (.setPaint g (Color. (Color/HSBtoRGB (rand) (double sat) (- 1 (* (rand) bri)))))
    (.fill g path)
    (.setPaint g (Color. (Color/HSBtoRGB (rand) (double sat) (- 1 (* (rand) bri)))))
    (.setStroke g (BasicStroke. 7))
    (.draw g path)
    (.repaint image-label)
    (.dispose g)))
