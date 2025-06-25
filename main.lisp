(use-package :trivia)

(defstruct (entity (:constructor ent (kind hp x y))) kind hp x y)

(defun draw-map (window map)
  (loop :for x :from 0 :below (array-dimension map 0) :do
    (loop :for y :from 0 :below (array-dimension map 1) :do
      (let ((ch (match (aref map x y)
                  (:wall #\#)
                  (:empty #\.)
                  (:space #\space)
                  (:player #\@)
                  (:monster #\M))))
        (charms:write-char-at-point window ch x y)))))

(defun clamp (value min max)
  (if (< value min) min (if (> value max) max value)))

(defstruct rect x1 x2 y1 y2)
(defconstant +min-room-size+ (+ 3 2))
(defconstant +max-room-size+ (+ 10 2))

(defun make-random-room (map-width map-height state)
  (let* ((width (+ +min-room-size+ (random (+ +max-room-size+ +min-room-size+) state)))
         (height (+ +min-room-size+ (random (+ +max-room-size+ +min-room-size+) state)))
         (x (random (- map-width width) state))
         (y (random (- map-height height) state)))
   (make-rect :x1 x :y1 y :x2 (+ x width -1) :y2 (+ y height -1))))

(defun intersect (a b)
  (and (<= (rect-x1 a) (rect-x2 b))
       (>= (rect-x2 a) (rect-x1 b)) 
       (<= (rect-y1 a) (rect-y2 b))
       (>= (rect-y2 a) (rect-y1 b))))

(defun generate-rooms (map-width map-height)
  (let ((state (make-random-state t))
        (rooms (list)))
    (dotimes (n 200)
       (let ((new-room (make-random-room map-width map-height state)))
         (if (loop :for room :in rooms :always (not (intersect new-room room)))
           (push new-room rooms))))
    rooms))

(defun center (rect)
  (with-slots (x1 x2 y1 y2) rect
    (let ((x (round (/ (+ x1 x2) 2)))
          (y (round (/ (+ y1 y2) 2))))
      (cons x y))))

(defun generate-map (map-width map-height)
  (let* ((map (make-array (list map-width map-height) :initial-element :space))
         (rooms (generate-rooms map-width map-height))
         (entities nil)
         (x (+ 1 (rect-x1 (first rooms))))
         (y (+ 1 (rect-y1 (first rooms))))
         (prev-room nil))
     (loop :for room :in rooms :do
       (if prev-room
         (let* ((pcenter (center prev-room))
                (center (center room))
                (x1 (min (car pcenter) (car center)))
                (x2 (max (car pcenter) (car center)))
                (y1 (min (cdr pcenter) (cdr center)))
                (y2 (max (cdr pcenter) (cdr center))))
           (loop :for x :from (- x1 1) :to x2 :do
             ; (setf (aref map x (cdr pcenter)) :empty)
             (setf (aref map x (- (cdr pcenter) 1)) :wall)
             (setf (aref map x (+ (cdr pcenter) 1)) :wall))
           (loop :for y :from y1 :to y2 :do
             ; (setf (aref map x (cdr pcenter)) :empty)
             (setf (aref map (- (car center) 1) y) :wall)
             (setf (aref map (+ (car center) 1) y) :wall))
           (setf prev-room room))
         (setf prev-room room)))
     (loop :for room :in rooms :do
      (with-slots (x1 y1 x2 y2) room
       (loop :for x :from x1 :below x2 :do
         (loop :for y :from y1 :below y2 :do
           (setf (aref map x y) :empty)))
       (loop :for x :from x1 :to x2 :do
         (setf (aref map x y1) :wall)
         (setf (aref map x y2) :wall))
       (loop :for y :from y1 :to y2 :do
         (setf (aref map x1 y) :wall)
         (setf (aref map x2 y) :wall))))
     (setf prev-room nil)
     (loop :for room :in rooms :do
       (if prev-room
         (let* ((pcenter (center prev-room))
                (center (center room))
                (x1 (min (car pcenter) (car center)))
                (x2 (max (car pcenter) (car center)))
                (y1 (min (cdr pcenter) (cdr center)))
                (y2 (max (cdr pcenter) (cdr center))))
           (loop :for x :from x1 :to x2 :do
             (setf (aref map x (cdr pcenter)) :empty))
           (loop :for y :from y1 :to y2 :do
             (setf (aref map (car center) y) :empty))
           (setf prev-room room))
         (setf prev-room room)))
     (push (ent :player 100 x y) entities)
     (list entities map)))

(defun main ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (charms/ll:curs-set 0)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (when (eql (charms/ll:has-colors) charms/ll:FALSE)
      (error "Your terminal does not support color!"))
    (loop 
      :named main-loop
      :with window = charms:*standard-window*
      ; :with width = (charms:window-dimensions window)
      :with width = 95
      :with height = 55
      :with (entities map) = (generate-map width height)
      :with message = ""
      :do
      (progn
        (loop :for e :in entities :do
          (with-slots (kind x y) e
            (match kind
              (:player
                (let ((new-x x) (new-y y))
                  (match (charms:get-char window :ignore-error t)
                    (#\q (return-from main-loop))
                    (#\h (decf new-x))
                    (#\l (incf new-x))
                    (#\j (incf new-y))
                    (#\k (decf new-y))
                    (#\t (setf message "t: help  j: down  k: up  h: left  l: right")) 
                    (#\space (setf message "")))
                  (setf new-x (clamp new-x 0 (- width 1)))
                  (setf new-y (clamp new-y 0 (- height 1)))
                  (match (aref map new-x new-y)
                    (:empty
                      (setf (aref map x y) :empty)
                      (setf x new-x) (setf y new-y)
                      (setf (aref map x y) :player)))))
              (:monster
                (setf (aref map x y) kind)))))
        (charms:clear-window window) 
        (draw-map window map)
        (charms:write-string-at-point window message 0 0)
        (charms:refresh-window window)
        (sleep 0.01)))))

