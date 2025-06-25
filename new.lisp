(ql:quickload :cffi)
(ql:quickload :tcod)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *limit-fps* 20)

(tcod:console-init-root *screen-width* *screen-height* :title "lisp roguelike"
  :fullscreen? nil)
(tcod:sys-set-fps *limit-fps*)
