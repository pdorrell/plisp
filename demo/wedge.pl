;;; The wedge program as in the PLisp document.

;; generates wedge.eps

(library "c:/dev/work/plisp/demo/my-macros.plisp")

(eps 0 0 400 200)

(defun inch (x) 1 (* x 72))

(defun wedge (angle) 0
  (let ( (half-angle (/ angle 2.0)) )
    (with-newpath-closepath
     (moveto 0 0)
     (translate 1 0)
     (rotate half-angle)
     (translate 0 (sin half-angle))
     (arc 0 0 (sin half-angle) -90 90) ) ) )

(with-gsave-grestore
 (translate (inch 2.75) (inch 6.5))
 (scale (inch 1) (inch 1))
 (wedge 10)
 (setlinewidth 0.02)
 (stroke) )

(defun flower (num-petals) 0
  (let ( (angle (/ 360.0 num-petals)) )
    (dotimes (i num-petals)
      (setgray (- 1.0 (* 0.3 (/ (1+ i) num-petals))))
      (with-gsave-grestore
       (wedge angle)
       (with-gsave-grestore (fill))
       (setgray 0)
       (stroke) )
      (rotate angle)) ) )
    

(with-gsave-grestore
 (translate (inch 4.25) (inch 9.25))
 (scale (inch 1.74) (inch 1.75))
 (setlinewidth 0.02)
 (flower 100) )
