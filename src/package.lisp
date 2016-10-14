(defpackage :fd-streams
  (:use :cl :trivial-gray-streams)
  (:export
   :*default-buffer-size*
   :fd-input-stream
   :fd-output-stream))
