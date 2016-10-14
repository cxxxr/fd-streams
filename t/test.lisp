(in-package :cl-user)
(defpackage :fd-streams-test
  (:use :cl :fd-streams :prove))
(in-package :fd-streams-test)

(plan nil)

(defun filename (filename)
  (merge-pathnames (format nil "t/~A" filename)
                   (asdf:system-source-directory :fd-streams)))

(defun unique-filename ()
  (loop for n from 0
        for filename = (filename (format nil "TMPFILE-~D" n))
        do (unless (uiop:file-exists-p filename)
             (return filename))))

(defun cleanup-temp-files ()
  (loop for pathname
          in (directory
              (merge-pathnames "*.*"
                               (merge-pathnames "t/"
                                                (asdf:system-source-directory :fd-streams))))
        when (ppcre:scan "TMPFILE-\\d+" (pathname-name pathname))
          do (delete-file pathname)))

(defun read-file (filename)
  (let* ((filename filename)
         (fd (sb-posix:open filename sb-unix:o_rdonly))
         (stream (make-instance 'fd-input-stream :fd fd :buffer-size 10)))
    (unwind-protect
         (loop for (str eofp) = (multiple-value-list (read-line stream nil t))
               collect (cons eofp str)
               until eofp)
      (close stream)
      (sb-posix:close fd))))

(defun write-file (filename strings &optional (buffer-size *default-buffer-size*))
  (let* ((filename filename)
         (fd (sb-posix:open filename
                            (logior sb-unix:o_wronly
                                    sb-unix:o_creat
                                    sb-unix:o_trunc)
                            #o666))
         (stream (make-instance 'fd-output-stream :fd fd :buffer-size buffer-size)))
    (unwind-protect
         (loop for str in strings
               do (princ str stream))
      (close stream)
      (sb-posix:close fd))
    (uiop:read-file-string filename)))

(defun peek-test ()
  (let* ((filename (filename "text3"))
         (fd (sb-posix:open filename sb-unix:o_rdonly))
         (stream (make-instance 'fd-input-stream :fd fd :buffer-size 10)))
    (unwind-protect
         (progn
           (is (schar "今" 0)
               (read-char stream))
           (is (schar "日" 0)
               (peek-char nil stream))
           (is (schar "日" 0)
               (peek-char nil stream))
           (is (schar "日" 0)
               (peek-char nil stream)))
      (close stream)
      (sb-posix:close fd))))

(defun unread-test ()
  (let* ((filename (filename "text3"))
         (fd (sb-posix:open filename sb-unix:o_rdonly))
         (stream (make-instance 'fd-input-stream :fd fd :buffer-size 10)))
    (unwind-protect
         (progn
           (unread-char (read-char stream) stream)
           (is (schar "今" 0) (peek-char nil stream))
           (is (schar "今" 0) (peek-char nil stream))
           (is (schar "今" 0) (read-char stream))
           (is (schar "日" 0) (read-char stream)))
      (close stream)
      (sb-posix:close fd))))

(is (read-file (filename "text"))
    '((NIL . "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
      (NIL . "abcdefghijklmnopqrstuvwxyz")
      (T . "1234567890")))

(is (read-file (filename "text2"))
    '((NIL . "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
      (NIL . "abcdefghijklmnopqrstuvwxyz")
      (NIL . "1234567890")
      (T . T)))

(peek-test)
(unread-test)

(is (write-file (unique-filename)
                '("abc")
                3)
    "abc")

(is (write-file (unique-filename)
                '("abcdefghijklm")
                3)
    "abcdefghijklm")

(is (write-file (unique-filename)
                (list "foo" (string #\newline) "bar" (string #\newline) "hogehoge"))
    "foo
bar
hogehoge")

(is (write-file (unique-filename)
                (list "あいうえお"))
    "あいうえお")

(cleanup-temp-files)

(finalize)
