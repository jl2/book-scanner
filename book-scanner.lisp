;;;; book-scanner.lisp 
;;
;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :book-scanner)

(defmacro with-gui-thread (&body body)
  "Wraps BODY in code which masks float traps.
   This is needed in SBCL on OSX because native code often
   generate :inexact traps and land you in the debugger.
   For non SBCL this wraps body in a progn."
  `(trivial-main-thread:call-in-main-thread
    (lambda ()
      #+sbcl (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
               ,@body)
      #+ccl (unwind-protect (progn
                              (ccl:set-fpu-mode :invalid nil)
                              ,@body)
              (ccl:set-fpu-mode :invalid t))
      #-(or sbcl ccl)
      ,@body)))


(defun filter-for-ocr (img)
  "Apply a filter that enhances the lines in a bar code."
  (let ((filtered (cv:create-image (cv:get-size img) 8 1)))
    (cv:cvt-color img filtered cv:+rgb-2-gray+)
    (let (
          (kernel (cv:create-mat 5 5 cv:+32FC1+))
          (mat #2A(( 0.0 0.0 0.0 0.0 0.0)
                   ( 0.0 0.2 0.0 0.2 0.0)
                   ( 0.0 0.0 1.0 0.0 0.0)
                   ( 0.0 0.2 0.0 0.2 0.0)
                   ( 0.0 0.0 0.0 0.0 0.0))))
      (unwind-protect
           (progn
             (dotimes (i 5)
               (dotimes (j 5)
                 (cv:set-2d kernel i j (cv:scalar (aref mat j i)))))
             (cv:filter-2d filtered filtered kernel))
        (cv:free kernel))
      filtered)
    filtered))


(defun scan-from-cv-image (image)
  (when image
    (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
      (let* ((barcodes (zbar-utils:scan-cv-image image))
             (text (cl-tesseract:with-base-api api
                     (cl-tesseract:init-tess-api api "eng")
                     (cl-tesseract:tessbaseapisetimage api
                                                       (cv:ipl-data image)
                                                       (cv:ipl-width image)
                                                       (cv:ipl-height image)
                                                       3
                                                       (* 3 (cv:ipl-width image)))
                     (cl-tesseract:TessBaseAPIRecognize api (cffi:null-pointer))
                     (tess:tessbaseapigetutf8text api))))
        (format t "Barcodes: ~a~%UTF-8 Text: ~a~%" barcodes text)))))

(defun scan-filtered-from-cv-image (image)
  (when image
    (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
      (let* ((barcodes (zbar-utils:scan-cv-image image))
             (filtered (filter-for-ocr image))
             (text (cl-tesseract:with-base-api api
                     (cl-tesseract:init-tess-api api "eng")
                     (cl-tesseract:tessbaseapisetimage api
                                                       (cv:ipl-data filtered)
                                                       (cv:ipl-width filtered)
                                                       (cv:ipl-height filtered)
                                                       1
                                                       (* 1 (cv:ipl-width filtered)))
                     (cl-tesseract:TessBaseAPIRecognize api (cffi:null-pointer))
                     (tess:tessbaseapigetutf8text api))))
        (format t "Barcodes: ~a~%UTF-8 Text: ~a~%" barcodes text)
        (cv:release-image filtered)
        (cons barcodes text)))))

(defun scan-from-webcam (&key (camera 0) (fps 10))
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (cv:with-captured-camera (vid :width 800 :height 600 :idx camera)
        (loop
           (let* ((frame (cv:query-frame vid))
                  (c (cv:wait-key (floor (/ 1000 fps)))))
             (scan-from-cv-image frame)
             (cv:show-image "book-scanner" frame)
             (when (or (= c 27) (= c 1048603))
               (format t "Exiting~%")
               (return))))))))

(defun scan-from-file (file-name)
  (let* ((cvimage (cv:load-image file-name))
         (data (scan-from-cv-image cvimage)))
    (cv:release-image cvimage)
    data))

(defun scan-filtered-from-file (file-name)
  (let* ((cvimage (cv:load-image file-name))
         (data (scan-filtered-from-cv-image cvimage)))
    (cv:release-image cvimage)
    data))

(defun show-scan-from-file (file-name)
  (with-gui-thread
    (cv:with-named-window ("book-scanner")
      (let* ((image (cv:load-image file-name))
             (fps 30))
        
        (loop
           (let ((data (scan-filtered-from-cv-image image))
                 (filtered (filter-for-ocr image))
                 (c (cv:wait-key (floor (/ 1000 fps)))))
             (cv:show-image "bar-code-scanner" filtered)
             (format t "data: ~a~%" data)
             (when (or (= c 27) (= c 1048603))
               (format t "Exiting~%")
               (return data))))))))

(defun scan-directory (directory-name database-name)
  (let ((isbns (remove-duplicates
                (sort
                 (apply 
                  #'concatenate
                  'list
                  (mapcar #'zbar-utils:simple-scan (uiop:directory-files (uiop:ensure-directory-pathname directory-name) "*.jpg")))
                 #'string< :key #'cdr)
                :test #'string= :key #'cdr)))
  (bookdb:with-db (db database-name)
    (dolist (isbn isbns)
      (bookdb:add-book db (bookdb:lookup-isbn (cdr isbn)))))))
  

