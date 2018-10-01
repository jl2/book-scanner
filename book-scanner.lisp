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


(defun scan-from-webcam (&key (camera 0) (fps 10))
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (cv:with-captured-camera (vid :width 800 :height 600 :idx camera)
        (loop
           (let* ((frame (cv:query-frame vid)))
             (unwind-protect
                  (progn
                    (cv:show-image "bar-code-scanner" frame)
                    (let ((barcodes (zbar-utils:scan-cv-image frame))
                          (c (cv:wait-key (floor (/ 1000 fps)))))
                      (cond (barcodes
                             (format t "~a~%" barcodes))
                            (t
                               (cl-tesseract:with-base-api api
                                 (cl-tesseract:init-tess-api api "eng")
                                 (cl-tesseract:tessbaseapisetimage api frame  )
                                 (format t "~a~%" (tess:tessbaseapigetutf8text api))))))
                      (when (or (= c 27) (= c 1048603))
                        (format t "Exiting~%")
                        (return))))))))))
