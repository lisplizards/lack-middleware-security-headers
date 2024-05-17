;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/security-headers)

(defparameter *lack-middleware-security-headers*
  #'(lambda (app &key (x-frame-options "DENY")
                   (x-xss-protection "0")
                   (x-content-type-options "nosniff")
                   (x-permitted-cross-domain-policies "none")
                   (referrer-policy "strict-origin-when-cross-origin")
                   strict-transport-security
                   content-security-policy
                   permissions-policy
                   additional-headers)
      (declare (optimize (speed 0) (safety 3) (debug 3))
               (type function app))
      (let ((response-headers ()))
        (declare (type list response-headers))
        (let ((security-headers
                (list :x-frame-options x-frame-options
                      :x-xss-protection x-xss-protection
                      :x-content-type-options x-content-type-options
                      :x-permitted-cross-domain-policies x-permitted-cross-domain-policies
                      :referrer-policy referrer-policy
                      :strict-transport-security strict-transport-security
                      :content-security-policy content-security-policy
                      :permissions-policy permissions-policy)))
          (declare (type list security-headers))
          (loop for (key value) on security-headers by #'cddr
                when value
                  do (progn
                       (check-type value string)
                       (setf (getf response-headers key) value)))
          (loop for (key value) on additional-headers by #'cddr
                when value
                  do (progn
                       (check-type value string)
                       (assert (not (member key security-headers))
                               nil
                               "Security header cannot belong to 'additional-headers': ~A" key)
                       (setf (getf response-headers key) value))))
        #'(lambda (env)
            (declare (optimize (speed 3) (safety 0) (debug 0))
                     (type list env))
            (let* ((response (funcall app env))
                   (headers (second response)))
              (declare (type list response headers))
              (loop for (key value) of-type (keyword string) on response-headers by #'cddr
                    do (unless (getf headers key)
                         (setf (getf headers key)
                               value)))
              `(,(first response) ,headers ,(third response)))))))
