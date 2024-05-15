;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/security-headers/tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun hello-world-app (env)
  (declare (ignore env))
  `(200
    (:content-type "text/plain"
     :content-length 13
     :x-baaz "Baaz")
    ("Hello, World.")))

(defun app-with-security-headers (env)
  (declare (ignore env))
  `(200
    (:content-type "text/plain"
     :content-length 13
     :x-frame-options "SAMEORIGIN"
     :x-baar "Baar"
     :x-baaz "Baaz")
    ("Hello, World.")))

(deftest middleware-defaults
  (testing "adds default security headers to the response"
           (let ((app (funcall lack/middleware/security-headers:*lack-middleware-security-headers*
                               #'hello-world-app)))
             (let* ((response (funcall app ()))
                    (response-headers (second response)))
               (ok (= 200 (first response)))
               (ok (equalp '("Hello, World.") (third response)))
               (ok (equal "Baaz" (getf response-headers :x-baaz)))
               (ok (equal "DENY" (getf response-headers :x-frame-options)))
               (ok (equal "0" (getf response-headers :x-xss-protection)))
               (ok (equal "nosniff" (getf response-headers :x-content-type-options)))
               (ok (equal "none" (getf response-headers :x-permitted-cross-domain-policies)))
               (ok (equal "strict-origin-when-cross-origin" (getf response-headers :referrer-policy)))
               (ok (null (getf response-headers :strict-transport-security)))
               (ok (null (getf response-headers :content-security-policy)))
               (ok (null (getf response-headers :permissions-policy)))))))

(deftest middleware-options
    (testing "adds or removes user-specified security header values to the response, overriding defaults"
           (let ((app (funcall lack/middleware/security-headers:*lack-middleware-security-headers*
                               #'hello-world-app
                               :x-frame-options "SAMEORIGIN"
                               :x-xss-protection "1; mode=block"
                               :x-content-type-options nil
                               :permissions-policy "document-domain=(), encrypted-media=(), usb=()"
                               :content-security-policy "default-src 'self'")))
             (let* ((response (funcall app ()))
                    (response-headers (second response)))
               (ok (= 200 (first response)))
               (ok (equalp '("Hello, World.") (third response)))
               (ok (equal "Baaz" (getf response-headers :x-baaz)))
               (ok (equal "SAMEORIGIN" (getf response-headers :x-frame-options)))
               (ok (equal "1; mode=block" (getf response-headers :x-xss-protection)))
               (ok (null (getf response-headers :x-content-type-options)))
               (ok (equal "none" (getf response-headers :x-permitted-cross-domain-policies)))
               (ok (equal "strict-origin-when-cross-origin" (getf response-headers :referrer-policy)))
               (ok (null (getf response-headers :strict-transport-security)))
               (ok (equal "default-src 'self'" (getf response-headers :content-security-policy)))
               (ok (equal "document-domain=(), encrypted-media=(), usb=()"
                          (getf response-headers :permissions-policy))))))

  (testing "adds additional headers to the response"
           (let ((app (funcall lack/middleware/security-headers:*lack-middleware-security-headers*
                               #'hello-world-app
                               :x-frame-options "SAMEORIGIN"
                               :x-xss-protection "1; mode=block"
                               :x-content-type-options nil
                               :permissions-policy "document-domain=(), encrypted-media=(), usb=()"
                               :additional-headers '(:x-foo "Foo"
                                                     :x-bar "Bar"
                                                     :x-baar nil
                                                     :quux "Quux"))))
             (let* ((response (funcall app ()))
                    (response-headers (second response)))
               (ok (= 200 (first response)))
               (ok (equalp '("Hello, World.") (third response)))
               (ok (equal "Baaz" (getf response-headers :x-baaz)))
               (ok (equal "Foo" (getf response-headers :x-foo)))
               (ok (equal "Bar" (getf response-headers :x-bar)))
               (ok (null (getf response-headers :x-baar)))
               (ok (equal "Quux" (getf response-headers :quux))))))

  (testing "does not allow setting security-headers from 'additional-headers'"
           (ok (signals
                (funcall lack/middleware/security-headers:*lack-middleware-security-headers*
                         #'hello-world-app
                         :additional-headers '(:x-frame-options "SAMEORIGIN"))
                'simple-error))
           (ok (signals
                (funcall lack/middleware/security-headers:*lack-middleware-security-headers*
                         #'hello-world-app
                         :additional-headers '(:content-security-policy "default-src 'self'"))
                'simple-error)))

  (testing "does not override security headers already present in the the application environment"
           (let ((app (funcall lack/middleware/security-headers:*lack-middleware-security-headers*
                               #'app-with-security-headers
                               :x-frame-options "DENY"
                               :permissions-policy "document-domain=(), encrypted-media=(), usb=()")))
             (let* ((response (funcall app ()))
                    (response-headers (second response)))
               (ok (= 200 (first response)))
               (ok (equalp '("Hello, World.") (third response)))
               (ok (equal "Baaz" (getf response-headers :x-baaz)))
               (ok (equal "SAMEORIGIN" (getf response-headers :x-frame-options)))
               (ok (equal "document-domain=(), encrypted-media=(), usb=()"
                          (getf response-headers :permissions-policy))))))

  (testing "does not override additional headers already present in the application environment"
           (let ((app (funcall lack/middleware/security-headers:*lack-middleware-security-headers*
                               #'app-with-security-headers
                               :additional-headers '(:x-foo "Foo"
                                                     :x-baar nil
                                                     :x-baaz "Quux"))))
             (let* ((response (funcall app ()))
                    (response-headers (second response)))
               (ok (= 200 (first response)))
               (ok (equalp '("Hello, World.") (third response)))
               (ok (equal "Foo" (getf response-headers :x-foo)))
               (ok (equal "Baar" (getf response-headers :x-baar)))
               (ok (equal "Baaz" (getf response-headers :x-baaz)))))))
