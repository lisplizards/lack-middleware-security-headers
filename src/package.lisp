;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:lack/middleware/security-headers
  (:use #:cl)
  (:export #:*lack-middleware-security-headers*))
