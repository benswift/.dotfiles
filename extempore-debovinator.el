;;; extempore-debovinator.el --- Parse C files and generate xtlang
;; Author: Ben Swift <ben@benswift.me>
;; Keywords: Extempore

;; Copyright (c) 2011-2016, Andrew Sorensen

;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.

;; Neither the name of the authors nor other contributors may be used
;; to endorse or promote products derived from this software without
;; specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;    Parse C files using semantic (from CEDET) and generate xtlang
;;    binding code

;;; Code:

(require 'lisp-mode)
(require 'semantic)
(require 'dash)
(require 'cl-lib)

(defun extempore-parser-map-c-type-to-xtlang-type (c-type)
  "currently assumes x86_64 architecture - and maps unsigned type to signed types (since xtlang has no unsigned types)"
  (let ((type-alist '(("void" . "i8") ;; for void pointers
                      ("char" . "i8")
                      ("unsigned char" . "i8")
                      ("short" . "i16")
                      ("unsigned short" . "i16")
                      ("int" . "i32")
                      ("unsigned int" . "i32")
                      ("long" . "i32")
                      ("unsigned long" . "i32")
                      ("long long" . "i64")
                      ("unsigned long long" . "i64")
                      ("int8_t" . "i8")
                      ("uint8_t" . "i8")
                      ("int16_t" . "i16")
                      ("uint16_t" . "i16")
                      ("int32_t" . "i32")
                      ("uint32_t" . "i32")
                      ("int64_t" . "i64")
                      ("uint64_t" . "i64")
                      ;; ("float" . "float")
                      ;; ("double" . "double")
                      )))
    (or (cdr-safe (assoc c-type type-alist))
        c-type)))

(defun extempore-debovinate-variable (name data pos)
  (cl-destructuring-bind (&key
                          type
                          pointer
                          dereference
                          default-value
                          constant-flag
                          &allow-other-keys) data
    (list (cons :name name)
          (cons :type
                (if type
                    (concat
                     (extempore-parser-map-c-type-to-xtlang-type type)
                     (make-string (or pointer dereference 0) ?*))
                  "i32"))
          (cons :value
                (or
                 (and (stringp default-value) default-value)
                 (and default-value (listp default-value)
                      (apply #'buffer-substring-no-properties default-value))
                 (and pos
                      (save-excursion
                        (goto-char pos)
                        (search-forward-regexp "\w*\\([x0-9]+\\)")
                        (match-string-no-properties 1)))
                 "nomatch")))))

(defun extempore-debovinator-insert-closure (name rettype args)
  (format "%s\n(bind-func %s [%s]*)\n"
          (string-join (-map-indexed (lambda (i x) (format ";; %d %s" i (cdr (assoc :name x)))) args) "\n")
          name
          (string-join (cons rettype (-map (lambda (x) (cdr (assoc :type x))) args)) ",")))

(defun extempore-debovinator-insert-named-type (name members)
  (format "%s\n(bind-type %s <%s>)\n"
          (string-join (-map-indexed (lambda (i x) (format ";; %d %s" i (cdr (assoc :name x)))) members) "\n")
          name
          (string-join (-map (lambda (x) (cdr (assoc :type x))) members) ",")))

(defun extempore-debovinator-insert-alias (data)
  (format "(bind-alias %s %s)\n"
          (cdr (assoc :name data))
          (cdr (assoc :type data))))

(defun extempore-debovinator-insert-globalvar (data)
  (format "(bind-val %s %s %s)\n"
          (cdr (assoc :name data))
          (cdr (assoc :type data))
          (cdr (assoc :value data))))

(defun extempore-debovinator-dispatch (args)
  (cl-destructuring-bind (name class data nil bounds) args
    (cl-destructuring-bind (&key
                            type
                            typedef
                            pointer
                            members
                            arguments
                            dereference
                            default-value
                            constant-flag
                            &allow-other-keys) data
      (cond
       ;; function/function prototype -> bind-func
       ((string-equal class "function")
        (extempore-debovinator-insert-closure
         name type
         (-map (lambda (x)
                 (extempore-parser-debovinate-variable
                  (car x)
                  (caddr x)
                  (elt (car (reverse x)) 1)))
               arguments)))
       ;; struct -> bind-type
       ((string-equal class "type")
        (cond
         ((string-equal type "struct")
          (extempore-debovinator-insert-named-type
           name
           (-map (lambda (x)
                   (extempore-parser-debovinate-variable
                    (car x)
                    (caddr x)
                    (elt (car (reverse x)) 1)))
                 members)))
         ;; enum -> bind-val
         ((string-equal type "enum")
          (extempore-debovinator-insert-alias (list (cons :name name)
                                                    (cons :type type)))
          (extempore-debovinator-insert-globalvar (extempore-debovinate-variable name data (and bounds (elt bounds 1)))))
         ;; typedef -> bind-alias
         ((and (string-equal type "typedef")
               (not (member '(:type "struct") typedef)))
          ;; probably a typedef'ed enum or something
          (extempore-debovinator-dispatch (cons name (cdr typedef))))))
       ;; globalvar -> bind-val
       ((string-equal class "variable")
        (extempore-debovinator-insert-globalvar (extempore-debovinate-variable name data (and bounds (elt bounds 1)))))))))
