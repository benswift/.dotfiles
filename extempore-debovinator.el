;;; extempore-debovinator.el -*- lexical-binding: t -*-
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

(defun extempore-debovinator-map-c-type-to-xtlang-type (c-type ptr-depth)
  "currently assumes x86_64 architecture - and maps unsigned type to signed types (since xtlang has no unsigned types)"
  (let ((type-alist
         '(("void" . "i8") ;; for void pointers
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
    (concat (or (cdr-safe (assoc c-type type-alist))
                c-type)
            (make-string ptr-depth ?*))))

(defun extempore-debovinate-variable (name data pos &optional buffer)
  (cl-destructuring-bind (&key
                          type
                          pointer
                          dereference
                          default-value
                          constant-flag
                          &allow-other-keys) data
    ;; if it's just a preprocessor define used at compile-time, ignore it
    (unless (equal data '(:constant-flag t))
      (list (cons :name name)
            (cons :type
                  (cond ((car-safe type)
                         (concat
                          (extempore-debovinator-map-c-type-to-xtlang-type
                           (car-safe type)
                           (or pointer dereference 0))))
                        (type
                         (concat
                          (extempore-debovinator-map-c-type-to-xtlang-type
                           type
                           (or pointer dereference 0))))
                        (t "i32")))
            (cons :value
                  (let ((val (or
                              (and (stringp default-value) default-value)
                              (and default-value (listp default-value)
                                   (with-current-buffer buffer
                                     (apply #'buffer-substring-no-properties default-value)))
                              (and buffer pos
                                   (with-current-buffer buffer
                                     (goto-char pos)
                                     (when (search-forward-regexp "[ =]*\\(0x[0-9A-Fa-f]?+\\|[0-9]?+\\)" (point-at-eol) :noerror)
                                       (match-string-no-properties 1)))))))
                    (cond
                     ((string-empty-p val) nil)
                     ((ignore-errors (string= (substring val 0 2) "0x"))
                      (concat "#" (substring val 1)))
                     (val))))))))

(defun extempore-debovinator-insert-sys-load (path)
  (insert (format "(sys:load \"%s\")\n"
                  (concat (file-name-sans-extension path) ".xtm"))))

(defun extempore-debovinator-insert-bind-lib (libname name rettype args)
  (insert (format "(bind-lib %s %s [%s]* \n\"%s\")\n"
                  libname
                  name
                  (string-join (cons rettype
                                     (-map (lambda (x) (cdr (assoc :type x))) args)) ",")
                  (string-join (-map-indexed (lambda (i x) (format "@param %s - index %d" (cdr (assoc :name x)) i)) args) "\n"))))

(defun extempore-debovinator-insert-named-type (name members)
  (when members
    (insert (format "(bind-type %s <%s>\n\"%s\")\n"
                    name
                    (string-join (-map (lambda (x) (cdr (assoc :type x))) members) ",")
                    (string-join (-map-indexed (lambda (i x) (format "@member %s - index %d" (cdr (assoc :name x)) i)) members) "\n")))))

(defun extempore-debovinator-insert-alias (data)
  (insert (format "(bind-alias %s %s)\n"
                  (cdr (assoc :name data))
                  (cdr (assoc :type data)))))

(defun extempore-debovinator-insert-globalvar (data)
  (when data
    (insert (format "(bind-val %s %s%s)\n"
                    (cdr (assoc :name data))
                    (cdr (assoc :type data))
                    (if (cdr (assoc :value data))
                        (concat " " (cdr (assoc :value data)))
                      "")))))

(defvar extempore-debovinate-current-enum-value 0)
(defvar extempore-debovinate-current-enum-typedef "enum")

(defun extempore-debovinator-insert-enum-globalvar (data)
  (when (cdr (assoc :value data))
    (setf extempore-debovinate-current-enum-value
          (cdr (assoc :value data))))
  (insert (format "(bind-val %s %s %s)\n"
                  (cdr (assoc :name data))
                  extempore-debovinate-current-enum-typedef
                  extempore-debovinate-current-enum-value))
  (if (numberp extempore-debovinate-current-enum-value)
      (incf extempore-debovinate-current-enum-value)
    (message "warning: didn't increment enum out the value for %s"
             (cdr (assoc :name data)))))

(defun extempore-debovinator-dispatch (args libname buffer)
  (let ((name (nth 0 args))
        (class (nth 1 args))
        (data (nth 2 args))
        (bounds (nth 4 args)))
    (cl-destructuring-bind
        (&key
         arguments
         constant-flag
         default-value
         dereference
         members
         pointer
         system-flag
         type
         typedef
         typemodifiers
         &allow-other-keys)
        data
      (cond ((string-equal class "include")
             (unless system-flag
               (extempore-debovinator-insert-sys-load
                name)))
            ;; function/function prototype -> bind-func
            ((string-equal class "function")
             (unless (member "inline" typemodifiers)
               (extempore-debovinator-insert-bind-lib
                libname
                name
                (if (and (stringp type) (string= type "void")) "void"
                  (extempore-debovinator-map-c-type-to-xtlang-type
                   (or (car-safe type) type) (or pointer dereference 0)))
                ;; this here for the main(void) case
                (unless (and (= (length arguments) 1)
                             (member '(:type "void") (car arguments)))
                  (-map
                   (lambda (x)
                     (extempore-debovinate-variable
                      (car x)
                      (caddr x)
                      (elt (car (reverse x)) 1)
                      buffer))
                   arguments)))))
            ;; struct -> bind-type
            ((string-equal class "type")
             (cond ((string-equal type "struct")
                    (extempore-debovinator-insert-named-type
                     name
                     (-map
                      (lambda (x)
                        (extempore-debovinate-variable
                         (car x)
                         (caddr x)
                         (elt (car (reverse x)) 1)
                         buffer))
                      members)))
                   ;; enum -> bind-val
                   ((string-equal type "enum")
                    (unless (string-empty-p name)
                      (extempore-debovinator-insert-alias
                       (list
                        (cons :name name)
                        (cons :type (extempore-debovinator-map-c-type-to-xtlang-type type 0)))))
                    (setf
                     extempore-debovinate-current-enum-value
                     0)
                    (unless (string-equal
                             extempore-debovinate-current-enum-typedef
                             name)
                      (setf
                       extempore-debovinate-current-enum-typedef
                       "enum"))
                    (-map
                     (lambda (x)
                       (extempore-debovinator-insert-enum-globalvar
                        (extempore-debovinate-variable
                         (car x)
                         (caddr x)
                         (elt (car (reverse x)) 1)
                         buffer)))
                     members))
                   ;; typedef -> bind-alias
                   ((string-equal type "typedef")
                    (cond ((and (listp typedef)
                                (= (length typedef) 1))
                           ;; probably a typedef'ed enum or something
                           (extempore-debovinator-insert-alias
                            (list
                             (cons :name name)
                             (cons :type (extempore-debovinator-map-c-type-to-xtlang-type (car typedef) 0)))))
                          ((not (member
                                 '(:type "struct")
                                 typedef))
                           ;; probably a typedef'ed enum or something
                           (setf
                            extempore-debovinate-current-enum-typedef
                            name)
                           (extempore-debovinator-dispatch
                            (cons name (cdr typedef))
                            libname
                            buffer))))))
            ;; globalvar -> bind-val
            ((string-equal class "variable")
             (extempore-debovinator-insert-globalvar
              (extempore-debovinate-variable
               name
               data
               (and bounds (elt bounds 1))
               buffer)))))))

;;;###autoload
(defun extempore-debovinate-file (filename libname)
  (interactive
   (list (car (find-file-read-args "C File (.c/.h): " t))
         (read-string "libname : ")))
  (let ((c-buffer (find-file-noselect filename)))
    (with-current-buffer c-buffer
      (let ((data (reverse (semantic-parse-region (point-min) (point-max)))))
        (with-temp-buffer
          (insert (format ";; xtlang bindings automatically generated from %s\n;; by extempore-debovinator.el on %s\n\n"
                          filename
                          (format-time-string "%F")))
          (-each data (lambda (x) (extempore-debovinator-dispatch x libname c-buffer)))
          (write-file (concat (file-name-sans-extension filename) ".xtm"))))
      (kill-buffer c-buffer))))

(provide 'extempore-debovinator)
