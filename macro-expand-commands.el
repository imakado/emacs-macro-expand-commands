;;; macro-expand-commands.el - usefull commands for writting macro

;; Copyright (C) 2010   IMAKADO

;; Author: IMAKADO <ken.imakado@gmail.com>
;; Keywords: emacs-lisp
;; Prefix: macro-expand-commands-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Note
;; this package maight load "cl.el" (when called `macro-expand-commands-cl-macroexpand')

;;; Commentary:
;; Call comannd at the beggining of sexp.

;; sample config:
;; (require 'macro-expand-commands)
;; (define-key emacs-lisp-mode-map (kbd "C-M-S-e") 'macro-expand-cl-sexp)
;; (define-key emacs-lisp-mode-map (kbd "M-E") 'macro-expand-sexp)
;; (define-key emacs-lisp-mode-map (kbd "C-M-S-b") 'macro-expand-byte-code)

;; Thanks to kitokitoki for `view-mode' support patch. (on github)

(eval-when-compile
  (require 'cl))
(require 'rx)
(require 'pp)

;;;; Customization
(defgroup macro-expand-commands nil
  "usefull commands for writting macro."
  :group 'convenience
  :prefix "macro-expand-commands-")
(defcustom macro-expand-commands-output-function
  (lambda (output-buffer)
    (pop-to-buffer output-buffer))
  "output function"
  :type '(choice (const :tag "Pop To Buffer"
                        (lambda (output-buffer)
                          (pop-to-buffer output-buffer)))
                 (const :tag "Switch To Buffer"
                        (lambda (output-buffer)
                          (switch-to-buffer output-buffer)))
                 function)
  :group 'macro-expand-commands)

(defcustom macro-expand-commands-setup-output-buffer-function
  (lambda (outbuf)
    (with-current-buffer outbuf
      (let ((view-no-disable-on-exit t))
        (view-mode)
        (setq view-exit-action
              (lambda (buffer)
                (kill-buffer buffer)
                (ignore-errors (delete-window)))))))
  "setup function"
  :group 'macro-expand-commands
  :type '(choice (const :tag "View Mode"
                        (lambda (outbuf)
                          (with-current-buffer outbuf
                            (let ((view-no-disable-on-exit t))
                              (view-mode)
                              (setq view-exit-action
                                    (lambda (buffer)
                                      (kill-buffer buffer)
                                      (ignore-errors (delete-window))))))))
                 (const :tag "Just View"
                        'identity)
                 function))

(defvar macro-expand-commands-output-buffer-name "*MacroExpand output*")


(defvar macro-expand-commands-dont-newline-symbols
  '( let let* quote while fset list lambda memq member member* car cdr
         or prog1 not expect mapcar catch))

(defun macro-expand-commands-cl-macroexpand (form &optional full)
  (unless (fboundp 'cl-macroexpand-all)
    (require 'cl))
  (let ((cl-macroexpand-cmacs full)
        (cl-compiling-file full)
        (byte-compile-macro-environment nil))
    (let ((ret-form (and (fboundp 'cl-macroexpand-all)
                         (cl-macroexpand-all
                          form
                          (and (not full) '((block) (eval-when)))))))
      (insert (format "%S" ret-form)))))

(defmacro macro-expand-commands-cl-prettyexpand-string (macro)
  (declare (debug (symbolp)))
  `(progn
     (with-temp-buffer
       (macro-expand-commands-cl-macroexpand (quote ,macro))
       (buffer-string))))

(defmacro macro-expand-commands-cl-prettyexpand-all-string (macro)
  `(progn
     (with-temp-buffer
       (macro-expand-commands-cl-macroexpand (quote ,macro) t)
       (buffer-string))))

(defmacro macro-expand-commands-macroexpand-string (macro)
  `(progn
     (format "%S" (macroexpand (quote ,macro)))))

(defmacro macro-expand-commands-macroexpand-all-string (macro)
  `(progn
     (format "%S" (macroexpand-all (quote ,macro)))))

(defmacro macro-expand-commands-compiled-byte-code-string (form)
  `(progn
     (format "%S" (byte-compile (lambda () ,form)))))

(defun* macro-expand-commands-aux (pp-macro-name &key (pp t))
  (let ((sexp (thing-at-point 'sexp)))
    (let ((ret (eval (car (read-from-string
                           (format "(%s  %s)" pp-macro-name sexp))))))
      (let ((outbuf (get-buffer-create macro-expand-commands-output-buffer-name)))
        (with-current-buffer outbuf
          (erase-buffer)
          (emacs-lisp-mode)
          (insert (format "%s" ret))
          ;; remove newline at bob and eob
          ;; replace newline to \\n (this makes fontify and indentation broken)
          ;; beautify buffer
          (when pp
            (macro-expand-commands-pp-output-buffer (current-buffer)))
          (funcall macro-expand-commands-output-function outbuf)
          (funcall macro-expand-commands-setup-output-buffer-function
                   outbuf))))))

(defun macro-expand-commands-pp-output-buffer (buf)
  (with-current-buffer buf
    (flet ((trim (re &optional (newstring "") (pred 'identity))
                 (save-excursion
                   (loop initially (goto-char (point-min))
                         while (re-search-forward re nil t)
                         when (funcall pred (point))
                         do (typecase newstring
                              (string (replace-match newstring))
                              (function (funcall newstring)))))))
      (trim (rx buffer-start (+ (any space "\n"))))
      (trim (rx (+ (any space "\n")) buffer-end))
      (trim "\n" "\\\\n" (lambda (ignore)
                           (let ((in-string?
                                  (nth 3 (save-excursion
                                           (parse-partial-sexp (point-min) (point))))))
                             in-string?)))
      (pp-buffer)
      (loop for sym in macro-expand-commands-dont-newline-symbols
            for sym-str = (symbol-name sym)
            for re = (rx-to-string
                      `(seq symbol-start ,sym-str symbol-end "\n" (* space)))
            do (trim re (concat sym-str " ")))
      (indent-region (point-min) (point-max)))))

(defun macro-expand-sexp (&optional prefix)
  (interactive "P")
  (if prefix
      (macro-expand-commands-aux "macro-expand-commands-macroexpand-all-string")
    (macro-expand-commands-aux "macro-expand-commands-macroexpand-string")))

(defun macro-expand-all-sexp ()
  (interactive)
  (macro-expand-commands-aux "macro-expand-commands-macroexpand-all-string"))

(defun macro-expand-cl-sexp (&optional prefix)
  (interactive "P")
  (if prefix
      (macro-expand-commands-aux "macro-expand-commands-cl-prettyexpand-all-string")
    (macro-expand-commands-aux "macro-expand-commands-cl-prettyexpand-string")))

(defun macro-expand-cl-all-sexp ()
  (interactive)
  (macro-expand-commands-aux "macro-expand-commands-cl-prettyexpand-all-string"))

(defun macro-expand-byte-code (&optional prefix)
  (interactive "P")
  (macro-expand-commands-aux "macro-expand-commands-compiled-byte-code-string"
                             :pp nil))

(provide 'macro-expand-commands)
;; macro-expand-commands.el ends here.
