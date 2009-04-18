;;; regex-tool --- A regular expression evaluation tool for programmers

;; Copyright (C) 2007 John Wiegley
;; Copyright (C) 2009 Yann Hodique

;; Author: John Wiegley <johnw@newartisans.com>
;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Created: 29 Oct 2007
;; Modified: 18 Apr 2009
;; Version: 2.0
;; Keywords: regex languages programming development
;; X-URL: 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; After you type M-x regex-tool, you will be presented a buffer with 4 sections
;;
;; * A selection group for specific regexp implementation. Currently supports
;;   emacs and perl.
;;
;; * A selection group for specific regexp input. Currently supports raw string
;;   and `rx' expressions. Also an input field for the regexp.
;;
;; * A "text" group that contains the sample text you want to match against.
;;   Change this however you like.
;;
;; * A "groups" group that will list out any regular expression groups that match.
;;   Your regular expression is searched for as many times as it appears in the
;;   buffer, and any groups that match will be repeated.
;;
;; The results are updated automatically whenever you change something in the buffer.

;;; Version History:

;; 1.1 - Don't die horribly if the user simply types '^' or '$'
;; 1.2 - Include cl.el at compile time
;; 2.0 - Use a single buffer for everything

(eval-when-compile
  (require 'cl))

(require 'wid-edit)
(require 'ewoc)

(defgroup regex-tool nil
  "Regular expression tool."
  :tag "regex-tool"
  :group 'programming)

(defvar regex-tool-re nil)
(defvar regex-tool-text nil)
(defvar regex-tool-res nil)
(defvar regex-tool-backend nil)
(defvar regex-tool-frontend nil)

(defcustom regex-tool-default-regex ""
  "The default regexp to use."
  :type 'string
  :group 'regex-tool)

(defcustom regex-tool-default-text 
  "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.\nDonec hendrerit tempor tellus.\nDonec pretium posuere tellus.\nProin quam nisl, tincidunt et, mattis eget, convallis nec, purus.\nCum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.\nNulla posuere.\nDonec vitae dolor.\nNullam tristique diam non turpis.\nCras placerat accumsan nulla.\nNullam rutrum.\nNam vestibulum accumsan nisl."
  "The default text to use."
  :type 'string
  :group 'regex-tool)

(defface regex-tool-matched-face
  '((((background light)) (:foreground "Red" :bold t))
    (((background dark)) (:foreground "Orange" :bold t)))
  "Face to use to emphasize matching portions of the text."
  :group 'regex-tool)

(defcustom regex-tool-default-backend 'emacs
  "The backend used to process regular expressions.
The `emacs' backend handles regular expressions directly.
The `perl' backend talks to a perl subprocess to do the handling."
  :type '(choice
	  (const :tag "Emacs" emacs)
	  (const :tag "Perl" perl))
  :group 'regex-tool)

(defcustom regex-tool-default-frontend 'string
  ""
  :type '(choice
	  (const :tag "String" string)
	  (const :tag "Rx" rx))
  :group 'regex-tool)

(define-derived-mode regex-tool-mode text-mode "Regex Tool"
  "This is regex-tool mode.")

(defun regex-tool-render-perl (regex sample)
  (with-temp-buffer
    (insert (format "@lines = <DATA>;
$line = join(\"\", @lines);
print \"(\";
while ($line =~ m/%s/mg) {
  print \"(\", length($`), \" \", length($&), \" \";
  for $i (1 .. 20) {
    if ($$i) {
      print \"(\", $i, \" . \\\"\", $$i, \"\\\") \";
    }
  }
  print \")\";
}
print \")\";
__DATA__
%s" regex sample))
    (call-process-region (point-min) (point-max) "perl" t t)
    (goto-char (point-min))
    (read (current-buffer))))

(defun regex-tool ()
  (switch-to-buffer "*Regex Tool*")
  (kill-all-local-variables)
  (regex-tool-mode)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all)))
  (widget-insert (propertize "Regular Expression Tool." 'face 'bold) "\n")
  (setq regex-tool-backend
        (widget-create 'radio-button-choice
                       :value regex-tool-default-backend
                       :notify 'regex-tool-re-changed
                       '(item emacs) '(item perl)))
  (widget-insert "\n")

  (widget-insert (propertize "Regular Expression" 'face 'bold) "\n")
  (setq regex-tool-frontend
        (widget-create 'radio-button-choice
                       :value regex-tool-default-frontend
                       :notify 'regex-tool-re-changed
                       '(item string) '(item rx)))
  (widget-insert "\n")
  (setq regex-tool-re
        (widget-create 'editable-field
                       :notify 'regex-tool-re-changed
                       regex-tool-default-regex))
  (widget-insert "\n")

  (widget-insert (propertize "Text" 'face 'bold) "\n\n")
  (setq regex-tool-text
        (widget-create 'text
                       :notify 'regex-tool-re-changed 
                       regex-tool-default-text))
  (widget-insert "\n")

  (widget-insert (propertize "Groups" 'face 'bold) "\n")
  (setq regex-tool-res
        (ewoc-create 'regex-tool-pp))
  (use-local-map widget-keymap)
  (widget-setup)
  (regex-tool-markup-text)
  (goto-char
   (overlay-start
    (widget-get regex-tool-re :field-overlay))))

(defun regex-tool-pp (data)
  "pretty-printer for matched data"
  (dolist (d data) 
    (let ((n (car d))
          (str (cdr d)))
      (insert (format "Group %d: '%s'\n" n str)))))

(defun regex-tool-re-changed (widget &rest ignore)
  (regex-tool-markup-text))

(defun regex-tool-get-regex ()
  (let ((val (widget-value regex-tool-re))
        (frontend (widget-value regex-tool-frontend)))
    (cond ((eq frontend 'string)
           val)
          ((eq frontend 'rx)
           (condition-case nil
               (eval-expression (read (concat "(rx " val ")")))
             (error nil)))
          (t
           (error "Undefined regex-tool frontend")))))

(defun regex-tool-markup-text ()
  (interactive)
  (let ((backend (widget-value regex-tool-backend))
        previous-point) 
    (let* ((regex (regex-tool-get-regex))
           (overlay (widget-get regex-tool-text :field-overlay))
           (start (overlay-start overlay))
           (end (overlay-end overlay)))
      (remove-overlays start end 'face 'regex-tool-matched-face)
      (ewoc-filter regex-tool-res 'ignore)
      (when (> (length regex) 0)
        (save-excursion
          (ignore-errors
            (goto-char start)
            (cond ((eq backend 'emacs)
                   (while (and (setq previous-point (point))
                               (re-search-forward regex end t))
                     (if (= (point) previous-point)
                         (forward-char 1)
                       (overlay-put (make-overlay (match-beginning 0)
                                                  (match-end 0))
                                    'face 'regex-tool-matched-face)
                       (ewoc-enter-last regex-tool-res
                                        (loop for i to 10 if (match-string i) collect (cons i (match-string i)))))))
                  ((eq backend 'perl)
                   (let ((results (regex-tool-render-perl regex (buffer-substring start end))))
                     (dolist (result results)
                       (ewoc-enter-last regex-tool-res
                                        (let ((offset (+ start (nth 0 result) -1))
                                              (length (nth 1 result))
                                              (matches (nthcdr 2 result))
                                              res)
                                          (overlay-put (make-overlay (1+ offset) (+ offset length 1))
                                                       'face 'regex-tool-matched-face)
                                          (let ((match-zero (buffer-substring (1+ offset)
                                                                              (+ offset length 1))))
                                            (setq res (list (cons 0 match-zero))))
                                          (dolist (match matches)
                                            (setq res (append (list match) res)))
                                          (nreverse res))))))
                  (t
                   (error "Undefined regex-tool backend")))))))))

(provide 'regex-tool)
;;; regex-tool.el ends here
