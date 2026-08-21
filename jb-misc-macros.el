;;; jb-misc-macros.el --- Miscellaneous macros

;; Filename: jb-misc-macros.el
;; Description: Miscellaneous macros
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-06-05 01:38:24
;; Version: 0.4
;; Last-Updated: 2026-08-21 20:59:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/jb-misc-macros
;; Keywords: lisp
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((cl-lib "1.0"))
;;
;; Features that might be required by this library:
;;
;; cl-lib 
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1AoGev8FTwVVspNZxHuu8LMztAwxdzRndZ
;;
;; This library contains miscellaneous functions & macros that I use in my projects,
;; or that I thought might be useful.

;;; Functions & Macros:
;;
;; Below is a list of functions and macros defined in this file:
;;
;; `lastcar' : Return the last element of a list
;; `jb-get-matching-name-buffers' : Return list of buffers with names matching REGEX
;; `jb-get-matching-mode-buffers' : Return list of buffers with mode names matching REGEX
;; `jb-apply-partially' : Return a function that is a partial application of FUN to ARGS
;; `jb-untilnext' : Evaluate INITFORM followed by NEXTFORM repeatedly. Stop when one of them returns non-nil, and returning that value.
;; `jb-list-subset' : Return elements of LIST corresponding to INDICES.
;; `jb-number-list' : Return a sequential list of numbers from START to END.
;; `jb-read-key-menu' : Prompt the user for a key and return the results of evaluating the corresponding form in the list FORMS.
;; `build-symbol-and-value-bindings' : Return a list of ‘let’ binding pairs from ARG-SPECS, which binds variables to symbols & values of args.
;;

;;; Examples:
;;
;; Prompt the user for a free keybinding:
;; (jb-untilnext (read-key-sequence "Enter a key: ")
;;            (read-key-sequence "That key is already bound to a command. Try again: ")
;;            (lambda (x) (not (key-binding x))))

;; Keep a track of the number of prompts
;; (jb-untilnext (read-number "What is 1+1? Attempt 1: ")
;;            (prog1 (read-number  (concat "Wrong! Try again. Attempt " (number-to-string num) ": "))
;;              (setq num (1+ num)))
;;            (lambda (x) (= x 2))
;;            (num 1))

;;  `(let ,(build-symbol-and-value-bindings '((foo fsym fval) ('bar nil bval) (1 2 3)))
;;    fsym)
;; expands to:
;; (let ((fsym 'foo)
;;       (fval (if (boundp 'foo) foo nil))
;;       (argsym2 'bar)
;;       (argval2 (if (boundp 'bar) bar nil))
;;       (argsym3 nil)
;;       (argval3 '(1 2 3)))
;;   fsym)"



;;; Installation:
;;
;; Put jb-misc-macros.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'jb-misc-macros)

;;; Change log:
;;	
;; 2013/06/05
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; Lars Brinkhoof for macro-utils.el, Paul Graham for his book "On Lisp"
;;

;;; TODO
;;
;;

;;; Require
(require 'cl-lib)
;;(require 'combinators)

;;; Code:

(defsubst lastcar (lst)
  (car (last lst)))

(defun jb-get-matching-name-buffers (regex)
  "Return list of buffers with names matching REGEX."
  (cl-loop for buf in (buffer-list)
           for name = (buffer-name buf)
           if (string-match regex name)
           collect buf))

(defun jb-get-matching-mode-buffers (regex)
  "Return list of buffers with mode names matching REGEX."
  (cl-loop for buf in (buffer-list)
           for modename = (symbol-name (with-current-buffer buf major-mode))
           if (string-match regex modename)
           collect buf))

;; Note: the cut macro in combinators.el does a similar job to the following function,
;; but cut doesn't allow reordering the args.
(defun jb-apply-partially (fun &rest args)
  "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called.
ARGS may also contain the symbols 'first 'second 'third' 'fourth 'fifth
'sixth 'seventh 'eighth 'ninth & 'tenth, which will be replaced by the
corresponding args in the call to the new function. When the function is called
with more args than specified by ARGS the remaining args will be appended to
the specified ones.
For example (jb-apply-partially '/ 'first 3 'third) returns a function which divides
its first argument by three, then divides the result by its third argument, then
divides by its second argument. If there any more than 3 arguments the result will be
further divided by these remaining arguments."
  (cl-flet ((cl (x) (intern-soft (concat "cl-" (symbol-name x)))))
    (let* (used
	   (positions '(first second third fourth fifth sixth seventh eighth ninth tenth))
	   (fixedargs
	    (mapcar (lambda (x)
		      (let ((it x))
			(cl-case it
			  ((first second third fourth fifth sixth seventh eighth ninth tenth)
			   (push it used)
			   (list (cl it) 'args))
			  (t x))))
		    args))
	   (maxpos (if used
		       (1+ (apply 'max (mapcar (lambda (x) (cl-position x positions))
					       used)))
		     0))
	   (otherargs (if used
			  (mapcar (lambda (x) (list (cl x) 'args))
				  (cl-set-difference (cl-subseq positions 0 maxpos) used)))))
      `(lambda (&rest args) (apply ',fun ,@fixedargs ,@otherargs (cl-subseq args ,maxpos))))))

(defmacro jb-untilnext (initform nextform &optional testfunc &rest bindings)
  "Evaluate INITFORM followed by NEXTFORM repeatedly. Stop when one of them returns non-nil, and returning that value.
If TESTFUNC is supplied it should be a function that takes a single argument (the results of evaluating INITFORM or NEXTFORM),
and will be used as the stopping criterion. In this case evaluation will stop when TESTFUNC returns non-nil, but the
return value of the macro will still be the return value of INITFORM or NEXTFORM.
If BINDINGS are supplied then these will be placed in a let form wrapping the code, thus allowing for some persistence of state
between successive evaluations of NEXTFORM.
Note: you can set INITFORM to nil if you only want to evaluate a single form repeatedly."
  (cl-once-only (initform)
    (let ((retval (gensym)))
      `(let* (,@bindings ,retval)
	 (or (and ,testfunc
		  (or (and ,initform (funcall ,testfunc ,initform) ,initform)
		      (while (not (funcall ,testfunc (setq ,retval ,nextform))))
		      ,retval))
	     ,initform
	     (while (not (setq ,retval ,nextform)))
	     ,retval)))))

;; This might be better as an inline function.
(defmacro jb-list-subset (indices list)
  "Return elements of LIST corresponding to INDICES."
  `(mapcar (lambda (i) (nth i ,list)) ,indices))

(defun jb-number-list (start end &optional length)
  "Return a sequential list of numbers from START to END.
If END is nil and LENGTH is provided then return a list from START to (1- (+ START LENGTH))."
  (if end
      (cl-loop for i from start to end collect i)
    (cl-assert length)
    (cl-loop for i from start to (1- (+ start length)) collect i)))

;; This might be better as a function but I wanted to practice writing macros.
;; Also this way we can use gensyms to minimize the number of variables bound in the let
;; form surrounding the evaluation of FORMS.
(cl-defmacro jb-read-key-menu (prompts forms &optional startstr endstr keys)
  "Prompt the user for a key and return the results of evaluating the corresponding form in the list FORMS.
If the corresponding form is a symbol just return that symbol unevaluated.
If KEYS is supplied then it should be a list (of the same length as PROMPTS & FORMS) of keys to be prompted for.
Each element of KEYS should be a string or vector as returned by `read-key-sequence'. 
If the KEYS list is not long enough to cover all PROMPTS, or if there are nil values in the list then any missing
values will be replaced by unused keys starting with the \"1\" key.

The prompt string for `read-key-sequence' will be formed from PROMPTS, KEYS and the optional STARTSTR and ENDSTR in
the following way:

STARTSTR
K1) PROMPT1
K2) PROMPT2
...
KN) PROMPT3
ENDSTR

Where K1-KN are key descriptions of the keys in KEYS, and PROMPT1-PROMPTN are the corresponding prompt strings in the
list PROMPTS.

The macro arguments will be evaluated once before expanding the macro."
  (cl-with-gensyms (newprompts prompt prompts2 retval newkeys keystrs maxlen)
    `(let* ((,prompts2 ,prompts)
	    (,newkeys (let* ((origkeys ,keys)
			     (uniqkeys (remove 'nil origkeys))
			     (nextkey 48))
			(mapcar (lambda (key)
				  (or key (progn
					    (setq nextkey (1+ nextkey))
					    (while (member (char-to-string nextkey) uniqkeys)
					      (setq nextkey (1+ nextkey)))
					    (char-to-string nextkey))))
				(nconc origkeys
				       (make-list (max 0 (- (length ,prompts2) (length origkeys))) nil)))))
	    (,keystrs (mapcar 'key-description ,newkeys))
	    (,maxlen (cl-loop for keystr in ,keystrs maximize (length keystr)))
	    (,newprompts (mapcar* (lambda (k p)
				    (let ((len (- ,maxlen (length k))))
				      (concat k ") " (make-string len ? ) p)))
				  ,keystrs ,prompts2))
	    (,prompt (concat (and ,(eval startstr) (concat ,(eval startstr) "\n"))
			     (mapconcat 'identity ,newprompts "\n")
			     "\nC-g) Quit"
			     (and ,(eval endstr) (concat "\n" ,(eval endstr)))))
	    (,retval 'again))
       (while (eq ,retval 'again)
	 (let (key)
	   (while (not (or (member key ,newkeys) (equal key "")))
	     (setq key (read-key-sequence ,prompt nil t nil t)))
	   (if (equal key "")
	       (keyboard-quit)
	     (setq ,retval (nth (cl-position key ,newkeys :test 'equal) ,forms)))))
       (if (symbolp ,retval) ,retval (eval ,retval)))))

(defun build-symbol-and-value-bindings (arg-specs)
  "Return a list of `let' binding pairs from ARG-SPECS, which binds variables to symbols & values of args.

Each element of ARG-SPECS is either:

  FORM                     - symbol bound to argsymN, and value bound to argvalN
                             where N indicates position in ARG-SPECS (starting at 1)
  (FORM SYM-NAME)          - symbol bound to SYM-NAME, no value variable is bound
  (FORM nil VAL-NAME)      - value bound to VAL-NAME, no symbol variable is bound
  (FORM SYM-NAME VAL-NAME) - symbol bound to SYM-NAME, and value bound to VAL-NAME

FORM may be a quoted or unquoted symbol, a quoted or unquoted list, a number, a string, or any other expression.
SYM-NAME and VAL-NAME are symbols or nil. nil means omit that binding.
Unless FORM is a quoted or unquoted symbol, its corresponding symbol variable will be set to nil.

Example:
 `(let ,(build-symbol-and-value-bindings '((foo fsym fval) ('bar nil bval) (1 2 3)))
   fsym)

expands to:

\(let ((fsym 'foo)
      (fval (if (boundp 'foo) foo nil))
      (argsym2 'bar)
      (argval2 (if (boundp 'bar) bar nil))
      (argsym3 nil)
      (argval3 '(1 2 3)))
  fsym)"
  (cl-flet ((bindpair (s v) (list (list sname s) (list vname v)))
	    (bindsym (s) (list (list sname s)))
	    (bindval (v) (list (list vname v))))
    (cl-loop for spec in arg-specs
             for i from 1
	     for sname = (intern (format "argsym%d" i))
	     for vname = (intern (format "argval%d" i))
             nconc
             (pcase spec
               ((pred symbolp) ;; 1. Unquoted symbol
		(bindpair `',spec `(if (boundp ',spec) ,spec nil)))
               (`(quote ,(and (pred symbolp) s)) ; 2. Quoted symbol: 'zzz  →  (quote zzz)
		(bindpair `',s `(if (boundp ',s) ,s nil)))
               (`(quote ,s) ;; 3. Quoted list: '(1 2 3)  →  (quote (1 2 3))
		(bindpair nil spec))
               ((and (pred listp)	;; 4. Unquoted list whose car is NOT a symbol → plain list form
                     (guard (not (or (symbolp (car spec))
				     (and (listp (car spec))
					  (eq (caar spec) 'quote))))))
		(bindpair nil `(quote ,spec)))
               ((pred numberp) ;; 5. Number
		(bindpair nil spec))
               ((pred stringp) ;; 6. String
		(bindpair nil spec))
               (`(,form nil ,(and (pred symbolp) vname)) ;; 7. (FORM nil VAL-NAME) - no sym binding
		(bindval (pcase form
			   ((pred symbolp)         `(if (boundp ',form) ,form nil))
			   (`(quote ,(and (pred symbolp) s)) `(if (boundp ',s) ,s nil))
			   (`(quote ,s)           form)
			   ((pred listp)          `(quote ,form))
			   ((pred numberp)        form)
			   ((pred stringp)        form)
			   (_                     form))))
               (`(,form ,(and (pred symbolp) sname) ,(and (pred symbolp) vname)) ;; 8. (FORM SYM-NAME VAL-NAME)
		(bindpair (pcase form
			    ((pred symbolp)         `',form)
			    (`(quote ,(and (pred symbolp) s)) `',s)
			    (_ nil))
			  (pcase form
			    ((pred symbolp)        `(if (boundp ',form) ,form nil))
			    (`(quote ,(and (pred symbolp) s)) `(if (boundp ',s) ,s nil))
			    (`(quote ,s)           form)
			    ((pred listp)          `(quote ,form))
			    ((pred numberp)        form)
			    ((pred stringp)        form)
			    (_                     form))))
               (`(,form ,(and (pred symbolp) sname)) ;; 9. (FORM SYM-NAME)  — no val binding
		(bindsym (pcase form
			   ((pred symbolp)         `',form)
			   (`(quote ,(and (pred symbolp) s)) `',s)
			   (_ nil))))
               (_ (bindpair nil spec)))))) ;; 10. Anything unrecognized → pass through, nil sym



(provide 'jb-misc-macros)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "jb-misc-macros.el" (buffer-name) (buffer-string) "update")

;;; jb-misc-macros.el ends here
