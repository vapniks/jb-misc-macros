;;; jb-misc-macros.el --- Miscellaneous macros

;; Filename: jb-misc-macros.el
;; Description: Miscellaneous macros
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-06-05 01:38:24
;; Version: 0.1
;; Last-Updated: 2013-06-05 01:38:24
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/jb-misc-macros
;; Keywords: lisp
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((macro-utils "1.0"))
;;
;; Features that might be required by this library:
;;
;; macro-utils.el
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
;; Bitcoin donations gratefully accepted: 
;;
;; How to create documentation and distribute package:
;;
;;     1) Remember to add ;;;###autoload magic cookies if possible
;;     2) Generate a bitcoin address for donations with shell command: bitcoin getaccountaddress jb-misc-macros
;;        and place address after "Commentary:" above.
;;     3) Use org-readme-top-header-to-readme to create initial Readme.org file.
;;     4) Use M-x auto-document to insert descriptions of commands and documents
;;     5) Create documentation in the Readme.org file:
;;        - Use org-mode features for structuring the data.
;;        - Divide the commands into different categories and create headings
;;          containing org lists of the commands in each category.
;;        - Create headings with any other extra information if needed (e.g. customization).
;;     6) In this buffer use org-readme-to-commentary to fill Commentary section with
;;        documentation from Readme.org file.
;;     7) Make any necessary adjustments to the documentation in this file (e.g. remove the installation
;;        and customization sections added in previous step since these should already be present).
;;     8) Use org-readme-marmalade-post and org-readme-convert-to-emacswiki to post
;;        the library on Marmalade and EmacsWiki respectively.
;; 
;;;;


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

;;; Customize:
;;
;; To automatically insert descriptions of customizable variables defined in this buffer
;; place point at the beginning of the next line and do: M-x auto-document

;;
;; All of the above can customized by:
;;      M-x customize-group RET jb-misc-macros RET
;;

;;; Change log:
;;	
;; 2013/06/05
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;;; Require
(require 'macro-utils)

;;; Code:
;; put macros in gist on github, or in their own package (if there are many of them)
(defmacro untilnext (initform nextform &optional test &rest bindings)
  "Do init1, then test, if test passes then return init1, otherwise do init2 until test passes.
Return the results of either init1 or init2."
  (let ((sym1 (gensym))
        (retval (gensym)))
    `(let* (,@bindings ,retval
            (,sym1 ,initform))
       (or (and ,test
                (or (and (funcall ,test ,sym1) ,sym1)
                    (while (not (funcall ,test (setq ,retval ,nextform))))
                    ,retval))
           ,sym1
           (and 
            (while (not (setq ,retval ,nextform)))
            ,retval)))))


(provide 'jb-misc-macros)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "jb-misc-macros.el" (buffer-name) (buffer-string) "update")

;;; jb-misc-macros.el ends here
