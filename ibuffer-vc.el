;;; ibuffer-vc --- make ibuffer's buffer list aware of files' vc status

;; Copyright (C) 2011 Steve Purcell

;; Author: Steve Purcell <steve [at] sanityinc.com>
;; Keywords: themes
;; X-URL: http://github.com/purcell/ibuffer-vc
;; URL: http://github.com/purcell/ibuffer-vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Adds functionality to ibuffer for grouping buffers by their parent
;; vc root directory, and for displaying the vc status of listed
;; files.
;;
;;; Use:
;;
;; To group buffers by vc parent dir:
;;
;;   M-x ibuffer-vc-set-filter-groups-by-vc-root
;;
;; or, make this the default:
;;
;;   (add-hook 'ibuffer-mode-hook
;;     (lambda ()
;;       (ibuffer-vc-set-filter-groups-by-vc-root)
;;       (ibuffer-do-sort-by-alphabetic)))
;;
;; To include vc status info in the ibuffer list, add either
;; vc-status-mini or vc-status to `ibuffer-formats':
;;
;; (setq ibuffer-formats
;;       '((mark modified read-only vc-status-mini " "
;;               (name 18 18 :left :elide)
;;               " "
;;               (size 9 -1 :right)
;;               " "
;;               (mode 16 16 :left :elide)
;;               " "
;;               (vc-status 16 16 :left)
;;               " "
;;               filename-and-process)))
;;
;;; Code:

;; requires

(require 'ibuffer)
(require 'vc-hooks)

;;; Group and filter ibuffer entries by parent vc directory


;; TODO: get this info by querying the vc backend for each file
(defun ibuffer-vc--find-any-root (file-name)
  (let ((root nil))
    (loop for dir in '(".git" ".svn" "CVS" ".bzr" "_darcs")
          do (setq root (vc-find-root file-name dir))
          until root
          finally return root)))

(defun ibuffer-vc-root (buf)
  (let* ((file (buffer-local-value 'buffer-file-name buf))
         (dir (buffer-local-value 'default-directory buf))
         (ref (or file dir)))
    (ibuffer-vc--find-any-root ref)))

(define-ibuffer-filter vc-root
    "Toggle current view to buffers with vc root dir QUALIFIER."
  (:description "vc root dir"
                :reader (read-from-minibuffer "Filter by vc root dir (regexp): "))
  (ibuffer-awhen (ibuffer-vc-root buf)
    (string-match (expand-file-name qualifier) (expand-file-name it))))

(defun ibuffer-vc-generate-filter-groups-by-vc-root ()
  "Create a set of ibuffer filter groups based on the vc root dirs of buffers"
  (mapcar (lambda (vc-dir)
                  (cons (format "%s:%s" vc-dir) `((vc-root . ,vc-dir))))
                (ibuffer-remove-duplicates
                 (delq nil (mapcar 'ibuffer-vc-root (buffer-list))))))

;;;###autoload
(defun ibuffer-vc-set-filter-groups-by-vc-root ()
  "Set the current filter groups to filter by vc root dir."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
  (ibuffer-update nil t))


;;; Display vc status info in the ibuffer list

(define-ibuffer-column vc-status
  (:name "VC status" :inline t)
  (if buffer-file-name
      (let ((state (vc-state buffer-file-name)))
        (if state
          (symbol-name state)
          "-"))))

(define-ibuffer-column vc-status-mini
  (:name "V" :inline t)
  (if buffer-file-name
      (let ((state (vc-state buffer-file-name)))
        (cond
         ((eq 'added state) "A")
         ((eq 'removed state) "D")
         ((eq 'up-to-date state) "@")
         ((eq 'edited state) "*")
         ((eq 'needs-update state) "O")
         ((memq state '(conflict needs-merge unlocked-changes)) "!")
         ((eq 'ignored state) "I")
         ((memq state '(() unregistered missing)) "?")))
    " "))


(provide 'ibuffer-vc)
