;;; org-api.el --- High-level API to org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; org-api is a high-level API to org-mode, primarily leveraging org-ml.

;;; Code:

(require 'org-ml)

(defun org-api/get-section (node)
  "Get the top level section of a NODE."
  (car (org-ml-match '(section) node)))

(defun org-api/get-headlines (node)
  "Get a flat list of all first-level headlines in a NODE."
  (org-ml-match '(headline) node))

(defun org-api/get-nodes-in-section (section condition)
  "Get a list of nodes in SECTION matching CONDITION.
CONDITION is a pattern used with `org-ml-match' that must evaluate to true
for the node to be included in the list of nodes."
  (org-ml-match condition section))

(defun org-api/get-nodes-recursive-in-headline (headline nodes condition)
  "Get all NODES in HEADLINE recursively matching CONDITION.
CONDITION is a pattern used with `org-ml-match' that must evaluate to true
for the node to be included in the list of nodes."
  (let ((section (org-api/get-section headline))
        (headlines (org-api/get-headlines headline)))
    (setq nodes
          (append nodes (org-api/get-nodes-in-section section condition)))
    (dolist (headline headlines)
      (setq nodes
            (org-api/get-nodes-recursive-in-headline headline nodes condition)))
    nodes))

(defun org-api/get-nodes-recursive-in-current-buffer (condition)
  "Get a flat list of all nodes in the current buffer matching CONDITION.
The syntax of CONDITION is the same as that of `pattern' in `org-ml-match'."
  (let* ((buffer-tree (org-ml-parse-this-buffer))
         (leading-section (org-api/get-section buffer-tree))
         (nodes (org-api/get-nodes-in-section leading-section condition))
         (headlines (org-api/get-headlines buffer-tree)))
    (dolist (headline headlines)
      (setq nodes (org-api/get-nodes-recursive-in-headline headline nodes condition)))
    nodes))

(provide 'org-api)

;;; org-api.el ends here
