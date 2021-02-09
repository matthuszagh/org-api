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

(defun org-api/get-matching-nodes-recursive-in-node (node nodes condition)
  "Get a flat list of nodes matching CONDITION in NODE.
NODES is a list of existing matched nodes."
  (if (org-ml-is-childless node)
      (append nodes (org-ml-match condition node))
    (let ((children (org-ml-get-children node)))
      (setq nodes
            (append nodes (org-ml-match condition node)))
      (dolist (child children)
        (setq nodes
              (org-api/get-matching-nodes-recursive-in-node child nodes condition)))
      nodes)))

(defun org-api/get-nodes-recursive-in-current-buffer (condition)
  "Get a flat list of all nodes in the current buffer matching CONDITION.
The syntax of CONDITION is the same as that of `pattern' in `org-ml-match'."
  (let* ((buffer-tree (org-ml-parse-this-buffer))
         (nodes '()))
    ;; TODO I'm not actually sure it's possible to have a childless
    ;; root node.
    (if (org-ml-is-childless buffer-tree)
        (setq nodes
              (append nodes (org-api/get-nodes-in-leaf buffer-tree condition)))
      (setq nodes (org-api/get-matching-nodes-recursive-in-node buffer-tree nodes condition)))
    nodes))

(defun org-api/map-nodes-recursive-in-current-buffer (fun condition)
  "Apply FUN to each node matching CONDITION.
FUNCTION is a function taking a single node."
  (let* ((nodes (org-api/get-nodes-recursive-in-current-buffer condition))
         (number-of-nodes (length nodes))
         (i 0))
    (while (< i number-of-nodes)
      (funcall fun (nth i nodes))
      ;; reset nodes, which may have changed
      (setq nodes (org-api/get-nodes-recursive-in-current-buffer condition))
      (setq new-number-of-nodes (length nodes))
      ;; increment `i', but also decrement it if we deleted node.
      (setq i (+ 1 (- i (- number-of-nodes new-number-of-nodes))))
      (setq number-of-nodes new-number-of-nodes))))

(defun org-api/delete-node (node)
  "Delete NODE.
This deletes the node's entire contents from `:begin' to `:end'."
  (let ((begin (org-ml-get-property :begin node))
        (end (org-ml-get-property :end node)))
    (goto-char begin)
    (delete-region begin end)))

(provide 'org-api)

;;; org-api.el ends here
