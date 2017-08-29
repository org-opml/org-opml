;; opml2org.el


(defun parse-opml-from-file (filepath)
  "Parses the OPML file and returns the complete document as an
s-expression to be used as the root XML node in other functions."
  (with-temp-buffer
    (insert-file-contents filepath)
    (xml-parse-region (point-min) (point-max))))


(defun get-version (root)
  ""
  (caar (cdr (car root))))


(defun get-content (root)
  "Returns the OPML content as an s-expression (everything except
the root `<opml>' tag and `version' attribute)."
  (let ((content (cddr (car root))))
    (car content)))


(defun get-header (root tag &optional export-tag)
  "Get a header element from the OPML root and return an org-mode
header line. If the header element is not present, returns nil.

`tag' and `export-tag' must be *symbols*, not strings."
  (let* ((export-tag (or export-tag tag))
         (headers (car (cdddr (car root))))
         (node (assq tag headers))
         (tag-content (cddr node)))
    (when node
      (format "#+%s: %s" (symbol-name export-tag) (car tag-content)))))


(defun get-body (root)
  ""
  (car (cdr (cddddr (car root)))))


(defun printf (s vals)
  "For development only."
  (print (format s vals)))


(defun format-as-org (node)
  (when node
    (let* ((attrs (xml-node-attributes node))
           (text (cdr (assq 'text attrs)))
           (structure (cdr (assq 'structure attrs))))
      (cond
       ((null text)
        (error (concat "Missing text attribute: " node)))

       ((equal structure "paragraph")
        (printf "%s\n" text))

       ((equal structure "headline")
        ;; FIXME: headline-depth
        (printf "* %s" text))

       ((equal structure "list")
        ;; FIXME: list-depth
        (printf "- %s" text))

       (t
        (printf "Unknown! Structure: %s | Text: %s." structure text))))))


(defun process-body (ls &optional data)
  (let ((data (or data nil))
        (elem (car ls))
        (rest (cdr ls)))
    (cond
     ((null rest) nil)

     ((listp elem)
      (progn
        (format-as-org elem)
        (process-body (xml-node-children elem))
        (process-body rest)))

     (t
      (progn
        (process-body rest))))))

;; dev stuff :

(defun test ()
  (let ((body (get-body (parse-opml-from-file "examples/attributes.opml"))))
    (process-body body)))
