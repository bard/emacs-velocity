;;; DEPENDENCIES

(require 'velocity-api)

;;; REGISTRATION

(velocity-register-backend
 'markdown-file
 (list :filter-result-fn 'velocity-markdown-filter-result
       :get-content-unit-fn 'velocity-markdown-get-content-unit/file
       :create-fn 'velocity-markdown-create))

(velocity-register-backend
 'markdown-heading-1
 (list :filter-result-fn 'velocity-markdown-filter-result
       :get-content-unit-fn 'velocity-markdown-get-content-unit/heading-1
       :create-fn 'velocity-markdown-create))

(velocity-register-backend
 'markdown-heading-2
 (list :filter-result-fn 'velocity-markdown-filter-result
       :get-content-unit-fn 'velocity-markdown-get-content-unit/heading-2
       :create-fn 'velocity-markdown-create))

;;; CALLBACKS

(defun velocity-markdown-create (title)
  (goto-char (point-min))
  (insert "# " title "\n\n")
  (list :start-pos (point-min)
        :end-pos (point)))

(defun velocity-markdown-filter-result (result)
  (let* ((snippet-lines (split-string (plist-get result :snippet) "\n"))
         (snippet-title (car snippet-lines))
         (snippet-body (string-join (cdr snippet-lines) " ")))
    (thread-first result
      (plist-put :title snippet-title)
      (plist-put :body snippet-body))))

(defun velocity-markdown-get-content-unit/file (from-pos)
  (if (= from-pos (point-max))
      nil
    (cons 1 (point-max))))

(defun velocity-markdown-get-content-unit/heading-1 (from-pos)
  (velocity--move-to-next-separator "^\\# " from-pos))

(defun velocity-markdown-get-content-unit/heading-2 (from-pos)
  (velocity--move-to-next-separator "^\\## " from-pos))

;;; META

(provide 'velocity-backend-markdown)
