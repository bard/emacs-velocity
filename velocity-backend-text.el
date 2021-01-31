;;; DEPENDENCIES

(require 'velocity-api)

;;; REGISTRATION

(velocity-register-backend
 'text-file
 (list :filter-result-fn 'velocity--text-filter-result
       :get-content-unit-fn 'velocity--text-get-content-unit/file
       :create-fn 'velocity--text-create))

;;; CALLBACKS

(defun velocity--text-create (title)
  (goto-char (point-min))
  (list :start-pos (point-min)
        :end-pos (point)))

(defun velocity--text-filter-result (result)
  (let* ((snippet-lines (split-string (plist-get result :snippet) "\n"))
         (snippet-title (car snippet-lines))
         (snippet-body (string-join (cdr snippet-lines) " ")))
    (thread-first result
      (plist-put :title snippet-title)
      (plist-put :body snippet-body))))

(defun velocity--text-get-content-unit/file (from-pos)
  (if (= from-pos (point-max))
      nil
    (cons 1 (point-max))))

;;; META

(provide 'velocity-backend-text)
