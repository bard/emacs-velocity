
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPENDENCIES

(require 'velocity-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGISTRATION

(velocity-register-backend
 'markdown
 (list :filter-result-fn 'velocity-markdown-filter-result
       :next-section-fn 'velocity-markdown-next-section
       :create-fn 'velocity-markdown-create))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACKS

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

(defun velocity-markdown-next-section ()
  (if (= (point) (point-max))
      nil
    (goto-char (point-max))
    (cons 1 (point))))

;; (debug  (velocity--move-to-next-section-by-separator "^\\# "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META

(provide 'velocity-backend-markdown)
