
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPENDENCIES

(require 'velocity-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGISTRATION

(velocity-register-backend
 'org
 (list :visit-fn 'velocity-org-visit
       :filter-result-fn 'velocity-org-filter-result
       :next-section-fn 'velocity-org-next-section))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACKS

(defun velocity-org-visit (candidate)
  (let* ((start-pos (plist-get candidate :start-pos))
         (end-pos (plist-get candidate :end-pos))
         (title (plist-get candidate :title))
         (file (plist-get candidate :filename))
         (buffer (velocity--make-indirect-buffer (or (get-file-buffer file)
                                                     (find-file-noselect file))
                                                 title)))
    (with-current-buffer buffer
      (goto-char start-pos)
      (org-show-subtree)
      (narrow-to-region start-pos end-pos))
    buffer))

(defun velocity-org-filter-result (basic-result)
  (let* ((snippet-lines (split-string (plist-get basic-result :snippet) "\n"))
         (snippet-title (velocity-org--prettify-title (car snippet-lines)))
         (snippet-body (velocity-org--prettify-body (string-join (cdr snippet-lines) " "))))
    (thread-first basic-result
      (plist-put :title snippet-title)
      (plist-put :body snippet-body))))

(defun velocity-org-next-section ()
  (velocity--move-to-next-section-by-separator "^\\* "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNALS

(defun velocity-org--prettify-title (title)
  (thread-first title
    velocity-org--strip-stars
    velocity-org--strip-tags
    velocity-org--remove-bracket-links
    (propertize 'face 'org-level-1)))

(defun velocity-org--prettify-body (body)
  (propertize (velocity-org--strip-properties body)
              'face 'shadow))

(defun velocity-org--remove-bracket-links (string)
  (if (string-match org-bracket-link-analytic-regexp string)
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward org-bracket-link-analytic-regexp nil t)
          (replace-match (match-string 5)))
        (buffer-string))
    string))

(defun velocity-org--strip-stars (title)
  (replace-regexp-in-string "^\\*+ "
                            ""
                            title))

(defun velocity-org--strip-tags (title)
  (replace-regexp-in-string "\\( *:[^ ]*:\\).*\\'"
                            ""
                            title nil nil 1))

(defun velocity-org--strip-properties (body)
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (when (re-search-forward ":PROPERTIES:" nil t)
      (let ((start (match-beginning 0)))
        (when (re-search-forward ":END:" nil t)
          (delete-region start (point)))))
    (string-trim (buffer-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META

(provide 'velocity-backend-org)
