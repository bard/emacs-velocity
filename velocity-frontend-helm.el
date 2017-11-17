;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPENDENCIES

(require 'velocity-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMANDS

(defun helm-velocity-1 ()
  (helm :sources (loop for (name . def) in velocity-searches
                       collect (helm-velocity--source-for-search name))
        :buffer "*helm-velocity*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM SOURCE

(defun helm-velocity--source-for-search (search-name)
  (helm-build-sync-source
      search-name
    :requires-pattern 3
    :nohighlight t
    :candidate-number-limit 15
    :action 'helm-velocity--action-visit
    :persistent-action 'helm-velocity--persistent-action
    :candidates '()
    :volatile t
    :filtered-candidate-transformer 'helm-velocity--search))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM CALLBACKS

(defun helm-velocity--search (candidates source)
  (let* ((search-query helm-pattern)
         (search-section-name (cdr (assoc 'name source)))
         (search-section-configs (cdr (assoc search-section-name
                                             velocity-searches)))
         (files (loop for config in search-section-configs
                      append (plist-get config :files)))
         (first-n (lambda (n list) (-slice list 0 n))))

    (thread-last files
      (velocity-search search-query)
      (-sort (lambda (r1 r2)
               (velocity-sort-results r1 r2 search-query)))
      (funcall first-n (helm-attr 'candidate-number-limit source))
      (mapcar (lambda (result)
                (cons (concat (plist-get result :title)
                              " "
                              (plist-get result :body))
                      result))))))

(defun helm-velocity--persistent-action (search-result)
  (helm-velocity--action-visit search-result)
  (helm-highlight-current-line))

(defun helm-velocity--action-visit (search-result)
  (velocity-visit-result search-result helm-pattern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META

(provide 'velocity-frontend-helm)