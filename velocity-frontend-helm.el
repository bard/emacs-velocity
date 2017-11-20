;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPENDENCIES

(require 'velocity-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMANDS

(defun helm-velocity-1 ()
  (helm :sources (cons (helm-velocity--source-for-create)
                       (loop for (name . def) in velocity-searches
                             collect (helm-velocity--source-for-search name)))
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
    :persistent-action 'helm-velocity--persistent-action-visit
    :candidates '()
    :volatile t
    :filtered-candidate-transformer 'helm-velocity--candidates-search))

(defun helm-velocity--source-for-create ()
  (helm-build-sync-source
      "Create"
    :requires-pattern 12
    :nohighlight t
    :filtered-candidate-transformer 'helm-velocity--candidates-create
    :action 'helm-velocity--action-create))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM CALLBACKS FOR SEARCH SOURCE

(defun helm-velocity--candidates-search (candidates source-name)
  (let* ((search-query helm-pattern)
         (search-section-name (cdr (assoc 'name source-name)))
         (search-section-configs (cdr (assoc search-section-name
                                             velocity-searches)))
         (files (loop for config in search-section-configs
                      append (plist-get config :files)))
         (first-n (lambda (n list) (-slice list 0 n))))

    (thread-last files
      (velocity-search search-query)
      (-sort (lambda (r1 r2)
               (velocity-sort-results r1 r2 search-query)))
      (funcall first-n (helm-attr 'candidate-number-limit source-name))
      (mapcar (lambda (result)
                (cons (concat (plist-get result :title)
                              " "
                              (plist-get result :body))
                      result))))))

(defun helm-velocity--persistent-action-visit (search-result)
  (helm-velocity--action-visit search-result)
  (helm-highlight-current-line))

(defun helm-velocity--action-visit (search-result)
  (velocity-visit-result search-result helm-pattern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM CALLBACKS FOR CREATE SOURCE

(defun helm-velocity--candidates-create (candidates source)
  (loop for target in (velocity-creation-candidates helm-pattern)
        with highlight-face = '(:foreground "#ffa724" :weight bold)
        collect (let* ((target-path (plist-get target :filename))
                       (target-directory-name (file-name-base
                                               (directory-file-name
                                                (file-name-directory target-path))))
                       (target-base-name (file-name-nondirectory target-path)))
                  (cons (concat "…/" target-directory-name
                                "/" (propertize target-base-name
                                                'face highlight-face))
                        target))))

(defun helm-velocity--action-create (candidate)
  (let* ((title (string-trim helm-pattern))
         (filename (plist-get candidate :filename))
         (backend-id (plist-get candidate :backend))
         (backend (plist-get velocity-backends backend-id))
         (visit-fn (plist-get backend :visit-fn))
         (create-fn (plist-get backend :create-fn)))

    (velocity-create (append candidate
                             (list :title title
                                   :visit-fn visit-fn
                                   :create-fn create-fn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META

(provide 'velocity-frontend-helm)
