;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPENDENCIES

(require 'velocity-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMANDS

(defun helm-velocity-1 ()
  (helm :sources (append (loop for (name . def) in velocity-searches
                               collect (helm-velocity--make-source-for-search name))
                         (list helm-source-velocity-create))
        :buffer "*helm-velocity*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM SOURCE

(defun helm-velocity--make-source-for-search (search-name)
  (helm-build-sync-source
      search-name
    :requires-pattern 3
    :nohighlight t
    :candidate-number-limit 15
    :action 'helm-velocity--action-visit
    :persistent-action 'helm-velocity--persistent-action-visit
    :candidates '()
    :volatile t
    :follow 1
    :filtered-candidate-transformer 'helm-velocity--candidates-search))

(defvar helm-source-velocity-create
  (helm-build-sync-source
      "Create"
    :requires-pattern 15
    :nohighlight t
    :volatile t
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
               (velocity-compare r1 r2 search-query)))
      (funcall first-n (helm-attr 'candidate-number-limit source-name))
      (mapcar (lambda (result)
                (cons (concat (plist-get result :title)
                              " "
                              (plist-get result :body))
                      result))))))

(defun helm-velocity--persistent-action-visit (content-handle)
  (helm-velocity--action-visit content-handle)
  (helm-highlight-current-line))

(defun helm-velocity--action-visit (content-handle)
  (velocity-visit content-handle helm-pattern))

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

(defun helm-velocity--action-create (content-handle)
  (let* ((title (string-trim helm-pattern))
         (filename (plist-get content-handle :filename))
         (backend-id (plist-get content-handle :backend))
         (backend (plist-get velocity-backends backend-id))
         (visit-fn (plist-get backend :visit-fn))
         (create-fn (plist-get backend :create-fn)))

    (velocity-create (append content-handle
                             (list :title title
                                   :visit-fn visit-fn
                                   :create-fn create-fn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META

(provide 'velocity-frontend-helm)
