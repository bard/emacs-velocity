;;; DEPENDENCIES

(require 'velocity-api)
(require 'helm)

;;; COMMANDS

(defun velocity--helm ()
  (interactive)
  (helm :sources (list (velocity--helm-make-source-for-search "Results")
                       velocity--helm-create-source)
        :buffer "*helm-velocity*"
        :truncate-lines t))

;;; HELM SOURCE

(defun velocity--helm-make-source-for-search (search-name)
  (helm-build-sync-source
      search-name
    :requires-pattern 3
    :nohighlight t
    :candidate-number-limit 30
    :action 'velocity--helm-action-visit
    :candidates 'velocity--helm-candidates-search
    :persistent-action 'velocity--helm-persistent-action-visit
    :follow 1
    :match-dynamic t))

(defvar velocity--helm-create-source
  (helm-build-sync-source
      "Create"
    :requires-pattern 15
    :nohighlight t
    :volatile t
    :match-dynamic t
    :candidates 'velocity--helm-candidates-create
    :action 'velocity--helm-action-create))

;;; HELM CALLBACKS FOR SEARCH SOURCE

(defun velocity--helm-candidates-search ()
  (let* ((search-query helm-pattern)
         (first-n (lambda (n list) (-slice list 0 n))))
    (thread-last search-query
      (velocity--search velocity-searches)
      (-sort (lambda (r1 r2)
               (velocity--compare r1 r2 search-query)))
      (mapcar (lambda (result)
                (cons (concat (plist-get result :title)
                              " "
                              (plist-get result :body))
                      result))))))

(defun velocity--helm-persistent-action-visit (content-handle)
  (velocity--helm-action-visit content-handle)
  (helm-highlight-current-line))

(defun velocity--helm-action-visit (content-handle)
  (velocity--visit content-handle helm-pattern))

;;; HELM CALLBACKS FOR CREATE SOURCE

(defun velocity--helm-candidates-create ()
  (loop for target in (velocity--creation-candidates helm-pattern)
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

(defun velocity--helm-action-create (content-handle)
  (let* ((title (string-trim helm-pattern))
         (filename (plist-get content-handle :filename))
         (backend-id (plist-get content-handle :backend))
         (backend (plist-get velocity-backends backend-id))
         (visit-fn (plist-get backend :visit-fn))
         (create-fn (plist-get backend :create-fn)))

    (velocity--create (append content-handle
                              (list :title title
                                    :visit-fn visit-fn
                                    :create-fn create-fn)))))

;;; META

(provide 'velocity-frontend-helm)
