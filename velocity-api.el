;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'names))

(define-namespace velocity-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC API

(defun search (search-configs search-query)
  (-stream-to-list
   (stream-concatenate
    (stream (loop for search-config in search-configs
                  with regexps = (-search-query-to-regexps search-query)
                  collect (let* ((backend-id (plist-get search-config :backend))
                                 (backend (plist-get velocity-backends backend-id))
                                 (fileset (plist-get search-config :files)))
                            (-result-stream (plist-get search-config :files)
                                            regexps
                                            (plist-get backend :next-section-fn)
                                            (plist-get backend :visit-fn)
                                            (or (plist-get backend :filter-result-fn)
                                                'identity))))))))

(defun visit (content-handle &optional search-query)
  (switch-to-buffer (-get-content-buffer content-handle))

  (when-let ((visit-fn (plist-get content-handle :visit-fn)))
    (funcall visit-fn))

  (when search-query
    (let ((split-pat (split-string search-query))
          (case-fold-search t))
      (re-search-forward (car split-pat) nil t))))

(defun create (content-handle)
  (let ((filename (plist-get content-handle :filename))
        (create-fn (plist-get content-handle :create-fn))
        (visit-fn (plist-get content-handle :visit-fn))
        (title (plist-get content-handle :title)))
    (visit (append content-handle
                   (with-current-buffer (-get-file-buffer filename)
                     (funcall create-fn title))))))

;; XXX accesses global
(defun creation-candidates (title)
  (loop for target-def in velocity-targets
        collect (let* ((target-pattern (plist-get target-def :file))
                       (target-path (format target-pattern title)))
                  (list :filename target-path
                        :backend (plist-get target-def :backend)))))

(defun compare (content-handle-1 content-handle-2 search-query)
  (let ((search-exprs (split-string search-query)))
    (> (-score-result content-handle-1 search-exprs)
       (-score-result content-handle-2 search-exprs))))

;; XXX accesses global
(defun register-backend (name callbacks)
  (setq velocity-backends
        (plist-put velocity-backends name
                   callbacks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNALS

(require 'stream)
(require 'dash)

(defun -result-stream (fileset
                       regexps
                       next-section-fn
                       visit-fn
                       filter-result-fn)
  (thread-last fileset
    (-mapcat #'file-expand-wildcards)
    (-map #'expand-file-name)
    (stream) ; make computation lazy just before expensive tasks
    (seq-map (lambda (filename)
               (list :filename filename
                     :buffer (or (get-file-buffer filename)
                                 (let ((temp-buffer (generate-new-buffer " *temp*")))
                                   (with-current-buffer temp-buffer
                                     (insert-file-contents filename))
                                   temp-buffer)))))
    (seq-map (lambda (content-handle-with-buffer)
               (-buffer-section-stream next-section-fn
                                       content-handle-with-buffer)))
    (stream-concatenate)
    (seq-filter (lambda (content-handle-with-section)
                  (-buffer-section-matches-regexps-p content-handle-with-section
                                                     regexps)))
    (seq-map (lambda (content-handle)
               (with-current-buffer (plist-get content-handle :buffer)
                 (let* ((start (plist-get content-handle :start-pos))
                        (end (plist-get content-handle :end-pos))
                        (snippet (buffer-substring-no-properties start
                                                                 (min (+ 300 start)
                                                                      end))))
                   (string-match "^\\(.*\\)\n\\([\0-\377[:nonascii:]]+\\)" snippet)
                   (append content-handle (list :snippet snippet
                                                :title (match-string 1 snippet)
                                                :body (match-string 2 snippet)))))))
    (seq-map (lambda (content-handle)
               (if visit-fn
                   (append content-handle (list :visit-fn visit-fn))
                 content-handle)))
    (seq-map (lambda (content-handle)
               (funcall filter-result-fn content-handle)))
    ))

(defun -stream-to-list (stream)
  "Eagerly traverse STREAM and return a list of its elements."
  (let (result)
    (seq-do (lambda (elt)
              (push elt result))
            stream)
    (reverse result)))

(defun -buffer-section-stream (next-section-fn data &optional pos)
  (stream-make
   (with-current-buffer (plist-get data :buffer)
     (save-excursion
       (let ((bounds (funcall next-section-fn (or pos 1))))
         (if bounds
             (cons (append data (list :start-pos (car bounds)
                                      :end-pos (cdr bounds)))

                   (-buffer-section-stream next-section-fn
                                           data
                                           (cdr bounds)))
           nil))))))

(defun -buffer-section-matches-regexps-p (section regexps)
  (let* ((buffer (plist-get section :buffer))
         (start (plist-get section :start-pos))
         (end (plist-get section :end-pos))
         (case-fold-search t))
    (with-current-buffer buffer
      (velocity--region-matches-all-p start end regexps))))

(defun -region-matches-all-p (start end regexps)
  (-all? (lambda (regexp)
           (save-excursion
             (goto-char start)
             (re-search-forward regexp end t)))
         regexps))

(defun -search-query-to-regexps (search-query)
  (let ((parts (split-string search-query " ")))
    (cons (concat "\\<" (car parts))
          (cdr parts))))

(defun -move-to-next-section-by-separator (section-separator from-pos)
  (goto-char from-pos)
  (when (re-search-forward section-separator nil t)
    (let ((section-start (match-beginning 0))
          (section-end (or (and (re-search-forward section-separator nil t )
                                (match-beginning 0))
                           (point-max))))
      (goto-char section-end)
      (cons section-start section-end))))

(defun -score-result (result search-exprs)
  (+ (-score-string (plist-get result :title) search-exprs)
     (-score-string (plist-get result :filename) search-exprs)))

(defun -score-string (string search-exprs)
  (let ((case-fold-search t))
    (loop for expr in search-exprs
          sum (+ (if (string-match expr string) 1 0)))))

(defun -lookup-prop (file property-name)
  (let* ((backend-id (plist-get (-search-config-for file)
                                :backend))
         (backend (plist-get velocity-backends backend-id)))
    (plist-get backend property-name)))

(defun -search-config-for (fileset)
  (let ((search-configs
         (loop for (name . configs) in velocity-searches
               append configs)))
    (-find (lambda (config)
             (-contains? (plist-get config :files) fileset))
           search-configs)))

(defun -get-content-buffer (content-handle)
  (let ((file-buffer (-get-file-buffer (plist-get content-handle :filename)))
        (start-pos (plist-get content-handle :start-pos))
        (end-pos (plist-get content-handle :end-pos))
        (title (plist-get content-handle :title)))
    (if (with-current-buffer file-buffer
          (and (eq start-pos (point-min))
               (eq end-pos (point-max))))
        file-buffer
      (with-current-buffer (-make-indirect-buffer file-buffer title)
        (narrow-to-region start-pos end-pos)
        (current-buffer)))))

(defun -get-file-buffer (filename)
  (or (get-file-buffer filename)
      (find-file-noselect filename)))

(defun -make-indirect-buffer (base-buffer name)
  "Create indirect buffer of `base-buffer' and name it `name'. If
buffer `name' exists already and is an indirect buffer of
`base-buffer', return it instead."
  (let* ((indirect-name (format "%s [%s]"
                                name
                                (file-name-base
                                 (buffer-file-name base-buffer))))
         (existing-indirect (get-buffer indirect-name)))
    (if (and existing-indirect
             (buffer-base-buffer existing-indirect)
             (equal (buffer-base-buffer existing-indirect) base-buffer))
        existing-indirect
      (make-indirect-buffer base-buffer indirect-name t))))

) ;; namespace velocity-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META

(provide 'velocity-api)
