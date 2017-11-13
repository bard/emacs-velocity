;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'names))

(define-namespace velocity-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC API

(defun search (search-query file-specs)
  (-stream-to-list
   (-search-stream file-specs search-query)))

(defun visit-result (search-result &optional search-query)
  (let ((visit-fn (or (-lookup-prop (plist-get search-result :filename)
                                    :visit-fn)
                      '-generic-visit-result)))

    (switch-to-buffer (funcall visit-fn search-result))

    (when search-query
      (let ((split-pat (split-string search-query))
            (case-fold-search t))

        (re-search-forward (car split-pat) nil t)))))

(defun sort-results (res1 res2 search-query)
  (let ((search-exprs (split-string search-query)))
    (> (-score-result res1 search-exprs)
       (-score-result res2 search-exprs))))

(defun register-backend (name callbacks)
  (setq velocity-backends
        (plist-put velocity-backends name
                   callbacks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNALS

(require 'stream)
(require 'dash)

(defun -search-stream (file-specs search-query)
  (let ((per-file-spec-result-streams
         (loop with regexps = (velocity--search-query-to-regexps search-query)
               for file-spec in file-specs
               collect (-search-result-stream (-backend-for-file-spec file-spec)
                                              file-spec regexps))))
    (stream-concatenate
     (stream per-file-spec-result-streams))))

(defun -search-result-stream (backend file-spec regexps)
  (let ((next-section-fn (plist-get backend :next-section-fn))
        (filter-result-fn (or (plist-get backend :filter-result-fn)
                              'identity)))
    (thread-last file-spec
      (file-expand-wildcards)
      (mapcar #'expand-file-name)
      (stream) ; make computation lazy just before expensive tasks
      (seq-map (lambda (filename)
                 (list :filename filename
                       :buffer (or (get-file-buffer filename)
                                   (let ((temp-buffer (generate-new-buffer " *temp*")))
                                     (with-current-buffer temp-buffer
                                       (insert-file-contents filename))
                                     temp-buffer)))))
      (seq-map (lambda (data-buffer)
                 (-buffer-section-stream next-section-fn data-buffer)))
      (stream-concatenate)
      (seq-filter (lambda (data-section)
                    (-buffer-section-matches-regexps-p data-section regexps)))
      (seq-map (lambda (data-result)
                 (with-current-buffer (plist-get data-result :buffer)
                   (let* ((start (plist-get data-result :start-pos))
                          (end (plist-get data-result :end-pos))
                          (snippet (buffer-substring-no-properties start
                                                                   (min (+ 300 start)
                                                                        end))))
                     (string-match "^\\(.*\\)\n\\([\0-\377[:nonascii:]]+\\)" snippet)
                     (append data-result (list :snippet snippet
                                               :title (match-string 1 snippet)
                                               :body (match-string 2 snippet)))))))
      (seq-map (lambda (data-result)
                 (funcall filter-result-fn data-result)))
      )))

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
       (goto-char (or pos 1))
       (let ((bounds (funcall next-section-fn)))
         (if bounds
             (cons (append data (list :start-pos (car bounds)
                                      :end-pos (cdr bounds)))

                   (-buffer-section-stream next-section-fn
                                           data
                                           (point)))
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

(defun -move-to-next-section-by-separator (section-separator)
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

(defun -generic-visit-result (search-result)
  (find-file-noselect (plist-get search-result :filename)))

(defun -backend-for-file-spec (file-spec)
  (let* ((search-config (-search-config-for file-spec))
         (backend-id (plist-get search-config :backend)))
    (plist-get velocity-backends backend-id)))

(defun -lookup-prop (file property-name)
  (let* ((backend-id (plist-get (-search-config-for file)
                                :backend))
         (backend (plist-get velocity-backends backend-id)))
    (plist-get backend property-name)))

(defun -search-config-for (file)
  (let ((search-configs
         (loop for section in velocity-searches
               append (cdr section))))
    (-find (lambda (config)
             (-contains? (plist-get config :files) file))
           search-configs)))

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
