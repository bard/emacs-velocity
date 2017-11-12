(eval-when-compile (require 'names))

(define-namespace velocity-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC API

(defun search (search-query file-specs)
  (loop with regexps = (-search-query-to-regexps search-query)
        for file-spec in file-specs
        append (let* ((search-config (-search-config-for file-spec))
                      (backend-id (plist-get search-config :backend))
                      (backend (plist-get velocity-backends backend-id))
                      (files (file-expand-wildcards file-spec)))
                 (-search-files files backend regexps))))

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

(require 'dash)

(defun -search-files (files backend regexps)
  (loop for file in files
        append (let* ((next-section-fn (plist-get backend :next-section-fn))
                      (filter-result-fn (or (plist-get backend :filter-result-fn)
                                            'identity))
                      (results (-search-sections-in-file file
                                                         regexps
                                                         next-section-fn)))
                 (loop for result in results
                       collect (funcall filter-result-fn
                                        (plist-put result :filename file))))))

(defmacro -with-possibly-opened-file (file &rest body)
  (declare (indent 1) (debug t))
  `(let ((file-buffer (get-file-buffer ,file)))
     (if file-buffer
         (with-current-buffer file-buffer
           (save-excursion
             (goto-char (point-min))
             ,@body))
       (with-temp-buffer
         (insert-file-contents ,file)
         ,@body))))

(defun -search-sections-in-file (file regexps next-section-fn)
  (-with-possibly-opened-file
   file
   (let ((case-fold-search t))
     (thread-last next-section-fn
       (velocity--map-sections
        (lambda (start end)
          (and (-region-matches-all-p start end regexps)
               (-parse-section start end))))
       (remove nil)))))

(defun -map-sections (fn next-section-fn)
  (loop for section-bounds = (funcall next-section-fn)
        while section-bounds
        collect (save-excursion
                  (funcall fn
                           (car section-bounds)
                           (cdr section-bounds)))))

(defun -parse-section (start end)
  (let ((snippet (buffer-substring-no-properties start
                                                 (min (+ 300 start)
                                                      end))))
    (string-match "^\\(.*\\)\n\\([\0-\377[:nonascii:]]+\\)" snippet)
    (list :start-pos start
          :end-pos end
          :snippet snippet
          :title (match-string 1 snippet)
          :body (match-string 2 snippet))))

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
