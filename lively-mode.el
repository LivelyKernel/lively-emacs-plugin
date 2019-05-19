;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; key bindings

(defvar lively-mode-map
  (make-sparse-keymap)
  "Keymap for Lively minor mode")

(define-key lively-mode-map (kbd "C-x C-e") 'lively-eval-selection-or-line)
(define-key lively-mode-map (kbd "C-x C-p") 'lively-eval-and-print-selection-or-line)
(define-key lively-mode-map (kbd "C-c M-p") 'lively-interactive-select-peer)

(define-prefix-command 'lively-prefix-map)
;; (define-key lively-mode-map (kbd "C-c y") 'lively-prefix-map)
(define-key global-map (kbd "C-c y") 'lively-prefix-map)
(define-key lively-prefix-map (kbd "s") 'lively-start)
(define-key lively-prefix-map (kbd "q") 'lively-quit)
(define-key lively-prefix-map (kbd "l") 'lively-show-rpc-log-buffer)
;; (define-key lively-prefix-map (kbd ".") 'lively-eval-selection-or-line)
;; (define-key lively-prefix-map (kbd ".") 'lively-interactive-eval)
(define-key lively-prefix-map (kbd "<tab>") 'lively-company-backend)
(define-key lively-prefix-map (kbd "<C-tab>") 'lively-completions-at-point)
(define-key lively-prefix-map (kbd "p") 'lively-interactive-select-peer)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; lively-mode

;;;###autoload
(define-minor-mode lively-mode
  "Mode for interacting with lively.next processes."
  :init-value nil
  :lighter " lv"
  :keymap lively-mode-map
  (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends '(company-tide :separate lively-company-backend))
  (add-to-list 'company-backends '(lively-company-backend :separate company-tide))
  (message "lively mode %s" (if lively-mode "enabled" "disabled")))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defvar lively-rpc-logging t
  "Should a log buffer keep all the messages? Useful for debugging.")

(defvar lively-rpc--call-id 0
  "Call id of the last lively-rpc call.
Used to associate responses to callbacks.")
(make-variable-buffer-local 'lively-rpc--call-id)

(defvar lively-rpc--buffer-p)
(make-variable-buffer-local 'lively-rpc--buffer-p)

(defvar lively-rpc--log-buffer-name)
(make-variable-buffer-local 'lively-rpc--log-buffer-name)

(defvar lively-rpc--buffer nil
  "Helper buffer for processing received l2l messages.")
;; (make-variable-buffer-local 'lively-rpc--buffer)

(defvar lively-rpc--backend-root-dir)
(make-variable-buffer-local 'lively-rpc--backend-root-dir)

(defvar lively-rpc--backend-node-command)
(make-variable-buffer-local 'lively-rpc--backend-node-command)

(defvar lively-rpc--callback-fifo (make-ring 100)
  "Stores callbacks for in-flight messages.")

(defvar lively-rpc--result nil
  "stores l2l message from lively server")

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defvar lively-rpc--current-peer-id nil
  "Id of l2l peer that is the current target of the session. See
  `lively-interactively-select-peer'.")
(make-variable-buffer-local 'lively-rpc--current-peer-id)

(defvar lively-rpc--current-module-id nil
  "Id of module used for evalution requests.")
(make-variable-buffer-local 'lively-rpc--current-module-id)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defvar lively--local-base-path nil
  "The path to the directory that locally hosts the lively
  installation. Used to map lively modules to local files.")

(defcustom *lively-default-local-base-paths*
  '("/home/robert/projects/lively/")
  "Directories with local lively installations.")

(defvar *lively-local-bash-paths-history* nil
  "For completion.")

(defvar lively-rpc--session-state nil
  "System.baseURL, fetched and cached when needed.")
(make-variable-buffer-local 'lively-rpc--session-state)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defcustom *lively-default-lively-servers*
  '("http://localhost:9011"
    "http://lively-next.org:9011/lively-socket.io")
  "lively servers that are offered to connect to on start")

(defvar *lively-chosen-server-history* nil
  "Servers that were chosen at `lively-start'.")

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; buffer handling

(defun lively-rpc--get-log-buffer (lively-rpc-buf)
  "Retrieve the log buffer"
  (when-let ((log-name (buffer-local-value 'lively-rpc--log-buffer-name lively-rpc-buf)))
    (get-buffer log-name)))

(defun lively-rpc--log-receive (lively-rpc-buf string)
  ""
  (when-let ((log-buffer (lively-rpc--get-log-buffer lively-rpc-buf)))
    (with-current-buffer log-buffer
      (unless (equal (line-beginning-position) (point))
	(insert "\n"))
      (insert (format-time-string "[%FT%T SEND] "))
      (insert string))))

(defun lively-rpc--log-send (lively-rpc-buf string)
  ""
  (when-let ((log-buffer (lively-rpc--get-log-buffer lively-rpc-buf)))
    (with-current-buffer log-buffer
      (unless (equal (line-beginning-position) (point))
	(insert "\n"))
      (insert (format-time-string "[%FT%T RECV] "))
      (insert string))))

(defun lively-rpc--process-buffer-p (buffer)
  "Return non-nil when BUFFER is a live lively-rpc process buffer.

If BUFFER is a buffer for an lively-rpc process, but the process
died, this will kill the process and buffer."
  (cond
   ((or (not buffer)
        (not (buffer-live-p buffer)))
    nil)
   ((not (buffer-local-value 'lively-rpc--buffer-p buffer))
    nil)
   ((and (get-buffer-process buffer)
         (process-live-p (get-buffer-process buffer)))
    t)
   (t
    (ignore-errors
      (kill-process (get-buffer-process buffer)))
    (ignore-errors
      (kill-buffer buffer))
    nil)))

(defun lively-rpc--cleanup-buffers ()
  "Close RPC buffers that have not been used in five minutes."
  (dolist (buffer (buffer-list))
    (when (lively-rpc--process-buffer-p buffer)
      (when-let ((log-buffer (lively-rpc--get-log-buffer buffer)))
	(ignore-errors
          (kill-buffer log-buffer)))
      (ignore-errors
        (kill-process (get-buffer-process buffer)))
      (ignore-errors
        (kill-buffer buffer)))

    ;; (when lively-rpc-maximum-buffer-age
    ;;   (let ((old (- (float-time)
    ;;                 lively-rpc-maximum-buffer-age)))
    ;;     (dolist (buffer (buffer-list))
    ;;       (when (and (lively-rpc--process-buffer-p buffer)
    ;;                  (< (or (buffer-local-value 'lively-rpc--last-call buffer)
    ;;                         old)
    ;;                     old))
    ;;         (ignore-errors
    ;;           (kill-process (get-buffer-process buffer)))
    ;;         (ignore-errors
    ;;           (kill-buffer buffer))))))
    ))

(defun lively-show-rpc-log-buffer ()
  ""
  (interactive)
  (if lively-rpc--buffer
      (pop-to-buffer (lively-rpc--get-log-buffer lively-rpc--buffer))
    (message "no lively-rpc-buffer found in %s" (current-buffer))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; lively process

(defun lively-rpc--environment ()
  "Return a `process-environment' for the RPC process.

This includes `lively-rpc-nodepath' in the NODEPATH, if set."
  process-environment
  ;; (if (or (not lively-rpc-nodepath)
  ;;         (not (file-exists-p (expand-file-name "lively/__init__.py"
  ;;                                               lively-rpc-nodepath))))
  ;;     process-environment
  ;;   (let* ((old-nodepath (getenv "NODEPATH"))
  ;;          (new-nodepath (if old-nodepath
  ;;                              (concat lively-rpc-nodepath
  ;;                                      path-separator
  ;;                                      old-nodepath)
  ;;                            lively-rpc-nodepath)))
  ;;     (cons (concat "NODEPATH=" new-nodepath)
  ;;           process-environment)))
  )

(defun lively-rpc--empty-session-state ()
  ""
  '((system-base-url . nil)
    (local-lively-dir . nil)))

(defun lively-rpc--reset-session-state (lively-rpc-buffer)
  ""
  (with-current-buffer lively-rpc-buffer
    (setq lively-rpc--session-state (lively-rpc--empty-session-state))))

(defun lively--open (server-address root-dir)
  "Start a l2l node process."
  (lively-rpc--cleanup-buffers)
  (let* ((full-node-command (executable-find "node"))
         (name (format " *lively [project:%s node:%s]*" root-dir full-node-command))
         (new-lively-rpc-buffer (generate-new-buffer name))
         (log-name (if lively-rpc-logging (format " *lively log [project:%s node:%s]*" root-dir full-node-command)))
         (new-lively-rpc-log-buffer (if lively-rpc-logging (generate-new-buffer log-name)))
         (proc nil))
    (when (not full-node-command)
      (error "Can't find Node command, configure `lively-rpc-node-command'"))
    (with-current-buffer new-lively-rpc-buffer
      (setq lively-rpc--buffer-p t
            lively-rpc--buffer (current-buffer)
            lively-rpc--backend-root-dir root-dir
            lively-rpc--backend-node-command full-node-command
	    lively-rpc--log-buffer-name log-name
	    lively-rpc--current-peer-id nil ;; connects to the emacs l2l client itself
	    lively-rpc--session-state (lively-rpc--empty-session-state)
            default-directory root-dir
            proc (condition-case err
                     (let ((process-connection-type nil)
                           (process-environment (lively-rpc--environment)))
                       (start-process name
                                      (current-buffer)
                                      full-node-command
				      "index.js"
				      "--server-address"
				      server-address))
                   (error
                    (concat "Lively can't start Node (%s: %s)"
			    (car err) (cadr err)))))
      (set-process-query-on-exit-flag proc nil)
      ;; (set-process-sentinel proc #'lively-rpc--sentinel)
      (set-process-filter proc #'lively-rpc--filter)
      ;; (lively-rpc-init root-dir
      ;;                  (lambda (result)
      ;; 			 (message "running! %s" result)))
      )
    new-lively-rpc-buffer))

(defun lively-rpc--filter (process output)
  "The filter for the RPC process."
  (let ((buffer (process-buffer process)))
    (when (and buffer
               (buffer-live-p buffer))
      (lively-rpc--log-receive buffer output)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output)
        (while (progn
                 (goto-char (point-min))
                 (search-forward "\n" nil t))
          (let ((line-end (point))
                (json nil)
                (did-read-json nil))
            (goto-char (point-min))
            (condition-case _err
                (progn
                  (setq json (let ((json-array-type 'list)
				   (json-object-type 'hash-table))
                               (json-read)))
                  (if (or (hash-table-p json) (listp json))
                      (setq  line-end (1+ (point))
                             did-read-json t)
                    (goto-char (point-min))))
              (error
               (goto-char (point-min))))
            (cond
             (did-read-json
	      ;; (let ((raw-json (buffer-substring (point-min) line-end)))
	      ;; 	(with-current-buffer log-buffer
	      ;; 	  (insert raw-json)))
              (delete-region (point-min) line-end)
              (lively-rpc--handle-json json))
             ((looking-at "lively.emacs started, version \\(.+\\)\n")
              (let ((rpc-version (match-string 1)))
                (replace-match "")
		(message "connected to lively.emacs version %s" rpc-version)))
	     ((looking-at (regexp-quote "[lively.modules] SystemJS configured with lively.transpiler & babel"))
              (delete-region (point-min) (point-max)))
             (t
              (let ((line (buffer-substring (point-min)
                                            line-end)))
                (delete-region (point-min) line-end)
                (lively-rpc--handle-unexpected-line line))))))))))

(defun lively-json-get (obj &rest keys)
  ""
  (loop for key in keys
	while (hash-table-p obj)
	do (setq obj (gethash key obj))
	finally (return obj)))

(defun lively-rpc--handle-json (json)
  ""
  ;; (message "got json %s" json)
  (setq lively-rpc--result json)
  ;; (let ((peers (make-hash-table))))
  )

(defun lively-rpc--handle-unexpected-line (line)
  ""
  (message "unexpected line %s" line))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; l2l peer selection

(defun lively-peer-stringify (peer)
  "`peer' as returned from listPeers command"
  (let* ((world (gethash "world" peer))
	 (loc (gethash "location" peer))
	 (type (gethash "type" peer))
	 (format-string (format "%s - %s" loc (or world type))))
    format-string))

(defun lively-peer-helm-candidate-transformer (peer)
  ""
  (let* ((id (gethash "id" peer)))
    `(,(lively-peer-stringify peer)  . ,peer)))

(defun lively-complete-peer-with-helm (peers &optional current-peer-id)
  "Asks user for completion of a lively peer. Returns the peer data"
  (let* ((current (loop for peer in peers
			if (equal current-peer-id (gethash "id" peer))
			return peer))
	 (candidates (seq-map 'lively-peer-helm-candidate-transformer peers))
	 (sources (helm-build-sync-source "lively peers"
		    :candidates candidates
		    ;; :candidate-transformer (lambda (c) (list "fooo"))
		    ;; :real-to-display (lambda (candidate)
		    ;; 		       (setq iwashere candidate)
		    ;; 	     (message "hmmm: %s" candidate)
		    ;; 	     ;; candidate
		    ;; 	     )
		    )))
    (helm :prompt "Select peer: "
          :sources sources
	  :preselect (if current (lively-peer-stringify current) "")
          :buffer "*helm lively peer selection*")))

(defun lively-start (server-address)
  "Start a lively client that connects to `server-address' which
should identify a runnig lively.server."
  (interactive (list
		  (helm-comp-read "Connect to lively server: "
		  *lively-default-lively-servers*
		  :history *lively-chosen-server-history*)))
  (let ((server-address (if (string-match "/lively-socket\\.io$" server-address)
			    server-address
			  (concat server-address
				  (if (string-match "/$" server-address) "" "/")
				  "lively-socket.io"))))
    (lively--open
     server-address
     (expand-file-name "~/projects/lively/emacs-plugin"))))

(defun lively-quit ()
  ""
  (interactive)
  (lively-rpc--cleanup-buffers))

(defun rk/lively-send (msg &optional timeout-marker timeout-secs)
  ""
  (let ((payload (concat (json-encode msg) "\n"))
	(proc (get-buffer-process lively-rpc--buffer))
	(start (time-to-seconds))
	(timeout-secs (or timeout-secs 30))
	(lively-rpc--result))
    (lively-rpc--log-send lively-rpc--buffer payload)
    (process-send-string proc payload)
    (while (and (not lively-rpc--result)
		(< (time-to-seconds (time-since start)) timeout-secs))
      (accept-process-output proc .5))
    (or lively-rpc--result timeout-marker)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun lively-send-fetch-peers-msg ()
  ""
  (let ((peer-data (rk/lively-send
		    (let ((msg (make-hash-table)))
		      (puthash "action" "listPeers" msg)
		      msg))))
    (if-let ((err (gethash "error" peer-data)))
    	err
      (when-let ((peers (gethash "result" peer-data)))
    	(if-let ((err (and (hash-table-p peers) (gethash "error" peers))))
    	    err)
    	peers))))

(defun lively-interactive-select-peer ()
  ""
  (interactive)
  (let* ((current-id (buffer-local-value 'lively-rpc--current-peer-id lively-rpc--buffer))
	 (selected (lively-complete-peer-with-helm (lively-send-fetch-peers-msg) current-id))
	 (selected-id (and selected (gethash "id" selected))))
    (message "Selecting %s" (if selected (lively-peer-stringify selected)
			      "emacs l2l client"))
    (lively-change-peer lively-rpc--buffer selected-id)))

(defun lively-change-peer (lively-rpc-buffer peer-id)
  ""
  (with-current-buffer lively-rpc-buffer
    (setq lively-rpc--current-peer-id peer-id)
    (lively-rpc--reset-session-state lively-rpc-buffer)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun lively-send-eval-msg (expr &optional target-module inspect inspect-depth)
  (rk/lively-send
   (let ((target (buffer-local-value 'lively-rpc--current-peer-id lively-rpc--buffer))
	 (msg (make-hash-table)))
     (puthash "action" "runEval" msg)
     (puthash "source" expr msg)
     (puthash "peerId" target msg)
     (puthash "targetModulte" (or target-module "lively://emacs-plugin/dummy-module") msg)
     (puthash "asString" t msg)
     (puthash "inspect" inspect msg)
     (puthash "inspectDepth" inspect-depth msg)
     msg)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun lively-send-completions-msg (prefix &optional target-module)
  (rk/lively-send
   (let ((target (buffer-local-value 'lively-rpc--current-peer-id lively-rpc--buffer))
	 (msg (make-hash-table)))
     (puthash "action" "completions" msg)
     (puthash "prefix" prefix msg)
     (puthash "peerId" target msg)
     (puthash "targetModule" (or target-module "lively://emacs-plugin/dummy-module") msg)
     msg)))

(defun lively-completion-prefix-at-point ()
  ""
  (save-excursion
    (loop with start = (point)
	  for pos from (current-column) downto 0
	  do (left-char)
	  for stop = (and
		      (not (in-string-p))
		      (let ((c (char-after (point))))
			(or (eql ?\  c) (eql ?= c))))
	  when stop do (forward-char 1)
	  until stop
	  finally (return (buffer-substring-no-properties (point) start))))
  ;; (buffer-substring-no-properties
  ;;  (save-excursion (skip-syntax-backward "w_.") (point))
  ;;  (point))
  )

(defun lively-completions-at-point ()
  ""
  (interactive)
  (lively-completions-for-prefix (company-grab-symbol) (lively-completion-prefix-at-point)))

(defun lively-completions-for-prefix (symbol prefix &optional target-module)
  ""
  (let* ((response (lively-send-completions-msg prefix target-module))
	 (err (gethash "error" response))
	 (completions (unless err
			(loop for (proto proto-completions)
			      in (gethash "completions" (gethash "result" response))
			      append (loop for ea in proto-completions
					   when (string-prefix-p symbol ea)
					   collect `(,proto . ,ea))))))
    (when err (error err))
    completions))

(defun lively-company-active-p ()
  ""
  lively-mode)

(defun lively-company-backend (command &optional arg &rest ignored)
  "A company-mode backend for Lively!"
  (interactive (list 'interactive))
  (cl-case command
    ;; (require-match 'never)
    ;; init => Called once per buffer

    (init
     (lively-company-active-p))

    ;; (interactive
    ;;  (company-begin-backend 'lively-company-backend))

    ;; prefix => return the prefix at point
    (prefix
     (when (and (lively-company-active-p)
		(not (company-in-string-or-comment)))
       ;; (lively-completion-prefix-at-point)
       (cons (company-grab-symbol) t)))

    (company-backends
     '(company-tide))

    ;; candidates <prefix> => return candidates for this prefix
    (candidates
     (when (and (lively-company-active-p)
		(not (company-in-string-or-comment)))
       (lexical-let ((prefix arg))
	 (cons :async
	       (lambda (callback)
		 (funcall
		  callback
		  (seq-map (lambda (ea)
			     (let* (
				    ;; (type (plist-get compl :type))
				    ;; (module-name (plist-get compl :module_name))
				    ;; (name (plist-get compl :name))
				    ;;(meta (concat type (if module-name (concat " from " module-name) "")))
				    )
			       (propertize (cdr ea) 'meta (car ea))
			       ;; (cdr ea)
			       ))
			   (lively-completions-for-prefix prefix (lively-completion-prefix-at-point) lively-rpc--current-module-id)
			   ;; (lively-completions-at-point)
			   )))))))

    ;; sorted => t if the list is already sorted
    ;; (sorted
    ;;  t)

    ;; duplicates => t if there could be duplicates
    (duplicates
     nil)

    ;; annotation <candidate> => short docstring for completion buffer
    (annotation
     (format " (%s)" (get-text-property 0 'meta arg)))

    (meta
     (message "meta? %s" arg)
     nil)
    (post-completion
     nil)))

;; (add-to-list 'company-backends 'lively-company-backend)
;; (add-to-list 'company-backends '(company-tide :separate lively-company-backend))
;; (add-to-list 'company-backends '( lively-company-backend :separate company-tide))
;; (pop company-backends)
;; (setq company-backends (remove 'lively-company-backend company-backends))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun lively-interactive-eval (expr &optional target-module inspect)
  (interactive "Sjs code: ")
  (let* ((inspect-depth (when inspect
			  (cond
			   ((numberp inspect) inspect)
			   ((equal '(4) inspect) 2)
			   ((and (consp inspect) (numberp (car inspect)))
			    (car inspect))
			   (t 2))))
	 (inspect (not (null inspect)))
	 (result-msg (lively-send-eval-msg expr target-module inspect inspect-depth))
	 (val (lively-json-get result-msg "result" "value"))
	 (err (or (lively-json-get result-msg "error")
		  (and (lively-json-get result-msg "result" "isError") val))))
    (or err val)))

(defun lively-eval-selection-or-line (inspect)
  ""
  (interactive "P")
  (let ((code (rk/selection-or-line-string)))
    (rk/flash-selection-or-line)
    (message "%s" (lively-interactive-eval code lively-rpc--current-module-id inspect))))

(defun lively-eval-and-print-selection-or-line (inspect)
  ""
  (interactive "P")
  (let ((code (rk/selection-or-line-string)))
    (rk/flash-selection-or-line)
    (let ((result (lively-interactive-eval code lively-rpc--current-module-id inspect)))
      (when (region-active-p)
	(let ((insertion-point (max (region-beginning) (region-end))))
	 (deactivate-mark)
	 (goto-char insertion-point)))
      (push-mark)
      (insert (if (char-or-string-p result) result (prin1-to-string result))))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun lively-set-local-base-path (base-path)
  ""
  (interactive (list
		(helm-comp-read "Local base path of lively installation: "
				*lively-default-local-base-paths*
				:history *lively-local-bash-paths-history*
				:default lively--local-base-path)))
  (setq lively--local-base-path base-path))

(defun lively-internal-url-for-file (file-path)
  "Returns a resource url for the file"
  (if-let* ((lively-dir lively--local-base-path)
	    (_ (string-prefix-p lively-dir file-path))
	    (relative-path (f-relative file-path lively-dir)))
      nil))

(defun lively-system-base-url ()
  ""
  (if-let* ((buf lively-rpc--buffer))
      (with-current-buffer buf
	(or
	 (alist-get 'system-base-url lively-rpc--session-state)
	 (setq lively-rpc--session-state
	       (cons
		`(system-base-url . ,(lively-interactive-eval "lively.modules.System.baseURL"))
		(remove* 'system-base-url lively-rpc--session-state :key 'car)))))))

(with-current-buffer lively-rpc--buffer lively-rpc--session-state)
(with-current-buffer lively-rpc--buffer (alist-get 'system-base-url lively-rpc--session-state))
(with-current-buffer lively-rpc--buffer (setq lively-rpc--session-state (lively-rpc--empty-session-state)))
(set-default 'lively-rpc--session-state nil)
lively-rpc--session-state


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'lively-mode)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
  (require 'lively-mode)
  (rk/lively-open)

  (setq completions (lively-send-completions-msg "global.a"))

  (lively-json-get completions "result" "prefix")
  (lively-json-get completions "result")



  (with-current-buffer "test.js"
    (company-grab-symbol))


  (lively-interactive-select-peer)
  (lively-interactive-eval "1+2" )

  (rk/lively-send (let ((msg (makehash-table)))
		    (puthash "action" "runEval" msg)
		    (puthash "source" "[typeof global, typeof window]" msg)
		    (puthash "peerId" (buffer-local-value 'lively-rpc--current-peer-id lively-rpc--buffer) msg)
		    msg))

  (rk/lively-send (let ((msg (make-hash-table))) (puthash "action" "listPeers" msg) msg) 23))
