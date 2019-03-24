(defvar lively-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Lively minor mode")

;;;###autoload
(define-minor-mode lively-mode
  :lighter " lv"
  :keymap lively-mode-map
  (message "lively mode enabled")
  "Get your foos in the right places.")

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defvar lively-rpc-logging t
  "Should a log buffer keep all the messages? Useful for debugging.")

(defvar lively-rpc--call-id 0
  "Call id of the last lively-rpc call.
Used to associate responses to callbacks.")
(make-variable-buffer-local 'lively-rpc--call-id)

(defvar lively-rpc--current-peer-id nil
  "Id of l2l peer that is the current target of the session. See
  `lively-interactively-select-peer'.")
(make-variable-buffer-local 'lively-rpc--current-peer-id)

(defvar lively-rpc--buffer-p)
(make-variable-buffer-local 'lively-rpc--buffer-p)

(defvar lively-rpc--log-buffer-name)
(make-variable-buffer-local 'lively-rpc--log-buffer-name)

(defvar lively-rpc--buffer)
;; (make-variable-buffer-local 'lively-rpc--buffer)

(defvar lively-rpc--backend-root-dir)
(make-variable-buffer-local 'lively-rpc--backend-root-dir)

(defvar lively-rpc--backend-node-command)
(make-variable-buffer-local 'lively-rpc--backend-node-command)

(defvar lively-rpc--callback-fifo (make-ring 100)
  "Stores callbacks for in-flight messages.")

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

(defun lively--open (root-dir)
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
            default-directory root-dir
            proc (condition-case err
                     (let ((process-connection-type nil)
                           (process-environment (lively-rpc--environment)))
                       (start-process name
                                      (current-buffer)
                                      full-node-command
				      "index.js"))
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
                  (setq json (let ((json-array-type 'list))
                               (json-read)))
                  (if (listp json)
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
             ((looking-at "lively.emacs started, version (\\([^ ]*\\))\n")
              (let ((rpc-version (match-string 1)))
                (replace-match "")
		(message "connected to lively.emacs version %s" rpc-version)))
             (t
              (let ((line (buffer-substring (point-min)
                                            line-end)))
                (delete-region (point-min) line-end)
                (lively-rpc--handle-unexpected-line line))))))))))

(defvar lively-rpc--result)

(defun lively-rpc--handle-json (json)
  ""
  ;; (message "got json %s" json)
  (setq lively-rpc--result json)
  ;; (let ((peers (make-hash-table))))
  )


(defun lively-peer-stringify (peer)
  "`peer' as returned from listPeers command"
  (let* ((world (alist-get 'world peer))
	 (loc (alist-get 'location peer))
	 (type (alist-get 'type peer))
	 (format-string (format "%s - %s" loc (or world type))))
    format-string))

(defun lively-peer-helm-candidate-transformer (peer)
  ""
  (let* ((id (alist-get 'id peer)))
    `(,(lively-peer-stringify peer)  . ,peer)))

(defun lively-complete-peer-with-helm (peers &optional current-peer-id)
  "Asks user for completion of a lively peer. Returns the peer data"
  (let* ((current (loop for peer in peers
			if (equal current-peer-id (alist-get 'id peer))
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
	  :preselect (lively-peer-stringify current)
          :buffer "*helm lively peer selection*")))

(defun lively-rpc--handle-unexpected-line (line)
  ""
  (message "unexpected line %s" line))

(defun rk/lively-open ()
  ""
  ;; (lively--open "/home/robertkrahn/scratch/ts_playground")
  (lively--open (expand-file-name "~/projects/lively/emacs-plugin"))
  ;; (pop-to-buffer lively-rpc--buffer)
  (when-let ((log-buf (lively-rpc--get-log-buffer lively-rpc--buffer)))
    (pop-to-buffer log-buf))
  (pop-to-buffer "lively-mode.el"))

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

(defun lively-fetch-peers ()
  ""
  (let ((peer-data (rk/lively-send
		    (let ((msg (make-hash-table)))
		      (puthash "action" "listPeers" msg)
		      msg))))
    (if-let ((err (alist-get 'error peer-data)))
    	err
      (when-let ((result (alist-get 'result peer-data)))
    	(if-let ((err (alist-get 'error result)))
    	    err)
    	result))))

(defun lively-interactive-select-peer ()
  ""
  (interactive)
  (let* ((current-id (buffer-local-value 'lively-rpc--current-peer-id lively-rpc--buffer))
	 (selected (lively-complete-peer-with-helm (lively-fetch-peers) current-id))
	 (selected-id (and selected (alist-get 'id selected))))
    (message "Selecting %s" (if selected (lively-peer-stringify selected)
			      "emacs l2l client"))
    (with-current-buffer lively-rpc--buffer
      (setq lively-rpc--current-peer-id selected-id))))

(defun lively-eval (expr)
  (rk/lively-send
   (let ((target (buffer-local-value 'lively-rpc--current-peer-id lively-rpc--buffer))
	 (msg (make-hash-table)))
     (puthash "action" "runEval" msg)
     (puthash "source" expr msg)
     (puthash "peerId" target msg)
     msg)))

(defun lively-interactive-eval (expr)
  (interactive "Sjs code: ")
  (message "%s" (lively-eval expr)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
  (rk/lively-open)

  (lively-interactive-select-peer)
  (lively-interactive-eval)

  (rk/lively-send (let ((msg (make-hash-table)))
		    (puthash "action" "runEval" msg)
		    (puthash "source" "[typeof global, typeof window]" msg)
		    (puthash "peerId" (buffer-local-value 'lively-rpc--current-peer-id lively-rpc--buffer) msg)
		    msg))

  (rk/lively-send (let ((msg (make-hash-table))) (puthash "action" "listPeers" msg) msg) 23)

  )


