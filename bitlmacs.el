(defvar bitlmacs/erc-bitlbee-channel
  "&bitlbee"
  "Name of the channel to pull the bitlbee nicklist from")

(defvar bitlmacs/im-open-color
  "darkblue"
  "Color of the background of the nicklist line when an IM window is open for the account")

(defvar bitlmacs/im-active-color
  "darkred"
  "Color of IM background when IM has new message")

(defvar bitlmacs/active-ims
  nil
  "List of currently active IMs")

(defun bitlmacs/im-msg-p (proc res)
  "Returns t if msg is an erc privmsg buffer, nil otherwise"
  (let ((channel-buffers     (erc-channel-list proc))
        (channel-name (car (erc-response.command-args res))))
    (if (member channel-name channel-buffers)
        nil
      t)))

(defun bitlmacs/add-active-im (proc res)
  "Adds an active IM to the list"
  (when (bitlmacs/im-msg-p proc res)
    (let ((im-name (or (car (split-string (erc-response.sender res) "!"))
                       (erc-response.sender res))))
      (when (not (member im-name bitlmacs/active-ims))
        (push im-name bitlmacs/active-ims)
        (bitlmacs/update-nicklist))))
  nil)

(add-hook 'erc-server-PRIVMSG-functions 'bitlmacs/add-active-im)

(defun bitlmacs/remove-active-ims ()
  (when (and (eq major-mode 'erc-mode)
             (member (buffer-name) bitlmacs/active-ims))
    (setq bitlmacs/active-ims (delete (buffer-name) bitlmacs/active-ims))
    (bitlmacs/update-nicklist))
  nil)

(defun bitlmacs/update-nicklist ()
  "Update nicklist. Useful for updating on buffer creation/deletion."
  (with-current-buffer bitlmacs/erc-bitlbee-channel
    (erc-nicklist-update))
  nil)

(add-hook 'window-configuration-change-hook 'bitlmacs/remove-active-ims)
(add-hook 'kill-buffer-hook 'bitlmacs/remove-active-ims)

(defun bitlmacs/winmove-update ()
  "Functions to run on select-window advice"
  ;; remove flag for the last visited buffer
  (with-current-buffer (cadr (buffer-list))
    (bitlmacs/remove-active-ims)))

(defadvice select-window (after bitlmacs/update-list-winmove activate compile)
  "After we change windows, if our new buffer is an active IM,
remove from list"
  (bitlmacs/winmove-update))


(defun bitlmacs/goto-next-im ()
  "Make buffer of next IM in buffer list active"
  )

(defun bitlmacs/goto-last-im ()
  "Make buffer of previous IM in buffer list active"
  )

(defun erc-nicklist-insert-contents (channel)
  "Insert the nicklist contents, with text properties and the optional images."
  (setq buffer-read-only nil)
  (erase-buffer)
  (dolist (u (erc-nicklist-channel-users-info channel))
    (let* ((server-user (car u))
	   (channel-user (cdr u))
	   (nick     (erc-server-user-nickname server-user))
	   (host     (erc-server-user-host server-user))
	   (login    (erc-server-user-login server-user))
	   (full-name(erc-server-user-full-name server-user))
	   (info     (erc-server-user-info server-user))
	   (channels (erc-server-user-buffers server-user))
	   (op       (erc-channel-user-op channel-user))
	   (voice    (erc-channel-user-voice channel-user))
	   (bbdb-nick (or (erc-nicklist-search-for-nick
			   (concat login "@" host))
			  ""))
	   (away-status (if voice "" "\n(Away)"))
	   (balloon-text (concat bbdb-nick (if (string= "" bbdb-nick)
					       "" "\n")
				 "Login: " login "@" host
				 away-status))
           (bg "black"))
      (when (get-buffer nick)
        (setq bg bitlmacs/im-open-color))
      (when (member nick bitlmacs/active-ims)
        (setq bg bitlmacs/im-active-color))
      (erc-nicklist-insert-medium-name-or-icon host channel (not voice))
      (unless (or voice erc-nicklist-use-icons)
	(setq nick (concat "(" nick ")")))
      (when op
	(setq nick (concat nick " (OP)")))

      (insert (erc-propertize nick
			      'erc-nicklist-nick nick
			      'mouse-face 'highlight
                              'face `(:background ,bg)
			      'erc-nicklist-channel channel
			      'help-echo balloon-text)
	      "\n")))
  (erc-nicklist-mode))

(provide 'bitlmacs)
