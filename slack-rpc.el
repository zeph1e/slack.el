;;; slack-rpc.el

(defgroup slack-rpc nil
"Slack RPC wrapping functions."
 :prefix "slack-rpc-"
 :group 'apis)

(require 'json)
(require 'slack-http)
(require 'slack-error)

(defconst slack-rpc-end-point "https://slack.com/api/"
  "Slack Web API end point URL.")

(defvar slack-rpc-request-id-counter 0
  "Slack request id counter.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; api
(defun slack-rpc-api-test (callback &optional arg-alist)
  "This method helps you test your calling code.
Web API Desc. @ https://api.slack.com/methods/

Arguments:
  CALLBACK   : Callback to receive response.
  ARG-ALIST  : Example property to return.

Return:
  CONTEXT-ID : a unique ID for request context.

Errors:
  This method has no expected error responses. However, other errors can be
  returned in the case where the service is down or other unexpected factors
  affect processing. Callers should always check the value of the ok params
  in the response.
"
  (if (not (functionp callback))
    (error "slack-rpc-api-test: callback is not a valid function")
    (let ((request-id (slack-rpc-new-request-id)))
      (if (process-live-p (get-buffer-process
			   (slack-http-post (concat slack-rpc-end-point "api.test")
					    'arg-alist callback request-id)))
	  request-id)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; channels
(defun slack-rpc-channels-archive (token channel)
  "This method archives a channel.

Arguments:
  TOKEN : Authentication token (Requires scope: post)
  CHANNEL : Channel to archive

Return:

Errors:
  channel-not-found : Value passed for channel was invalid.
  already-archived : Channel has already been archived.
  cant-archive-general : You cannot archive the general channel
  last-ra-channel : You cannot archive the last channel for a restricted account
  restricted-action : A team preference prevents the authenticated user from archiving.
  not-authed : No authentication token provided.
  invalid-auth : Invalid authentication token.
  account-inactive : Authentication token is for a deleted user or team.
  user-is-bot : This method cannot be called by a bot user.
  user-is-restricted : This method cannot be called by a restricted user or single channel guest.
"
)

(defun slack-rpc-channels-create (token name))

(defun slack-rpc-channels-history (token channel &optional latest oldest inclusive count))

(defun slack-rpc-channels-info (token channel))

(defun slack-rpc-channels-invite (token channel user))

(defun slack-rpc-channels-join (token name))

(defun slack-rpc-channels-kick (token channel user))

(defun slack-rpc-channels-leave (token channel))

(defun slack-rpc-channels-list (token &optional exclude-archived))

(defun slack-rpc-channels-mark (token channel ts))

(defun slack-rpc-channels-rename (token channel name))

(defun slack-rpc-channels-set-purpose (token channel purpose))

(defun slack-rpc-channels-unarchive (token channel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chat
(defun slack-rpc-chat-delete (token ts channel))

(defun slack-rpc-chat-post-message (token channel text &optional
				    username as-user parse link-names attachments
				    unfurl-links unfurl-media icon-url icon-emoji))

(defun slack-rpc-chat-update (token ts channel text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emoji
(defun slack-rpc-emoji-list (token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; files
(defun slack-rpc-files-delete (token file))

(defun slack-rpc-files-info (token file &optional count page))

(defun slack-rpc-files-list (token &optional user ts-from ts-to types count page))

(defun slack-rpc-files-upload (token &optional file content filetype filename title
				     initial-comment channels))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; groups
(defun slack-rpc-groups-archive (token channel))

(defun slack-rpc-groups-close (token channel))

(defun slack-rpc-groups-create (token name))

(defun slack-rpc-groups-create-child (token channel))

(defun slack-rpc-groups-history (token channel &optional latest oldest inclusive count))

(defun slack-rpc-groups-info (token channel))

(defun slack-rpc-groups-invite (token channel user))

(defun slack-rpc-groups-kick (token channel user))

(defun slack-rpc-groups-leave (token channel))

(defun slack-rpc-groups-list (token exclude_archived))

(defun slack-rpc-groups-mark (token channel ts))

(defun slack-rpc-groups-open (token channel))

(defun slack-rpc-groups-rename (token channel name))

(defun slack-rpc-groups-set-purpose (token channel purpose))

(defun slack-rpc-groups-set-topic (token channel topic))

(defun slack-rpc-groups-unarchive (token channel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; im
(defun slack-rpc-im-close (token channel))

(defun slack-rpc-im-history (token channel &optional latest oldest inclusive count))

(defun slack-rpc-im-list (token))

(defun slack-rpc-im-mark (token channel ts))

(defun slack-rpc-im-open (token user))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; oauth
(defun slack-rpc-oauth-access (client-id client-secret code redirect-uri))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtm
(defun slack-rpc-rtm-start (token)
"
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search
(defun slack-rpc-search-all (token query &optional sort sort-dir highlight count page))

(defun slack-rpc-search-files (token query &optional sort sort-dir highlight count page))

(defun slack-rpc-search-messages (token query &optional sort sort-dir highlight count page))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stars
(defun slack-rpc-stars-list (token &optional user count page))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; team
(defun slack-rpc-team-access-logs (token &optional count page))

(defun slack-rpc-team-info (token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; users
(defun slack-rpc-users-get-presence (token user))

(defun slack-rpc-users-info (token user))

(defun slack-rpc-users-list (token))

(defun slack-rpc-users-set-active (token))

(defun slack-rpc-users-set-presence (token presence))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non api, util-functions
(defun slack-rpc-new-request-id ()
   (setq slack-rpc-request-id-counter (1+ slack-rpc-request-id-counter)))



(provide 'slack-rpc)
