;;; slack-rpc.el

(defgroup slack-rpc nil
"Slack RPC wrapping functions."
 :prefix "slack-rpc-"
 :group 'apis)

(require 'json)

(defconst slack-rpc-end-point "https://slack.com/api/"
  "Slack Web API end point URL.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; channels apis
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
;; chat apis
(defun slack-rpc-chat-delete (token ts channel))

(defun slack-rpc-chat-post-message (token channel text &optional
				    username as-user parse link-names attachments
				    unfurl-links unfurl-media icon-url icon-emoji))

(defun slack-rpc-chat-update (token ts channel text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emoji
(defun slack-rpc-emoji-list (token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; files api
(defun slack-rpc-files-delete (token file))



(provide 'slack-rpc)
