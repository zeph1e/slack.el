;;; slack-rpc.el

(require 'json)
(require 'slack-http)
(require 'slack-utils)

(defconst slack-rpc-end-point "https://slack.com/api/"
  "Slack Web API end point URL.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; api
(defun slack-rpc-api-test (&optional callback &rest keyword-args)
  "This method helps you test your calling code.
Web API Desc. @ https://api.slack.com/methods/api.test

Arguments:
  CALLBACK : Callback to handle response.
  KEYWORDS-ARGS : Any key-value pair can be given. The given key-value pair
                  will be echoed in response.

Return:
  When callback is a valid function, CONTEXT-ID, the unique id of request
  context, for this request will be returned. Otherwise, this will return
  the response as it is.

If you call this function with valid CALLBACK, the given CALLBACK will be
called, when the response of request is being received, with arguments
(CONTEXT-ID RESPONSE-ALIST). RESPONSE-ALIST might be in following form:

 (( ok . t ) ( args . ( foo . \"bar\" ))

If user specified an error in request, the response will be in following form:

 (( ok . :json-false ) ( error . \"my_error\" ) ( args . ( error . \"my_error\" ))

If no callback is given, this function will return RESPONSE-ALIST synchronously.
"
  (let ((req-id (slack-utils-id)))
    (slack-http-call-method 'api.test keyword-args callback req-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auth
(defun slack-rpc-auth-test (token &optional callback)
  "This method helps you test your token.
Web API Desc. @ https://api.slack.com/methods/auth.test

Arguments:
  TOKEN    : Slack auth token (required scope : identify)
  CALLBACK : Callback to handle response.

Return:
  When callback is a valid function, CONTEXT-ID, the unique id of request
  context, for this request will be returned. Otherwise, this will return
  the response as it is.

If you call this function with valid CALLBACK, the given CALLBACK will be
called, when the response of request is being received, with arguments
(CONTEXT-ID RESPONSE-ALIST). RESPONSE-ALIST might be in following form:

  (( ok . t ) ( url . \"https:\/\/myteam.slack.com\/\" )
   ( team . \"My Team\" ) ( user . \"cal\" )
   ( team_id . \"T12345\" ) ( user_id . \"U12345\" ))


If there's an error, the response will be delievered in following form:

  (( ok . :json-false ) ( error . \"not_authed\" ))

The value of error might be one of:

  not_authed       No authentication token provided.
  invalid_auth     Invalid authentication token.
  account_inactive Authentication token is for a deleted user or team.

If no callback is given, this function will return RESPONSE-ALIST synchronously.
"
  (let ((req-id (slack-utils-id))
	(args (list (cons 'token token))))
    (slack-http-call-method 'auth.test args callback req-id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; channels
(defun slack-rpc-channels-archive (token channel &optional callback)
  "This method archives a channel.
Web API Desc. @ https://api.slack.com/methods/channels.archive

Arguments:
  TOKEN    : Authentication token (Requires scope: post)
  CHANNEL  : Channel to archive
  CALLBACK : Callback to receive response

Return:
  When callback is a valid function, CONTEXT-ID, the unique id of request
  context, for this request will be returned. Otherwise, this will return
  the response as it is.

If you call this function with valid CALLBACK, the given CALLBACK will be
called, when the response of request is being received, with arguments
(CONTEXT-ID RESPONSE-ALIST). RESPONSE-ALIST might be in following form:

  (( ok . t ))

If there's an error, the response will be delivered in following form:

  (( ok . :jason-false ) ( error . \"cant_archive_general\" ))

The value of error might be one of:

  channel_not_found    Value passed for channel was invalid.
  already_archived     Channel has already been archived.
  cant_archive_general You cannot archive the general channel
  last_ra_channel      You cannot archive the last channel for a restricted account
  restricted_action    A team preference prevents the authenticated user from archiving.
  not_authed           No authentication token provided.
  invalid_auth         Invalid authentication token.
  account_inactive     Authentication token is for a deleted user or team.
  user_is_bot          This method cannot be called by a bot user.
  user_is_restricted   This method cannot be called by a restricted user or single channel guest.

If no callback is given, this function will return RESPONSE-ALIST synchronously.
"
  (let ((req-id (slack-utils-id))
	(args (list :token token :channel channel)))
    (slack-http-call-method 'channels.archive args callback req-id)))


(defun slack-rpc-channels-create (token name &optional callback)
  "This method creates a channel.
Web API Desc. @ https://api.slack.com/methods/channels.create

Arguments:
  TOKEN    : Authentication token (Requires scope: post)
  NAME     : Name of channel to create
  CALLBACK : Callback to receive response

Return:
  When callback is a valid function, CONTEXT-ID, the unique id of request
  context, for this request will be returned. Otherwise, this will return
  the response as it is.

If you call this function with valid CALLBACK, the given CALLBACK will be
called, when the response of request is being received, with arguments
(CONTEXT-ID RESPONSE-ALIST). RESPONSE-ALIST might be in following form:

  ((channel (purpose (last_set . 0) (creator . \"\") (value . \"\"))
            (topic (last_set . 0) (creator . \"\") (value . \"\"))
            (members . [\"UXXXXXXXX\"])
            (unread_count_display . 0)
            (unread_count . 0)
            (latest)
            (last_read . \"0000000000.000000\")
            (is_member . t)
            (is_general . :json-false)
            (is_archived . :json-false)
            (creator . \"UXXXXXXXX\") ...)
   (ok . t))

If there's an error, the response will be delivered in following form:

  (( ok . :jason-false ) ( error . \"restricted_action\" ))

The value of error might be one of:

  name_taken         A channel cannot be created with the given name.
  restricted_action  A team preference prevents the authenticated user from creating channels.
  no_channel         Value passed for name was empty.
  not_authed         No authentication token provided.
  invalid_auth       Invalid authentication token.
  account_inactive   Authentication token is for a deleted user or team.
  user_is_bot        This method cannot be called by a bot user.
  user_is_restricted This method cannot be called by a restricted user or single channel guest.

If no callback is given, this function will return RESPONSE-ALIST synchronously."

  (let ((req-id (slack-utils-id))
    (args (list :token token :name name)))
    (slack-http-call-method 'channels.create args callback req-id)))

(defun slack-rpc-channels-history (token channel &optional callback &rest keyword-args)
  "This method fetches history of message and events from a channel.
Web API Desc. @ https://api.slack.com/methods/channels.history

Arguments:
  TOKEN    : Authentication token (Requires scope: post)
  CHANNEL  : Channel to fetch history for
  CALLBACK : Callback to receive response
  KEYWORD-ARGS: Several keywords make sense:
    :latest latest_ts
        End of time range of messages to include in result.
    :oldest oldest_ts
        Start of time range of messages to include in result.
    :inclusive inclusive
        Include messages with latest or oldest timestamp in result (0 or 1)
    :count count
        Number of messages to return, between 1 and 1000.

Return:
  When callback is a valid function, CONTEXT-ID, the unique id of request
  context, for this request will be returned. Otherwise, this will return
  the response as it is.

If you call this function with valid CALLBACK, the given CALLBACK will be
called, when the response of request is being received, with arguments
(CONTEXT-ID RESPONSE-ALIST). RESPONSE-ALIST might be in following form:

  ((is_limited . :json-false)
   (has_more . t)
   (messages . [(...) (...)])
   (ok . t))

If there's an error, the response will be delivered in following form:

  (( ok . :jason-false ) ( error . \"not_authed\" ))

The value of error might be one of:

  channel_not_found  Value passed for channel was invalid.
  invalid_ts_latest  Value passed for latest was invalid
  invalid_ts_oldest  Value passed for oldest was invalid
  not_authed         No authentication token provided.
  invalid_auth       Invalid authentication token.
  account_inactive   Authentication token is for a deleted user or team.

If no callback is given, this function will return RESPONSE-ALIST synchronously."

  (let ((req-id (slack-utils-id))
        (args (append (list :token token :channel channel) keyword-args)))
    (slack-http-call-method 'channels.history args callback req-id)))


(defun slack-rpc-channels-info (token channel &optional callback))

(defun slack-rpc-channels-invite (token channel user &optional callback))

(defun slack-rpc-channels-join (token name &optional callback))

(defun slack-rpc-channels-kick (token channel user &optional callback))

(defun slack-rpc-channels-leave (token channel &optional callback))

(defun slack-rpc-channels-list (token &optional callback exclude-archived))

(defun slack-rpc-channels-mark (token channel ts &optional callback))

(defun slack-rpc-channels-rename (token channel name &optional callback))

(defun slack-rpc-channels-set-purpose (token channel purpose &optional callback))

(defun slack-rpc-channels-unarchive (token channel &optional callback))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chat
(defun slack-rpc-chat-delete (token ts channel &optional callback))

(defun slack-rpc-chat-post-message (token channel text &optional callback &rest keyword-args))
  ;; username as-user parse link-names attachments
  ;; unfurl-links unfurl-media icon-url icon-emoji


(defun slack-rpc-chat-update (token ts channel text &optional callback))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emoji
(defun slack-rpc-emoji-list (token &optional callback))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; files
(defun slack-rpc-files-delete (token file &optional callback))

(defun slack-rpc-files-info (token file &optional callback &rest keyword-args))
  ;;count page))

(defun slack-rpc-files-list (token &optional callback &rest keyword-args))
  ;; user ts-from ts-to types count page))

(defun slack-rpc-files-upload (token &optional callback &rest keyword-args))
;; file content filetype filename title
;;    			     initial-comment channels))

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
(defun slack-rpc-rtm-start (token &optional callback)
"This method starts a Real Time Messaging API session.
Refer to the RTM API documentation for full details on how to use the RTM API.
Web API Desc. @ http://api.slack.com/methods/rtm.start

Arguments:
  TOKEN    : Slack auth token (required scope: client)
  CALLBACK : Callback to handle response.

Return:
  When callback is a valid function, CONTEXT-ID, the unique id of request
  context, for this request will be returned. Otherwise, this will return
  the response as it is.

If you call this function with valid CALLBACK, the given CALLBACK will be
called, when the response of request is being received, with arguments
(CONTEXT-ID RESPONSE-ALIST). RESPONSE-ALIST might be in following form:

  (( ok . t )
   ( url . \"wss:\/\/ms9.slack-msgs.com\/websocket\/7I5yBpcvk\" )
   ( self . ( id . \"U023BECGF\" )
            ( name . \"bobby\" )
            ( prefs . ( ... ) )
            ( created . 1402363766 )
            ( manual_presence . \"active\" ))
   ( team . ( id . \"T024BE7LD\" )
            ( name . \"Example team\" )
            ( email_domain . \"\" )
            ( domain . \"example\" )
            ( msg_edit_window_mins . -1 )
            ( over_storage_limit . :json-false )
            ( prefs . ( ... ) )
            ( plan . \"std\" ))
   ( users . ( ... ))
   ( channels . ( ... ))
   ( groups . ( ... ))
   ( ims . ( ... ))
   ( bots . ( ... )))

The `url' property contains a WebSocket Message Server URL.
Connecting to this URL will initiate a Real Time Messaging session.
These URLs are only valid for 30 seconds, so connect quickly!

The `self' property contains details on the authenticated user.

The `team' property contains details on the authenticated user's team.

The `users' property contains a list of user objects, one for every member of the team.

The `channels' property is a list of channel objects,
one for every channel visible to the authenticated user.
For regular or administrator accounts this list will include every team channel.
The `is_member' property indicates if the user is a member of this channel.
If true then the channel object will also include the topic,
purpose member list and read-state related information.

The `groups' property is a list of group objects,
one for every group the authenticated user is in.

The `ims' property is a list of IM objects,
one for every direct message channel visible to the authenticated user.

The `bots' property gives details of the integrations set up on this team.


If there's an error, the response will be delivered in following form:

  (( ok . :json-false ) ( error . \"not_authed\"))

The value of error might be on of:

  migration_in_progress  Team is being migrated between servers.
                         See the team_migration_started event documentation
                         for details.
  not_authed             No authentication token provided.
  invalid_auth           Invalid authentication token.
  account_inactive       Authentication token is for a deleted user or team.

If no callback is given, this function will return RESPONSE-ALIST synchronously.
"
  (let ((req-id (slack-utils-id))
	(args (list (cons 'token token))))
    (slack-http-call-method 'rtm.start args callback req-id)))


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



(provide 'slack-rpc)
