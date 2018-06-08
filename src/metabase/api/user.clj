(ns metabase.api.user
  "/api/user endpoints"
  (:require [cemerick.friend.credentials :as creds]
            [compojure.core :refer [DELETE GET POST PUT]]
            [metabase.api
             [common :as api]
             [session :as session-api]]
            [metabase.email.messages :as email]
            [metabase.integrations.ldap :as ldap]
            [metabase.models.user :as user :refer [User]]
            [metabase.util :as u]
            [metabase.util.schema :as su]
            [puppetlabs.i18n.core :refer [tru]]
            [schema.core :as s]
            [toucan.db :as db]))

(defn- check-self-or-superuser
  "Check that USER-ID is *current-user-id*` or that `*current-user*` is a superuser, or throw a 403."
  [user-id]
  {:pre [(integer? user-id)]}
  (api/check-403 (or (= user-id api/*current-user-id*)
                     (:is_superuser @api/*current-user*))))

(def ^:private all-user-fields
  (vec (cons User user/all-user-fields)))

(api/defendpoint GET "/"
  "Fetch a list of `Users` for the admin People page. By default returns only active users. If `include_deactivated`
  is true, return all users (active and inactive)."
  [include_deactivated]
  {include_deactivated (s/maybe su/BooleanString)}
  (db/select all-user-fields
    (merge {:order-by [[:%lower.last_name :asc]
                       [:%lower.first_name :asc]]}
           (when-not include_deactivated
             {:where [:= :is_active true]}))))

(defn- fetch-user [& query-criteria]
  (apply db/select-one all-user-fields query-criteria))

(defn- reactivate-user! [existing-user]
  (db/update! User (u/get-id existing-user)
    :is_active     true
    :is_superuser  false
    ;; if the user orignally logged in via Google Auth and it's no longer enabled, convert them into a regular user
    ;; (see Issue #3323)
    :google_auth   (boolean (and (:google_auth existing-user)
                                 ;; if google-auth-client-id is set it means Google Auth is enabled
                                 (session-api/google-auth-client-id)))
    :ldap_auth     (boolean (and (:ldap_auth existing-user)
                                 (ldap/ldap-configured?))))
  ;; now return the existing user whether they were originally active or not
  (fetch-user :id (u/get-id existing-user)))


(api/defendpoint POST "/"
  "Create a new `User`, return a 400 if the email address is already taken"
  [:as {{:keys [first_name last_name email password login_attributes] :as body} :body}]
  {first_name       su/NonBlankString
   last_name        su/NonBlankString
   email            su/Email
   login_attributes (s/maybe user/LoginAttributes)}
  (api/check-superuser)
  (api/checkp (not (db/exists? User :email email))
    "email" (tru "Email address already in use."))
  (let [new-user-id (u/get-id (user/invite-user! (select-keys body [:first_name :last_name :email :password :login_attributes])
                                                 @api/*current-user*))]
    (fetch-user :id new-user-id)))

(api/defendpoint GET "/current"
  "Fetch the current `User`."
  []
  (api/check-404 @api/*current-user*))


(api/defendpoint GET "/:id"
  "Fetch a `User`. You must be fetching yourself *or* be a superuser."
  [id]
  (check-self-or-superuser id)
  (api/check-404 (fetch-user :id id, :is_active true)))

(api/defendpoint PUT "/:id"
  "Update an existing, active `User`."
  [id :as {{:keys [email first_name last_name is_superuser login_attributes] :as body} :body}]
  {email            (s/maybe su/Email)
   first_name       (s/maybe su/NonBlankString)
   last_name        (s/maybe su/NonBlankString)
   login_attributes (s/maybe user/LoginAttributes)}
  (check-self-or-superuser id)
  ;; only allow updates if the specified account is active
  (api/check-404 (db/exists? User, :id id, :is_active true))
  ;; can't change email if it's already taken BY ANOTHER ACCOUNT
  (api/checkp (not (db/exists? User, :email email, :id [:not= id]))
    "email" (tru "Email address already associated to another user."))
  (api/check-500
   (db/update! User id
     (u/select-keys-when body
       :present (when api/*is-superuser?*
                  #{:login_attributes})
       :non-nil (set (concat [:first_name :last_name :email]
                             (when api/*is-superuser?*
                               [:is_superuser]))))))
  (fetch-user :id id))

(api/defendpoint PUT "/:id/reactivate"
  "Reactivate user at `:id`"
  [id]
  (api/check-superuser)
  (let [user (db/select-one [User :id :is_active :google_auth :ldap_auth] :id id)]
    (api/check-404 user)
    ;; Can only reactivate inactive users
    (api/check (not (:is_active user))
      [400 {:message (tru "Not able to reactivate an active user")}])
    (reactivate-user! user)))


(api/defendpoint PUT "/:id/password"
  "Update a user's password."
  [id :as {{:keys [password old_password]} :body}]
  {password su/ComplexPassword}
  (check-self-or-superuser id)
  (api/let-404 [user (db/select-one [User :password_salt :password], :id id, :is_active true)]
    ;; admins are allowed to reset anyone's password (in the admin people list) so no need to check the value of
    ;; `old_password` for them regular users have to know their password, however
    (when-not (:is_superuser @api/*current-user*)
      (api/checkp (creds/bcrypt-verify (str (:password_salt user) old_password) (:password user))
        "old_password"
        (tru "Invalid password"))))
  (user/set-password! id password)
  ;; return the updated User
  (fetch-user :id id))


;; TODO - This could be handled by PUT /api/user/:id, we don't need a separate endpoint
(api/defendpoint PUT "/:id/qbnewb"
  "Indicate that a user has been informed about the vast intricacies of 'the' Query Builder."
  [id]
  (check-self-or-superuser id)
  (api/check-500 (db/update! User id, :is_qbnewb false))
  {:success true})


(api/defendpoint POST "/:id/send_invite"
  "Resend the user invite email for a given user."
  [id]
  (api/check-superuser)
  (when-let [user (User :id id, :is_active true)]
    (let [reset-token (user/set-password-reset-token! id)
          ;; NOTE: the new user join url is just a password reset with an indicator that this is a first time user
          join-url    (str (user/form-password-reset-url reset-token) "#new")]
      (email/send-new-user-email! user @api/*current-user* join-url))))


(api/defendpoint DELETE "/:id"
  "Disable a `User`.  This does not remove the `User` from the DB, but instead disables their account."
  [id]
  (api/check-superuser)
  (api/check-500 (db/update! User id, :is_active false))
  {:success true})


(api/define-routes)
