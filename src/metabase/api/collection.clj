(ns metabase.api.collection
  "/api/collection endpoints."
  (:require [compojure.core :refer [GET POST PUT]]
            [metabase.api
             [card :as card-api]
             [common :as api]]
            [metabase.models
             [card :refer [Card]]
             [dashboard :refer [Dashboard]]
             [collection :as collection :refer [Collection]]
             [permissions :as perms]
             [interface :as mi]
             [pulse :as pulse :refer [Pulse]]]
            [metabase.util.schema :as su]
            [puppetlabs.i18n.core :refer [tru]]
            [schema.core :as s]
            [toucan
             [db :as db]
             [hydrate :refer [hydrate]]]
            [metabase.util :as u]))

(api/defendpoint GET "/"
  "Fetch a list of all Collections that the current user has read permissions for.
  This includes `:can_write`, which means whether the current user is allowed to add or remove Cards to this
  Collection; keep in mind that regardless of this status you must be a superuser to modify properties of Collections
  themselves.

  By default, this returns non-archived Collections, but instead you can show archived ones by passing
  `?archived=true`."
  [archived]
  {archived (s/maybe su/BooleanString)}
  (as-> (db/select Collection :archived (Boolean/parseBoolean archived)
                   {:order-by [[:%lower.name :asc]]}) collections
    (filter mi/can-read? collections)
    (hydrate collections :can_write)))


;;; --------------------------------- Fetching a single Collection & its 'children' ----------------------------------

(def ^:private model->collection-children-fn
  "Functions for fetching the 'children' of a Collection. Each function takes `collection-id` as a param."
  {:cards      #(db/select [Card :name :id],      :collection_id %, :archived false)
   :dashboards #(db/select [Dashboard :name :id], :collection_id %, :archived false)
   :pulses     #(db/select [Pulse :name :id],     :collection_id %)})

(defn- collection-children
  "Fetch a map of the 'child' objects belonging to a Collection of type `model`, or of all available types if `model` is
  `nil`.

      (collection-children :cards model->collection-children-fn 1)
      ;; -> {:cards [...cards for Collection 1...]}

      (collection-children nil model->collection-children-fn  1)
      ;; -> {:cards [...], :dashboards [...], :pulses [...]}"
  [model collection-id]
  (into {} (for [[a-model children-fn] model->collection-children-fn
                 ;; only fetch models that are specified by the `model` param; or everything if it's `nil`
                 :when (or (nil? model)
                           (= (name model) (name a-model)))]
             ;; return the results like {:card <results-of-card-children-fn>}
             {a-model (children-fn collection-id)})))

(api/defendpoint GET "/:id"
  "Fetch a specific (non-archived) Collection, including objects of a specific `model` that belong to it. If `model` is
  unspecified, it will return objects of all types."
  [id model]
  {model (s/maybe (s/enum "cards" "dashboards" "pulses"))}
  (merge
   (-> (api/read-check Collection id, :archived false)
       (hydrate :effective_location :effective_children :effective_ancestors))
   (collection-children model id)))


(defn- current-user-has-root-collection-read-perms? []
  (perms/set-has-full-permissions? @api/*current-user-permissions-set*
    (perms/collection-read-path collection/root-collection)))

(api/defendpoint GET "/root"
  "Fetch objects that the current user should see at their root level. As mentioned elsewhere, the 'Root' Collection
  doesn't actually exist as a row in the application DB: it's simply a virtual Collection where things with no
  `collection_id` exist. It does, however, have its own set of Permissions.

  This endpoint will actually show objects with no `collection_id` for Users that have Root Collection
  permissions, but for people without Root Collection perms, we'll just show the objects that have an effective
  location of `/`.

  This endpoint is intended to power a 'Root Folder View' for the Current User, so regardless you'll see all the
  top-level objects you're allowed to access."
  [model]
  {model (s/maybe (s/enum "cards" "dashboards" "pulses"))}
  (merge
   {:name                (tru "Root Collection")
    :id                  "root"
    :effective_location  nil
    :effective_ancestors []
    ;; anybody gets to see other Collections that have an Effective Location of being in the Root Collection.
    :effective_children  (collection/effective-children collection/root-collection)}
   ;; Only people with Root Collection Read Permissions get to see objects that have no `collection_id`.
   (if (current-user-has-root-collection-read-perms?)
     (collection-children model nil)
     ;; for people who can't see the loose items in the Root Collection just return empty arrays to avoid confusion
     {:cards      []
      :dashboards []
      :pulses     []})))


;;; ----------------------------------------- Creating/Editing a Collection ------------------------------------------

(api/defendpoint POST "/"
  "Create a new Collection."
  [:as {{:keys [name color description location]} :body}]
  {name        su/NonBlankString
   color       collection/hex-color-regex
   description (s/maybe su/NonBlankString)
   location    (s/maybe collection/LocationPath)}
  ;; PERMS CHECKS: For the time being, you must be a superuser to create a new Collection. If we want to change that
  ;; in the future, we need to add a check for `location` -- if you're going to set it, we need to check that you have
  ;; write perms for the parent Collection.
  (api/check-superuser)
  ;; Now create the new Collection :)
  (db/insert! Collection
    (merge
     {:name        name
      :color       color
      :description description}
     (when location
       {:location location}))))

(api/defendpoint PUT "/:id"
  "Modify an existing Collection, including archiving or unarchiving it."
  [id, :as {{:keys [name color description archived location], :as body} :body}]
  {name        (s/maybe su/NonBlankString)
   color       (s/maybe collection/hex-color-regex)
   description (s/maybe su/NonBlankString)
   archived    (s/maybe s/Bool)
   location    (s/maybe collection/LocationPath)}
  ;; You have to be a superuser to modify a Collection itself, but `/collection/:id/` perms are sufficient for
  ;; adding/removing Cards. As with creating a new Collection, since we require superuser status we don't need to do
  ;; futher perms checks if you're going to set `location`, but if we change how Collection perms work in the future,
  ;; we'll need to add appropriate write perms checks for editing the parent Collection
  (api/check-superuser)
  ;; Check and see if this Collection exists, or throw a 404
  (api/api-let [404 "Not Found"] [collection-before-update (Collection id)]
    ;; ok, go ahead and update it! Only update keys that were specified in the `body`
    (db/update! Collection id
      (u/select-keys-when body :present [:name :color :description :archived :location]))
    ;; Check and see if if the Collection is switiching to archived
    (when (and (not (:archived collection-before-update))
               archived)
      (when-let [alerts (seq (apply pulse/retrieve-alerts-for-cards (db/select-ids Card, :collection_id id)))]
        ;; When a collection is archived, all of it's cards are also marked as archived, but this is down in the model
        ;; layer which will not cause the archive notification code to fire. This will delete the relevant alerts and
        ;; notify the users just as if they had be archived individually via the card API
        (card-api/delete-alert-and-notify-archived! alerts))))
  ;; return the updated object
  (Collection id))


;;; ------------------------------------------------ GRAPH ENDPOINTS -------------------------------------------------

(api/defendpoint GET "/graph"
  "Fetch a graph of all Collection Permissions."
  []
  (api/check-superuser)
  (collection/graph))


(defn- ->int [id] (Integer/parseInt (name id)))

(defn- dejsonify-collections [collections]
  (into {} (for [[collection-id perms] collections]
             {(->int collection-id) (keyword perms)})))

(defn- dejsonify-groups [groups]
  (into {} (for [[group-id collections] groups]
             {(->int group-id) (dejsonify-collections collections)})))

(defn- dejsonify-graph
  "Fix the types in the graph when it comes in from the API, e.g. converting things like `\"none\"` to `:none` and
  parsing object keys as integers."
  [graph]
  (update graph :groups dejsonify-groups))

(api/defendpoint PUT "/graph"
  "Do a batch update of Collections Permissions by passing in a modified graph."
  [:as {body :body}]
  {body su/Map}
  (api/check-superuser)
  (collection/update-graph! (dejsonify-graph body))
  (collection/graph))


(api/define-routes)
