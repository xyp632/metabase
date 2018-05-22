(ns metabase.api.card-test
  "Tests for /api/card endpoints."
  (:require [cheshire.core :as json]
            [dk.ative.docjure.spreadsheet :as spreadsheet]
            [expectations :refer :all]
            [medley.core :as m]
            [metabase
             [email-test :as et]
             [http-client :as http :refer :all]
             [middleware :as middleware]
             [util :as u]]
            [metabase.models
             [card :refer [Card]]
             [card-favorite :refer [CardFavorite]]
             [card-label :refer [CardLabel]]
             [collection :refer [Collection]]
             [database :refer [Database]]
             [label :refer [Label]]
             [permissions :as perms]
             [permissions-group :as perms-group]
             [pulse :as pulse :refer [Pulse]]
             [pulse-card :refer [PulseCard]]
             [pulse-channel :refer [PulseChannel]]
             [pulse-channel-recipient :refer [PulseChannelRecipient]]
             [table :refer [Table]]
             [view-log :refer [ViewLog]]]
            [metabase.query-processor.middleware.results-metadata :as results-metadata]
            [metabase.test
             [data :as data]
             [util :as tu :refer [match-$ random-name]]]
            [metabase.test.data.users :refer :all]
            [metabase.util.date :as du]
            [toucan.db :as db]
            [toucan.util.test :as tt])
  (:import java.io.ByteArrayInputStream
           java.util.UUID))

;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                              Helper Fns & Macros                                               |
;;; +----------------------------------------------------------------------------------------------------------------+

(def card-defaults
  {:archived            false
   :collection_id       nil
   :collection_position nil
   :description         nil
   :display             "scalar"
   :enable_embedding    false
   :embedding_params    nil
   :made_public_by_id   nil
   :public_uuid         nil
   :query_type          "query"
   :cache_ttl           nil
   :result_metadata     nil})

(defn- mbql-count-query
  ([]
   (mbql-count-query (data/id) (data/id :venues)))
  ([db-or-id table-or-id]
   {:database (u/get-id db-or-id)
    :type     "query"
    :query    {:source-table (u/get-id table-or-id), :aggregation {:aggregation-type "count"}}}))

(defn- card-with-name-and-query
  ([]
   (card-with-name-and-query (tu/random-name)))
  ([card-name]
   (card-with-name-and-query card-name (mbql-count-query)))
  ([card-name query]
   {:name                   card-name
    :display                "scalar"
    :dataset_query          query
    :visualization_settings {:global {:title nil}}}))


(defn- do-with-temp-native-card
  {:style/indent 0}
  [f]
  (tt/with-temp* [Database   [db    {:details (:details (Database (data/id))), :engine :h2}]
                  Table      [table {:db_id (u/get-id db), :name "CATEGORIES"}]
                  Card       [card  {:dataset_query {:database (u/get-id db)
                                                     :type     :native
                                                     :native   {:query "SELECT COUNT(*) FROM CATEGORIES;"}}}]]
    (f db card)))

(defmacro ^:private with-temp-native-card
  {:style/indent 1}
  [[db-binding card-binding] & body]
  `(do-with-temp-native-card (fn [~(or db-binding '_) ~(or card-binding '_)]
                               ~@body)))


(defn do-with-cards-in-a-collection [card-or-cards-or-ids grant-perms-fn! f]
  (tt/with-temp Collection [collection]
    ;; put all the Card(s) in our temp `collection`
    (doseq [card-or-id (if (sequential? card-or-cards-or-ids)
                         card-or-cards-or-ids
                         [card-or-cards-or-ids])]
      (db/update! Card (u/get-id card-or-id) {:collection_id (u/get-id collection)}))
    ;; now use `grant-perms-fn!` to grant appropriate perms
    (grant-perms-fn! (perms-group/all-users) collection)
    ;; call (f)
    (f)))

(defmacro with-cards-in-readable-collection
  "Execute `body` with `card-or-cards-or-ids` added to a temporary Collection that All Users have read permissions for."
  {:style/indent 1}
  [card-or-cards-or-ids & body]
  `(do-with-cards-in-a-collection ~card-or-cards-or-ids perms/grant-collection-read-permissions! (fn [] ~@body)))

(defmacro with-cards-in-writeable-collection
  "Execute `body` with `card-or-cards-or-ids` added to a temporary Collection that All Users have *write* permissions
  for."
  {:style/indent 1}
  [card-or-cards-or-ids & body]
  `(do-with-cards-in-a-collection ~card-or-cards-or-ids perms/grant-collection-readwrite-permissions! (fn [] ~@body)))


(defn- do-with-temp-native-card-with-params {:style/indent 0} [f]
  (tt/with-temp*
    [Database   [db    {:details (:details (Database (data/id))), :engine :h2}]
     Table      [table {:db_id (u/get-id db), :name "VENUES"}]
     Card       [card  {:dataset_query
                        {:database (u/get-id db)
                         :type     :native
                         :native   {:query         "SELECT COUNT(*) FROM VENUES WHERE CATEGORY_ID = {{category}};"
                                    :template_tags {:category {:id           "a9001580-3bcc-b827-ce26-1dbc82429163"
                                                               :name         "category"
                                                               :display_name "Category"
                                                               :type         "number"
                                                               :required     true}}}}}]]
    (f db card)))

(defmacro ^:private with-temp-native-card-with-params {:style/indent 1} [[db-binding card-binding] & body]
  `(do-with-temp-native-card-with-params (fn [~(or db-binding '_) ~(or card-binding '_)] ~@body)))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                           FETCHING CARDS & FILTERING                                           |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn- card-returned? [model object-or-id card-or-id]
  (contains? (set (for [card ((user->client :rasta) :get 200 "card", :f model, :model_id (u/get-id object-or-id))]
                    (u/get-id card)))
             (u/get-id card-or-id)))

;; Filter cards by database
(expect
  {1 true
   2 false
   3 true}
  (tt/with-temp* [Database [db]
                  Card     [card-1 {:database_id (data/id)}]
                  Card     [card-2 {:database_id (u/get-id db)}]]
    (with-cards-in-readable-collection [card-1 card-2]
      (array-map
       1 (card-returned? :database (data/id) card-1)
       2 (card-returned? :database db        card-1)
       3 (card-returned? :database db        card-2)))))


(expect (get middleware/response-unauthentic :body) (http/client :get 401 "card"))
(expect (get middleware/response-unauthentic :body) (http/client :put 401 "card/13"))


;; Make sure `model_id` is required when `f` is :database
(expect {:errors {:model_id "model_id is a required parameter when filter mode is 'database'"}}
  ((user->client :crowberto) :get 400 "card" :f :database))

;; Filter cards by table
(expect
  {1 true
   2 false
   3 true}
  (tt/with-temp* [Database [db]
                  Table    [table-1  {:db_id (u/get-id db)}]
                  Table    [table-2  {:db_id (u/get-id db)}]
                  Card     [card-1   {:table_id (u/get-id table-1)}]
                  Card     [card-2   {:table_id (u/get-id table-2)}]]
    (with-cards-in-readable-collection [card-1 card-2]
      (array-map
       1 (card-returned? :table (u/get-id table-1) (u/get-id card-1))
       2 (card-returned? :table (u/get-id table-2) (u/get-id card-1))
       3 (card-returned? :table (u/get-id table-2) (u/get-id card-2))))))

;; Make sure `model_id` is required when `f` is :table
(expect
  {:errors {:model_id "model_id is a required parameter when filter mode is 'table'"}}
  ((user->client :crowberto) :get 400 "card", :f :table))


;;; Filter by `recent`
;; Should return cards that were recently viewed by current user only
(expect
  ["Card 3"
   "Card 4"
   "Card 1"]
  (tt/with-temp* [Card    [card-1 {:name "Card 1"}]
                  Card    [card-2 {:name "Card 2"}]
                  Card    [card-3 {:name "Card 3"}]
                  Card    [card-4 {:name "Card 4"}]
                  ;; 3 was viewed most recently, followed by 4, then 1. Card 2 was viewed by a different user so
                  ;; shouldn't be returned
                  ViewLog [_ {:model "card", :model_id (u/get-id card-1), :user_id (user->id :rasta)
                              :timestamp (du/->Timestamp "2015-12-01")}]
                  ViewLog [_ {:model "card", :model_id (u/get-id card-2), :user_id (user->id :trashbird)
                              :timestamp (du/->Timestamp "2016-01-01")}]
                  ViewLog [_ {:model "card", :model_id (u/get-id card-3), :user_id (user->id :rasta)
                              :timestamp (du/->Timestamp "2016-02-01")}]
                  ViewLog [_ {:model "card", :model_id (u/get-id card-4), :user_id (user->id :rasta)
                              :timestamp (du/->Timestamp "2016-03-01")}]
                  ViewLog [_ {:model "card", :model_id (u/get-id card-3), :user_id (user->id :rasta)
                              :timestamp (du/->Timestamp "2016-04-01")}]]
    (with-cards-in-readable-collection [card-1 card-2 card-3 card-4]
      (map :name ((user->client :rasta) :get 200 "card", :f :recent)))))

;;; Filter by `popular`
;; `f=popular` should return cards sorted by number of ViewLog entries for all users; cards with no entries should be
;; excluded
(expect
  ["Card 3"
   "Card 2"]
  (tt/with-temp* [Card     [card-1 {:name "Card 1"}]
                  Card     [card-2 {:name "Card 2"}]
                  Card     [card-3 {:name "Card 3"}]
                  ;; 3 entries for card 3, 2 for card 2, none for card 1,
                  ViewLog  [_ {:model "card", :model_id (u/get-id card-3), :user_id (user->id :rasta)}]
                  ViewLog  [_ {:model "card", :model_id (u/get-id card-2), :user_id (user->id :trashbird)}]
                  ViewLog  [_ {:model "card", :model_id (u/get-id card-2), :user_id (user->id :rasta)}]
                  ViewLog  [_ {:model "card", :model_id (u/get-id card-3), :user_id (user->id :crowberto)}]
                  ViewLog  [_ {:model "card", :model_id (u/get-id card-3), :user_id (user->id :rasta)}]]
    (with-cards-in-readable-collection [card-1 card-2 card-3]
      (map :name ((user->client :rasta) :get 200 "card", :f :popular)))))

;;; Filter by `archived`
;; check that the set of Card IDs returned with f=archived is equal to the set of archived cards
(expect
  #{"Card 2" "Card 3"}
  (tt/with-temp* [Card [card-1 {:name "Card 1"}]
                  Card [card-2 {:name "Card 2", :archived true}]
                  Card [card-3 {:name "Card 3", :archived true}]]
    (with-cards-in-readable-collection [card-1 card-2 card-3]
      (set (map :name ((user->client :rasta) :get 200 "card", :f :archived))))))

;;; Filter by `fav`
(expect
  [{:name "Card 1", :favorite true}]
  (tt/with-temp* [Card         [card-1 {:name "Card 1"}]
                  Card         [card-2 {:name "Card 2"}]
                  Card         [card-3 {:name "Card 3"}]
                  CardFavorite [_ {:card_id (u/get-id card-1), :owner_id (user->id :rasta)}]
                  CardFavorite [_ {:card_id (u/get-id card-2), :owner_id (user->id :crowberto)}]]
    (with-cards-in-readable-collection [card-1 card-2 card-3]
      (for [card ((user->client :rasta) :get 200 "card", :f :fav)]
        (select-keys card [:name :favorite])))))

;;; Filter by labels
(expect
  ["Card 2"]
  (tt/with-temp* [Card      [card-1 {:name "Card 1"}]
                  Card      [card-2 {:name "Card 2"}]
                  Label     [label-1 {:name "Toucans"}] ; slug will be `toucans`
                  Label     [label-2 {:name "More Toucans"}] ; slug will be `more_toucans`
                  CardLabel [_ {:card_id (u/get-id card-1), :label_id (u/get-id label-1)}]
                  CardLabel [_ {:card_id (u/get-id card-2), :label_id (u/get-id label-2)}]]
    (with-cards-in-readable-collection [card-1 card-2]
      ;; When filtering by `more_toucans` only the second Card should get returned
      (map :name ((user->client :rasta) :get 200 "card", :label "more_toucans"))))) ; filtering is done by slug


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                CREATING A CARD                                                 |
;;; +----------------------------------------------------------------------------------------------------------------+

;; Test that we can make a card
(let [card-name (random-name)]
  (tt/expect-with-temp [Database   [db]
                        Table      [table {:db_id (u/get-id db)}]
                        Collection [collection]]
    (merge card-defaults
           {:name                   card-name
            :collection_id          (u/get-id collection)
            :collection             collection
            :creator_id             (user->id :rasta)
            :dataset_query          (mbql-count-query (u/get-id db) (u/get-id table))
            :visualization_settings {:global {:title nil}}
            :database_id            (u/get-id db) ; these should be inferred automatically
            :table_id               (u/get-id table)
            :labels                 []
            :can_write              true
            :dashboard_count        0
            :read_permissions       nil
            :creator                (match-$ (fetch-user :rasta)
                                      {:common_name  "Rasta Toucan"
                                       :is_superuser false
                                       :is_qbnewb    true
                                       :last_login   $
                                       :last_name    "Toucan"
                                       :first_name   "Rasta"
                                       :date_joined  $
                                       :email        "rasta@metabase.com"
                                       :id           $})})
    (tu/with-model-cleanup [Card]
      (perms/grant-collection-readwrite-permissions! (perms-group/all-users) collection)
      (-> ((user->client :rasta) :post 200 "card"
           (assoc (card-with-name-and-query card-name (mbql-count-query (u/get-id db) (u/get-id table)))
             :collection_id (u/get-id collection)))
          (dissoc :created_at :updated_at :id)))))

;; Make sure when saving a Card the query metadata is saved (if correct)
(expect
  [{:base_type    "type/Integer"
    :display_name "Count Chocula"
    :name         "count_chocula"
    :special_type "type/Number"}]
  (let [metadata [{:base_type    :type/Integer
                   :display_name "Count Chocula"
                   :name         "count_chocula"
                   :special_type :type/Number}]
        card-name (tu/random-name)]
    (tu/with-model-cleanup [Card]
      ;; create a card with the metadata
      ((user->client :rasta) :post 200 "card"
       (assoc (card-with-name-and-query card-name)
         :result_metadata    metadata
         :metadata_checksum  (#'results-metadata/metadata-checksum metadata)))
      ;; now check the metadata that was saved in the DB
      (db/select-one-field :result_metadata Card :name card-name))))

;; make sure when saving a Card the correct query metadata is fetched (if incorrect)
(expect
  [{:base_type    "type/Integer"
    :display_name "count"
    :name         "count"
    :special_type "type/Number"}]
  (let [metadata [{:base_type    :type/Integer
                   :display_name "Count Chocula"
                   :name         "count_chocula"
                   :special_type :type/Number}]
        card-name (tu/random-name)]
    (tt/with-temp Collection [collection]
      (perms/grant-collection-readwrite-permissions! (perms-group/all-users) collection)
      (tu/with-model-cleanup [Card]
        ;; create a card with the metadata
        ((user->client :rasta) :post 200 "card"
         (assoc (card-with-name-and-query card-name)
           :collection_id      (u/get-id collection)
           :result_metadata    metadata
           :metadata_checksum  "ABCDEF")) ; bad checksum
        ;; now check the correct metadata was fetched and was saved in the DB
        (db/select-one-field :result_metadata Card :name card-name)))))

;; Make sure we can create a Card with a Collection position
(expect
  #metabase.models.card.CardInstance{:collection_id true, :collection_position 1}
  (tu/with-model-cleanup [Card]
    (let [card-name (tu/random-name)]
      (tt/with-temp Collection [collection]
        (perms/grant-collection-readwrite-permissions! (perms-group/all-users) collection)
        ((user->client :rasta) :post 200 "card" (assoc (card-with-name-and-query card-name)
                                                  :collection_id (u/get-id collection), :collection_position 1))
        (some-> (db/select-one [Card :collection_id :collection_position] :name card-name)
                (update :collection_id (partial = (u/get-id collection))))))))

;; ...but not if we don't have permissions for the Collection
(expect
  nil
  (tu/with-model-cleanup [Card]
    (let [card-name (tu/random-name)]
      (tt/with-temp Collection [collection]
        ((user->client :rasta) :post 403 "card" (assoc (card-with-name-and-query card-name)
                                                  :collection_id (u/get-id collection), :collection_position 1))
        (some-> (db/select-one [Card :collection_id :collection_position] :name card-name)
                (update :collection_id (partial = (u/get-id collection))))))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                            FETCHING A SPECIFIC CARD                                            |
;;; +----------------------------------------------------------------------------------------------------------------+

;; Test that we can fetch a card
(tt/expect-with-temp [Database   [db]
                      Table      [table {:db_id (u/get-id db)}]
                      Collection [collection]
                      Card       [card  {:collection_id (u/get-id collection)
                                         :dataset_query (mbql-count-query (u/get-id db) (u/get-id table))}]]
  (merge card-defaults
         (match-$ card
           {:dashboard_count        0
            :name                   $
            :creator_id             (user->id :rasta)
            :creator                (match-$ (fetch-user :rasta)
                                      {:common_name  "Rasta Toucan"
                                       :is_superuser false
                                       :is_qbnewb    true
                                       :last_login   $
                                       :last_name    "Toucan"
                                       :first_name   "Rasta"
                                       :date_joined  $
                                       :email        "rasta@metabase.com"
                                       :id           $})
            :updated_at             $
            :dataset_query          $
            :read_permissions       nil
            :id                     $
            :display                "table"
            :visualization_settings {}
            :can_write              false
            :created_at             $
            :database_id            (u/get-id db) ; these should be inferred from the dataset_query
            :table_id               (u/get-id table)
            :collection_id          (u/get-id collection)
            :collection             collection
            :labels                 []}))
  (do
    (perms/grant-collection-read-permissions! (perms-group/all-users) collection)
    ((user->client :rasta) :get 200 (str "card/" (u/get-id card)))))

;; Check that a user without permissions isn't allowed to fetch the card
(expect
  "You don't have permissions to do that."
  (tt/with-temp* [Database [db]
                  Table    [table    {:db_id (u/get-id db)}]
                  Card     [card              {:dataset_query (mbql-count-query (u/get-id db) (u/get-id table))}]]
    ;; revoke permissions for default group to this database
    (perms/delete-related-permissions! (perms-group/all-users) (perms/object-path (u/get-id db)))
    ;; now a non-admin user shouldn't be able to fetch this card
    ((user->client :rasta) :get 403 (str "card/" (u/get-id card)))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                UPDATING A CARD                                                 |
;;; +----------------------------------------------------------------------------------------------------------------+

;; updating a card that doesn't exist should give a 404
(expect "Not found."
  ((user->client :crowberto) :put 404 "card/12345"))

;; Test that we can edit a Card
(expect
  {1 "Original Name"
   2 "Updated Name"}
  (tt/with-temp Card [card {:name "Original Name"}]
    (with-cards-in-writeable-collection card
      (array-map
       1 (db/select-one-field :name Card, :id (u/get-id card))
       2 (do ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:name "Updated Name"})
             (db/select-one-field :name Card, :id (u/get-id card)))))))

;; Can we update a Card's archived status?
(expect
  {1 false
   2 true
   3 false}
  (tt/with-temp Card [card]
    (with-cards-in-writeable-collection card
      (let [archived?     (fn [] (:archived (Card (u/get-id card))))
            set-archived! (fn [archived]
                            ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:archived archived})
                            (archived?))]
        (array-map
         1 (archived?)
         2 (set-archived! true)
         3 (set-archived! false))))))

;; we shouldn't be able to update archived status if we don't have collection *write* perms
(expect
  "You don't have permissions to do that."
  (tt/with-temp* [Collection [collection]
                  Card       [card {:collection_id (u/get-id collection)}]]
    (perms/grant-collection-read-permissions! (perms-group/all-users) collection)
    ((user->client :rasta) :put 403 (str "card/" (u/get-id card)) {:archived true})))

;; Can we clear the description of a Card? (#4738)
(expect
  nil
  (tt/with-temp Card [card {:description "What a nice Card"}]
    (with-cards-in-writeable-collection card
      ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:description nil})
      (db/select-one-field :description Card :id (u/get-id card)))))

;; description should be blankable as well
(expect
  ""
  (tt/with-temp Card [card {:description "What a nice Card"}]
    (with-cards-in-writeable-collection card
      ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:description ""})
      (db/select-one-field :description Card :id (u/get-id card)))))

;; Can we update a card's embedding_params?
(expect
  {:abc "enabled"}
  (tt/with-temp Card [card]
    (tu/with-temporary-setting-values [enable-embedding true]
      ((user->client :crowberto) :put 200 (str "card/" (u/get-id card)) {:embedding_params {:abc "enabled"}}))
    (db/select-one-field :embedding_params Card :id (u/get-id card))))

;; We shouldn't be able to update them if we're not an admin...
(expect
  "You don't have permissions to do that."
  (tt/with-temp Card [card]
    (tu/with-temporary-setting-values [enable-embedding true]
      ((user->client :rasta) :put 403 (str "card/" (u/get-id card)) {:embedding_params {:abc "enabled"}}))))

;; ...or if embedding isn't enabled
(expect
  "Embedding is not enabled."
  (tt/with-temp Card [card]
    (tu/with-temporary-setting-values [enable-embedding false]
      ((user->client :crowberto) :put 400 (str "card/" (u/get-id card)) {:embedding_params {:abc "enabled"}}))))

;; make sure when updating a Card the query metadata is saved (if correct)
(expect
  [{:base_type    "type/Integer"
    :display_name "Count Chocula"
    :name         "count_chocula"
    :special_type "type/Number"}]
  (let [metadata [{:base_type    :type/Integer
                   :display_name "Count Chocula"
                   :name         "count_chocula"
                   :special_type :type/Number}]]
    (tt/with-temp Card [card]
      (with-cards-in-writeable-collection card
        ;; update the Card's query
        ((user->client :rasta) :put 200 (str "card/" (u/get-id card))
         {:dataset_query     (mbql-count-query)
          :result_metadata   metadata
          :metadata_checksum (#'results-metadata/metadata-checksum metadata)})
        ;; now check the metadata that was saved in the DB
        (db/select-one-field :result_metadata Card :id (u/get-id card))))))

;; Make sure when updating a Card the correct query metadata is fetched (if incorrect)
(expect
  [{:base_type    "type/Integer"
    :display_name "count"
    :name         "count"
    :special_type "type/Number"}]
  (let [metadata [{:base_type    :type/Integer
                   :display_name "Count Chocula"
                   :name         "count_chocula"
                   :special_type :type/Number}]]
    (tt/with-temp Card [card]
      (with-cards-in-writeable-collection card
        ;; update the Card's query
        ((user->client :rasta) :put 200 (str "card/" (u/get-id card))
         {:dataset_query     (mbql-count-query)
          :result_metadata   metadata
          :metadata_checksum "ABC123"}) ; invalid checksum
        ;; now check the metadata that was saved in the DB
        (db/select-one-field :result_metadata Card :id (u/get-id card))))))

;; Can we change the Collection position of a Card?
(expect
  1
  (tt/with-temp Card [card]
    (with-cards-in-writeable-collection card
      ((user->client :rasta) :put 200 (str "card/" (u/get-id card))
       {:collection_position 1})
      (db/select-one-field :collection_position Card :id (u/get-id card)))))

;; ...and unset (unpin) it as well?
(expect
  nil
  (tt/with-temp Card [card {:collection_position 1}]
    (with-cards-in-writeable-collection card
      ((user->client :rasta) :put 200 (str "card/" (u/get-id card))
       {:collection_position nil})
      (db/select-one-field :collection_position Card :id (u/get-id card)))))

;; ...we shouldn't be able to if we don't have permissions for the Collection
(expect
  nil
  (tt/with-temp* [Collection [collection]
                  Card       [card {:collection_id (u/get-id collection)}]]
    ((user->client :rasta) :put 403 (str "card/" (u/get-id card))
     {:collection_position 1})
    (db/select-one-field :collection_position Card :id (u/get-id card))))

(expect
  1
  (tt/with-temp* [Collection [collection]
                  Card       [card {:collection_id (u/get-id collection), :collection_position 1}]]
    ((user->client :rasta) :put 403 (str "card/" (u/get-id card))
     {:collection_position nil})
    (db/select-one-field :collection_position Card :id (u/get-id card))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                        Card updates that impact alerts                                         |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn- rasta-alert-not-working [body-map]
  (et/email-to :rasta {:subject "One of your alerts has stopped working",
                       :body body-map}))

(defn- crowberto-alert-not-working [body-map]
  (et/email-to :crowberto {:subject "One of your alerts has stopped working",
                           :body body-map}))

;; Validate archiving a card trigers alert deletion
(expect
  {1 (merge (crowberto-alert-not-working {"the question was archived by Rasta Toucan" true})
            (rasta-alert-not-working {"the question was archived by Rasta Toucan" true}))
   2 nil}
  (tt/with-temp* [Card                  [card]
                  Pulse                 [pulse {:alert_condition  "rows"
                                                :alert_first_only false
                                                :creator_id       (user->id :rasta)
                                                :name             "Original Alert Name"}]

                  PulseCard             [_     {:pulse_id (u/get-id pulse)
                                                :card_id  (u/get-id card)
                                                :position 0}]
                  PulseChannel          [pc    {:pulse_id (u/get-id pulse)}]
                  PulseChannelRecipient [_     {:user_id          (user->id :crowberto)
                                                :pulse_channel_id (u/get-id pc)}]
                  PulseChannelRecipient [_     {:user_id          (user->id :rasta)
                                                :pulse_channel_id (u/get-id pc)}]]
    (with-cards-in-writeable-collection card
      (et/with-fake-inbox
        (et/with-expected-messages 2
          ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:archived true}))
        (array-map
         1 (et/regex-email-bodies #"the question was archived by Rasta Toucan")
         2 (Pulse (u/get-id pulse)))))))

;; Validate changing a display type trigers alert deletion
(expect
  {1 (merge (crowberto-alert-not-working {"the question was edited by Rasta Toucan" true})
            (rasta-alert-not-working {"the question was edited by Rasta Toucan" true}))

   2 nil}
  (tt/with-temp* [Card                  [card  {:display :table}]
                  Pulse                 [pulse {:alert_condition  "rows"
                                                :alert_first_only false
                                                :creator_id       (user->id :rasta)
                                                :name             "Original Alert Name"}]

                  PulseCard             [_     {:pulse_id (u/get-id pulse)
                                                :card_id  (u/get-id card)
                                                :position 0}]
                  PulseChannel          [pc    {:pulse_id (u/get-id pulse)}]
                  PulseChannelRecipient [_     {:user_id          (user->id :crowberto)
                                                :pulse_channel_id (u/get-id pc)}]
                  PulseChannelRecipient [_     {:user_id          (user->id :rasta)
                                                :pulse_channel_id (u/get-id pc)}]]
    (with-cards-in-writeable-collection card
      (et/with-fake-inbox
        (et/with-expected-messages 2
          ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:display :line}))
        (array-map
         1 (et/regex-email-bodies #"the question was edited by Rasta Toucan")
         2 (Pulse (u/get-id pulse)))))))

;; Changing the display type from line to table should force a delete
(expect
  {1 (rasta-alert-not-working {"the question was edited by Rasta Toucan" true})
   2 nil}
  (tt/with-temp* [Card                  [card  {:display                :line
                                                :visualization_settings {:graph.goal_value 10}}]
                  Pulse                 [pulse {:alert_condition  "goal"
                                                :alert_first_only false
                                                :creator_id       (user->id :rasta)
                                                :name             "Original Alert Name"}]
                  PulseCard             [_     {:pulse_id (u/get-id pulse)
                                                :card_id  (u/get-id card)
                                                :position 0}]
                  PulseChannel          [pc    {:pulse_id (u/get-id pulse)}]
                  PulseChannelRecipient [_     {:user_id          (user->id :rasta)
                                                :pulse_channel_id (u/get-id pc)}]]
    (with-cards-in-writeable-collection card
      (et/with-fake-inbox
        (et/with-expected-messages 1
          ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:display :table}))
        (array-map
         1 (et/regex-email-bodies #"the question was edited by Rasta Toucan")
         2 (Pulse (u/get-id pulse)))))))

;; Changing the display type from line to area/bar is fine and doesn't delete the alert
(expect
  {1 {}
   2 true
   3 {}
   4 true}
  (tt/with-temp* [Card                  [card  {:display                :line
                                                :visualization_settings {:graph.goal_value 10}}]
                  Pulse                 [pulse {:alert_condition  "goal"
                                                :alert_first_only false
                                                :creator_id       (user->id :rasta)
                                                :name             "Original Alert Name"}]
                  PulseCard             [_     {:pulse_id (u/get-id pulse)
                                                :card_id  (u/get-id card)
                                                :position 0}]
                  PulseChannel          [pc    {:pulse_id (u/get-id pulse)}]
                  PulseChannelRecipient [_     {:user_id          (user->id :rasta)
                                                :pulse_channel_id (u/get-id pc)}]]
    (with-cards-in-writeable-collection card
      (et/with-fake-inbox
        (array-map
         1 (do
             ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:display :area})
             (et/regex-email-bodies #"the question was edited by Rasta Toucan"))
         2 (boolean (Pulse (u/get-id pulse)))
         3 (do
             ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:display :bar})
             (et/regex-email-bodies #"the question was edited by Rasta Toucan"))
         4 (boolean (Pulse (u/get-id pulse))))))))

;; Removing the goal value will trigger the alert to be deleted
(expect
  {1 (rasta-alert-not-working {"the question was edited by Rasta Toucan" true})
   2 nil}
  (tt/with-temp* [Card                  [card  {:display                :line
                                                :visualization_settings {:graph.goal_value 10}}]
                  Pulse                 [pulse {:alert_condition  "goal"
                                                :alert_first_only false
                                                :creator_id       (user->id :rasta)
                                                :name             "Original Alert Name"}]
                  PulseCard             [_     {:pulse_id (u/get-id pulse)
                                                :card_id  (u/get-id card)
                                                :position 0}]
                  PulseChannel          [pc    {:pulse_id (u/get-id pulse)}]
                  PulseChannelRecipient [pcr   {:user_id          (user->id :rasta)
                                                :pulse_channel_id (u/get-id pc)}]]
    (with-cards-in-writeable-collection card
      (et/with-fake-inbox
        (et/with-expected-messages 1
          ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:visualization_settings {:something "else"}}))
        (array-map
         1 (et/regex-email-bodies #"the question was edited by Rasta Toucan")
         2 (Pulse (u/get-id pulse)))))))

;; Adding an additional breakout will cause the alert to be removed
(expect
  {1 (rasta-alert-not-working {"the question was edited by Crowberto Corv" true})
   2 nil}
  (tt/with-temp* [Card                 [card  {:display                :line
                                               :visualization_settings {:graph.goal_value 10}
                                               :dataset_query          (assoc-in
                                                                        (mbql-count-query (data/id) (data/id :checkins))
                                                                        [:query :breakout]
                                                                        [["datetime-field"
                                                                          (data/id :checkins :date)
                                                                          "hour"]])}]
                  Pulse                 [pulse {:alert_condition  "goal"
                                                :alert_first_only false
                                                :creator_id       (user->id :rasta)
                                                :name             "Original Alert Name"}]
                  PulseCard             [_     {:pulse_id (u/get-id pulse)
                                                :card_id  (u/get-id card)
                                                :position 0}]
                  PulseChannel          [pc    {:pulse_id (u/get-id pulse)}]
                  PulseChannelRecipient [pcr   {:user_id          (user->id :rasta)
                                                :pulse_channel_id (u/get-id pc)}]]
    (et/with-fake-inbox
      (et/with-expected-messages 1
        ((user->client :crowberto) :put 200 (str "card/" (u/get-id card))
         {:dataset_query (assoc-in (mbql-count-query (data/id) (data/id :checkins))
                                   [:query :breakout] [["datetime-field" (data/id :checkins :date) "hour"]
                                                       ["datetime-field" (data/id :checkins :date) "minute"]])}))
      (array-map
       1 (et/regex-email-bodies #"the question was edited by Crowberto Corv")
       2 (Pulse (u/get-id pulse))))))

;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                          DELETING A CARD (DEPRECATED)                                          |
;;; +----------------------------------------------------------------------------------------------------------------+
;; Deprecated because you're not supposed to delete cards anymore. Archive them instead

;; Check that we can delete a card
(expect
  nil
  (tt/with-temp Card [card]
    (with-cards-in-writeable-collection card
      ((user->client :rasta) :delete 204 (str "card/" (u/get-id card)))
      (Card (u/get-id card)))))

;; deleting a card that doesn't exist should return a 404 (#1957)
(expect
  "Not found."
  ((user->client :crowberto) :delete 404 "card/12345"))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                   FAVORITING                                                   |
;;; +----------------------------------------------------------------------------------------------------------------+

;; Helper Functions
(defn- fave? [card]
  (db/exists? CardFavorite, :card_id (u/get-id card), :owner_id (user->id :rasta)))

(defn- fave! [card]
  ((user->client :rasta) :post 200 (format "card/%d/favorite" (u/get-id card))))

(defn- unfave! [card]
  ((user->client :rasta) :delete 204 (format "card/%d/favorite" (u/get-id card))))

;; ## GET /api/card/:id/favorite
;; Can we see if a Card is a favorite ?
(expect
  false
  (tt/with-temp Card [card]
    (with-cards-in-readable-collection card
      (fave? card))))

;; ## POST /api/card/:id/favorite
;; Can we favorite a card?
(expect
  {1 false
   2 true}
  (tt/with-temp Card [card]
    (with-cards-in-readable-collection card
      (array-map
       1 (fave? card)
       2 (do (fave! card)
             (fave? card))))))

;; DELETE /api/card/:id/favorite
;; Can we unfavorite a card?
(expect
  {1 false
   2 true
   3 false}
  (tt/with-temp Card [card]
    (with-cards-in-readable-collection card
      (array-map
       1 (fave? card)
       2 (do (fave! card)
             (fave? card))
       3 (do (unfave! card)
             (fave? card))))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                              LABELS (DEPRECATED)                                               |
;;; +----------------------------------------------------------------------------------------------------------------+
;; DEPRECATED because Labels are deprecated in favor of Collections.

;;; POST /api/card/:id/labels
;; Check that we can update card labels
(expect
  {1 []
   2 [{:name "Toucan-Friendly",   :slug "toucan_friendly",   :icon nil}
      {:name "Toucan-Unfriendly", :slug "toucan_unfriendly", :icon nil}]
   3 []}
  (tt/with-temp* [Card  [card]
                  Label [label-1 {:name "Toucan-Friendly"}]
                  Label [label-2 {:name "Toucan-Unfriendly"}]]
    (with-cards-in-writeable-collection card
      (let [get-labels     (fn []
                             (for [label (:labels ((user->client :rasta) :get 200, (str "card/" (u/get-id card))))]
                               (dissoc label :id)))
            update-labels! (fn [labels]
                             ((user->client :rasta) :post 200, (format "card/%d/labels" (u/get-id card))
                              {:label_ids (map u/get-id labels)})
                             (get-labels))]
        (array-map
         ;; (1) should start w/ no labels
         1 (get-labels)
         ;; (2) set a few labels
         2 (update-labels! [label-1 label-2])
         ;; (3) can reset to no labels?
         3 (update-labels! []))))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                            CSV/JSON/XLSX DOWNLOADS                                             |
;;; +----------------------------------------------------------------------------------------------------------------+

;;; Tests for GET /api/card/:id/json

;; endpoint should return an array of maps, one for each row
(expect
  [{(keyword "COUNT(*)") 75}]
  (with-temp-native-card [_ card]
    (with-cards-in-readable-collection card
      ((user->client :rasta) :post 200 (format "card/%d/query/json" (u/get-id card))))))

;;; Tests for GET /api/card/:id/xlsx
(expect
  [{:col "COUNT(*)"} {:col 75.0}]
  (with-temp-native-card [_ card]
    (with-cards-in-readable-collection card
      (->> ((user->client :rasta) :post 200 (format "card/%d/query/xlsx" (u/get-id card))
            {:request-options {:as :byte-array}})
           ByteArrayInputStream.
           spreadsheet/load-workbook
           (spreadsheet/select-sheet "Query result")
           (spreadsheet/select-columns {:A :col})))))

;;; Test GET /api/card/:id/query/csv & GET /api/card/:id/json & GET /api/card/:id/query/xlsx **WITH PARAMETERS**
(def ^:private ^:const ^String encoded-params
  (json/generate-string [{:type   :category
                          :target [:variable [:template-tag :category]]
                          :value  2}]))

;; CSV
(expect
  (str "COUNT(*)\n"
       "8\n")
  (with-temp-native-card-with-params [_ card]
    (with-cards-in-readable-collection card
      ((user->client :rasta) :post 200 (format "card/%d/query/csv?parameters=%s" (u/get-id card) encoded-params)))))

;; JSON
(expect
  [{(keyword "COUNT(*)") 8}]
  (with-temp-native-card-with-params [_ card]
    (with-cards-in-readable-collection card
      ((user->client :rasta) :post 200 (format "card/%d/query/json?parameters=%s" (u/get-id card) encoded-params)))))

;; XLSX
(expect
  [{:col "COUNT(*)"} {:col 8.0}]
  (with-temp-native-card-with-params [_ card]
    (with-cards-in-readable-collection card
      (->> ((user->client :rasta) :post 200 (format "card/%d/query/xlsx?parameters=%s" (u/get-id card) encoded-params)
            {:request-options {:as :byte-array}})
           ByteArrayInputStream.
           spreadsheet/load-workbook
           (spreadsheet/select-sheet "Query result")
           (spreadsheet/select-columns {:A :col})))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                  COLLECTIONS                                                   |
;;; +----------------------------------------------------------------------------------------------------------------+

;; Make sure we can create a card and specify its `collection_id` at the same time
(expect
  (tt/with-temp Collection [collection]
    (tu/with-model-cleanup [Card]
      (perms/grant-collection-readwrite-permissions! (perms-group/all-users) collection)
      (let [card ((user->client :rasta) :post 200 "card"
                  (assoc (card-with-name-and-query)
                    :collection_id (u/get-id collection)))]
        (= (db/select-one-field :collection_id Card :id (u/get-id card))
           (u/get-id collection))))))

;; Make sure we card creation fails if we try to set a `collection_id` we don't have permissions for
(expect
  "You don't have permissions to do that."
  (tu/with-model-cleanup [Card]
    (tt/with-temp Collection [collection]
      ((user->client :rasta) :post 403 "card"
       (assoc (card-with-name-and-query)
         :collection_id (u/get-id collection))))))

;; Make sure we can change the `collection_id` of a Card if it's not in any collection
(expect
  (tt/with-temp* [Card       [card]
                  Collection [collection]]
    ((user->client :crowberto) :put 200 (str "card/" (u/get-id card)) {:collection_id (u/get-id collection)})
    (= (db/select-one-field :collection_id Card :id (u/get-id card))
       (u/get-id collection))))

;; Make sure we can still change *anything* for a Card if we don't have permissions for the Collection it belongs to
(expect
  "You don't have permissions to do that."
  (tt/with-temp* [Collection [collection]
                  Card       [card       {:collection_id (u/get-id collection)}]]
    ((user->client :rasta) :put 403 (str "card/" (u/get-id card)) {:name "Number of Blueberries Consumed Per Month"})))

;; Make sure that we can't change the `collection_id` of a Card if we don't have write permissions for the new
;; collection
(expect
  "You don't have permissions to do that."
  (tt/with-temp* [Collection [original-collection]
                  Collection [new-collection]
                  Card       [card                {:collection_id (u/get-id original-collection)}]]
    (perms/grant-collection-readwrite-permissions! (perms-group/all-users) original-collection)
    ((user->client :rasta) :put 403 (str "card/" (u/get-id card)) {:collection_id (u/get-id new-collection)})))

;; Make sure that we can't change the `collection_id` of a Card if we don't have write permissions for the current
;; collection
(expect
  "You don't have permissions to do that."
  (tt/with-temp* [Collection [original-collection]
                  Collection [new-collection]
                  Card       [card                {:collection_id (u/get-id original-collection)}]]
    (perms/grant-collection-readwrite-permissions! (perms-group/all-users) new-collection)
    ((user->client :rasta) :put 403 (str "card/" (u/get-id card)) {:collection_id (u/get-id new-collection)})))

;; But if we do have permissions for both, we should be able to change it.
(expect
  (tt/with-temp* [Collection [original-collection]
                  Collection [new-collection]
                  Card       [card                {:collection_id (u/get-id original-collection)}]]
    (perms/grant-collection-readwrite-permissions! (perms-group/all-users) original-collection)
    (perms/grant-collection-readwrite-permissions! (perms-group/all-users) new-collection)
    ((user->client :rasta) :put 200 (str "card/" (u/get-id card)) {:collection_id (u/get-id new-collection)})
    (= (db/select-one-field :collection_id Card :id (u/get-id card))
       (u/get-id new-collection))))


;;; Test GET /api/card?collection= -- Test that we can use empty string to return Cards in the Root Collection
(expect
  ["Card 2"]
  (tt/with-temp* [Collection [collection]
                  Card       [card-1 {:name "Card 1", :collection_id (u/get-id collection)}]
                  Card       [card-2 {:name "Card 2"}]]
    (map :name ((user->client :crowberto) :get 200 "card/" :collection ""))))

;; Test GET /api/card?collection=<slug> filters by collection with slug
(expect
  ["Card 1"]
  (tt/with-temp* [Collection [collection {:name "Favorite Places"}]
                  Card       [card-1 {:name "Card 1", :collection_id (u/get-id collection)}]
                  Card       [card-2 {:name "Card 2"}]]
    (perms/grant-collection-read-permissions! (perms-group/all-users) collection)
    (map :name ((user->client :rasta) :get 200 "card/" :collection :favorite_places))))

;; Test GET /api/card?collection=<slug> should return a 404 if no such collection exists
(expect
  "Not found."
  ((user->client :rasta) :get 404 "card/" :collection :some_fake_collection_slug))

;; Make sure GET /api/card?collection=<slug> still works with Collections with URL-encoded Slugs (#4535)
(expect
  []
  (tt/with-temp Collection [collection {:name "Obsługa klienta"}]
    (perms/grant-collection-read-permissions! (perms-group/all-users) collection)
    ((user->client :rasta) :get 200 "card/" :collection "obs%C5%82uga_klienta")))

;; ...even if the slug isn't passed in URL-encoded
(expect
  []
  (tt/with-temp Collection [collection {:name "Obsługa klienta"}]
    (perms/grant-collection-read-permissions! (perms-group/all-users) collection)
    ((user->client :rasta) :get 200 "card/" :collection "obsługa_klienta")))


;;; ------------------------------ Bulk Collections Update (POST /api/card/collections) ------------------------------

(defn- collection-names
  "Given a sequences of `cards-or-card-ids`, return a corresponding sequence of names of the Collection each Card is
  in."
  [cards-or-card-ids]
  (when (seq cards-or-card-ids)
    (let [cards               (db/select [Card :collection_id] :id [:in (map u/get-id cards-or-card-ids)])
          collection-ids      (set (filter identity (map :collection_id cards)))
          collection-id->name (when (seq collection-ids)
                                (db/select-id->field :name Collection :id [:in collection-ids]))]
      (for [card cards]
        (get collection-id->name (:collection_id card))))))

(defn- POST-card-collections!
  "Update the Collection of CARDS-OR-CARD-IDS via the `POST /api/card/collections` endpoint using USERNAME;
   return the response of this API request and the latest Collection IDs from the database."
  [username expected-status-code collection-or-collection-id-or-nil cards-or-card-ids]
  (array-map
   :response
   ((user->client username) :post expected-status-code "card/collections"
    {:collection_id (when collection-or-collection-id-or-nil
                      (u/get-id collection-or-collection-id-or-nil))
     :card_ids      (map u/get-id cards-or-card-ids)})

   :collections
   (collection-names cards-or-card-ids)))

;; Test that we can bulk move some Cards with no collection into a collection
(expect
  {:response    {:status "ok"}
   :collections ["Pog Collection"
                 "Pog Collection"]}
  (tt/with-temp* [Collection [collection {:name "Pog Collection"}]
                  Card       [card-1]
                  Card       [card-2]]
    (POST-card-collections! :crowberto 200 collection [card-1 card-2])))

;; Test that we can bulk move some Cards from one collection to another
(expect
  {:response    {:status "ok"}
   :collections ["New Collection" "New Collection"]}
  (tt/with-temp* [Collection [old-collection {:name "Old Collection"}]
                  Collection [new-collection {:name "New Collection"}]
                  Card       [card-1         {:collection_id (u/get-id old-collection)}]
                  Card       [card-2         {:collection_id (u/get-id old-collection)}]]
    (POST-card-collections! :crowberto 200 new-collection [card-1 card-2])))

;; Test that we can bulk remove some Cards from a collection
(expect
  {:response    {:status "ok"}
   :collections [nil nil]}
  (tt/with-temp* [Collection [collection]
                  Card       [card-1     {:collection_id (u/get-id collection)}]
                  Card       [card-2     {:collection_id (u/get-id collection)}]]
    (POST-card-collections! :crowberto 200 nil [card-1 card-2])))

;; Check that we aren't allowed to move Cards if we don't have permissions for destination collection
(expect
  {:response    "You don't have permissions to do that."
   :collections [nil nil]}
  (tt/with-temp* [Collection [collection]
                  Card       [card-1]
                  Card       [card-2]]
    (POST-card-collections! :rasta 403 collection [card-1 card-2])))

;; Check that we aren't allowed to move Cards if we don't have permissions for source collection
(expect
  {:response    "You don't have permissions to do that."
   :collections ["Horseshoe Collection" "Horseshoe Collection"]}
  (tt/with-temp* [Collection [collection {:name "Horseshoe Collection"}]
                  Card       [card-1     {:collection_id (u/get-id collection)}]
                  Card       [card-2     {:collection_id (u/get-id collection)}]]
    (POST-card-collections! :rasta 403 nil [card-1 card-2])))

;; Check that we aren't allowed to move Cards if we don't have permissions for the Card
(expect
  {:response    "You don't have permissions to do that."
   :collections [nil nil]}
  (tt/with-temp* [Collection [collection]
                  Database   [database]
                  Table      [table      {:db_id (u/get-id database)}]
                  Card       [card-1     {:dataset_query (mbql-count-query (u/get-id database) (u/get-id table))}]
                  Card       [card-2     {:dataset_query (mbql-count-query (u/get-id database) (u/get-id table))}]]
    (perms/revoke-permissions! (perms-group/all-users) (u/get-id database))
    (perms/grant-collection-readwrite-permissions! (perms-group/all-users) collection)
    (POST-card-collections! :rasta 403 collection [card-1 card-2])))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                            PUBLIC SHARING ENDPOINTS                                            |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn- shared-card []
  {:public_uuid       (str (UUID/randomUUID))
   :made_public_by_id (user->id :crowberto)})

;;; ----------------------------------------- POST /api/card/:id/public_link -----------------------------------------

;; Test that we can share a Card
(expect
  (tu/with-temporary-setting-values [enable-public-sharing true]
    (tt/with-temp Card [card]
      (let [{uuid :uuid} ((user->client :crowberto) :post 200 (format "card/%d/public_link" (u/get-id card)))]
        (db/exists? Card :id (u/get-id card), :public_uuid uuid)))))

;; Test that we *cannot* share a Card if we aren't admins
(expect
  "You don't have permissions to do that."
  (tu/with-temporary-setting-values [enable-public-sharing true]
    (tt/with-temp Card [card]
      ((user->client :rasta) :post 403 (format "card/%d/public_link" (u/get-id card))))))

;; Test that we *cannot* share a Card if the setting is disabled
(expect
  "Public sharing is not enabled."
  (tu/with-temporary-setting-values [enable-public-sharing false]
    (tt/with-temp Card [card]
      ((user->client :crowberto) :post 400 (format "card/%d/public_link" (u/get-id card))))))

;; Test that we *cannot* share a Card if the Card has been archived
(expect
  {:message "The object has been archived.", :error_code "archived"}
  (tu/with-temporary-setting-values [enable-public-sharing true]
    (tt/with-temp Card [card {:archived true}]
      ((user->client :crowberto) :post 404 (format "card/%d/public_link" (u/get-id card))))))

;; Test that we get a 404 if the Card doesn't exist
(expect
  "Not found."
  (tu/with-temporary-setting-values [enable-public-sharing true]
    ((user->client :crowberto) :post 404 (format "card/%d/public_link" Integer/MAX_VALUE))))

;; Test that if a Card has already been shared we reüse the existing UUID
(expect
  (tu/with-temporary-setting-values [enable-public-sharing true]
    (tt/with-temp Card [card (shared-card)]
      (= (:public_uuid card)
         (:uuid ((user->client :crowberto) :post 200 (format "card/%d/public_link" (u/get-id card))))))))


;;; ---------------------------------------- DELETE /api/card/:id/public_link ----------------------------------------

;; Test that we can unshare a Card
(expect
  false
  (tu/with-temporary-setting-values [enable-public-sharing true]
    (tt/with-temp Card [card (shared-card)]
      ((user->client :crowberto) :delete 204 (format "card/%d/public_link" (u/get-id card)))
      (db/exists? Card :id (u/get-id card), :public_uuid (:public_uuid card)))))

;; Test that we *cannot* unshare a Card if we are not admins
(expect
  "You don't have permissions to do that."
  (tu/with-temporary-setting-values [enable-public-sharing true]
    (tt/with-temp Card [card (shared-card)]
      ((user->client :rasta) :delete 403 (format "card/%d/public_link" (u/get-id card))))))

;; Test that we get a 404 if Card isn't shared
(expect
  "Not found."
  (tu/with-temporary-setting-values [enable-public-sharing true]
    (tt/with-temp Card [card]
      ((user->client :crowberto) :delete 404 (format "card/%d/public_link" (u/get-id card))))))

;; Test that we get a 404 if Card doesn't exist
(expect
  "Not found."
  (tu/with-temporary-setting-values [enable-public-sharing true]
    ((user->client :crowberto) :delete 404 (format "card/%d/public_link" Integer/MAX_VALUE))))

;; Test that we can fetch a list of publicly-accessible cards
(expect
  [{:name true, :id true, :public_uuid true}]
  (tu/with-temporary-setting-values [enable-public-sharing true]
    (tt/with-temp Card [card (shared-card)]
      (for [card ((user->client :crowberto) :get 200 "card/public")]
        (m/map-vals boolean (select-keys card [:name :id :public_uuid]))))))

;; Test that we can fetch a list of embeddable cards
(expect
  [{:name true, :id true}]
  (tu/with-temporary-setting-values [enable-embedding true]
    (tt/with-temp Card [card {:enable_embedding true}]
      (for [card ((user->client :crowberto) :get 200 "card/embeddable")]
        (m/map-vals boolean (select-keys card [:name :id]))))))

;; Test related/recommended entities
(expect
  #{:table :metrics :segments :dashboard-mates :similar-questions :canonical-metric :dashboards :collections}
  (tt/with-temp Card [card]
    (-> ((user->client :crowberto) :get 200 (format "card/%s/related" (u/get-id card))) keys set)))
