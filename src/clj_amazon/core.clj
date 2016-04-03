;; Copyright (C) 2011, Eduardo Julián. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the 
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns clj-amazon.core
  "The core functionality shared by other namespaces."
  (:import (java.io ByteArrayInputStream))
  (:require [clj-http.client :as http]
            [clojure.pprint :refer [pprint]]
            [clojure.xml :as xml]
            [taoensso.timbre :as log]
            [clojure.walk :as walk]
            [clojure.zip :as zip]))

; The following is a Clojure version of Amazon's SignedRequestsHelper class + some modifications.
(defn percent-encode-rfc-3986 [s]
  (-> (java.net.URLEncoder/encode (str s) "UTF-8")
    (.replace "+" "%20")
    (.replace "*" "%2A")
    (.replace "%7E" "~")))

(defn timestamp []
  (-> (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'.000Z'")
        (.setTimeZone (java.util.TimeZone/getTimeZone "GMT")))
    (.format (.getTime (java.util.Calendar/getInstance)))))

(defn canonicalize [sorted-map]
  (if (empty? sorted-map)
    ""
    (->> sorted-map
      (map (fn [[k v]] (if v (str (percent-encode-rfc-3986 k) "=" (percent-encode-rfc-3986 v)))))
      (filter (comp not nil?))
      (interpose "&")
      (apply str)
      ;((fn [_] (prn _) _))
      )
    ))

(defprotocol ISignedRequestsHelper
  (sign [self params])
  (hmac [self string]))

; Forgive the weird code. I just like writing point-free kind of code.
(defrecord SignedRequestsHelper [endpoint access-key secret-key secret-key-spec mac]
  ISignedRequestsHelper
  (sign [self params]
    (let [query-str (-> params
                      (assoc "AWSAccessKeyId" access-key, "Timestamp" (timestamp))
                      ;java.util.TreeMap.
                      canonicalize)]
      (->> query-str
        (str "GET" "\n" endpoint "\n" "/onca/xml" "\n")
        (.hmac self)
        percent-encode-rfc-3986
        (str "http://" endpoint "/onca/xml" "?" query-str "&Signature="))
      ))
  (hmac [self string]
    (-> string (.getBytes "UTF-8")
      (->> (.doFinal mac)
        (.encode (org.apache.commons.codec.binary.Base64. 76 (byte-array 0))))
      String.)))

(defn signed-request-helper "Try not to use this directly. Better use it through with-signer."
  [access-key secret-key]
  (let [secret-key-spec (-> secret-key (.getBytes "UTF-8") (javax.crypto.spec.SecretKeySpec. "HmacSHA256"))
        mac (javax.crypto.Mac/getInstance "HmacSHA256")]
    (.init mac secret-key-spec)
    (SignedRequestsHelper. "ecs.amazonaws.com" access-key secret-key
                           secret-key-spec mac)))

; The following are the basic utils for Amazon's APIs.
(def ^:dynamic *signer*)

(defn fetch-url [url]
  (let [{:keys [status body] :as response} (http/get url {:headers {"Accept" "text/xml"
                                                                    "Accept-Charset" "UTF-8"}})]
    (log/trace "Response:\n" (with-out-str (pprint response))) 
    (slurp (ByteArrayInputStream. (.getBytes body "UTF-8")))))

(defn encode-url [url] (if url (java.net.URLEncoder/encode url "UTF-8")))

(defn assoc+
  ([m k v]
   (let [item (get m k)]
     (if k
       (cond (nil? item) (assoc m k v)
             (vector? item) (assoc m k (conj item v))
             :else (assoc m k [item v]))
       m)))
  ([m k v & kvs] (apply assoc+ (assoc+ m k v) kvs)))

(defn _bool->str [bool] (if bool "True" "False"))

(defn _str->sym [string]
  (-> (reduce #(if (Character/isUpperCase %2) (str %1 "-" (Character/toLowerCase %2)) (str %1 %2)) "" string)
    (.substring 1) symbol ))

(defn _extract-strs [strs] (map #(if (list? %) (second %) %) strs))

(defn _extract-vars [strs] (map _str->sym (_extract-strs strs)))

(defmacro with-signer
  "Evaluates the given forms with the given [\"Access Key\", \"Secret Key\"] pair."
  [signer-params & body]
  `(binding [*signer* (signed-request-helper ~@signer-params)]
     ~@body))

(defmacro ^:private make-fns [& specifics] 
  (let [standard-fields '(response-group subscription-id associate-tag merchant-id)]
    `(do ~@(for [[operation specific-appends] (partition 2 specifics)]
             (let [strs (_extract-strs specific-appends)
                   vars (_extract-vars specific-appends)
                   susts (apply hash-map (interleave strs vars))
                   mvars (walk/prewalk-replace susts specific-appends)]
               `(defn ~(_str->sym operation) "" [& {:keys ~(vec (concat standard-fields vars))}]
                  (->> (sorted-map "Service" "AWSECommerceService"
                                   "Version" "2011-08-01"
                                   "Operation" ~operation
                                   "ResponseGroup" ~'response-group
                                   "SubscriptionId" ~'subscription-id
                                   "AssociateTag" ~'associate-tag
                                   "MerchantId" ~'merchant-id
                                   ~@(interleave strs mvars))
                    (.sign *signer*) fetch-url)))))))

(make-fns
  "ItemLookup" ; item-lookup
  ["ItemId" "SearchIndex" "Condition" "IdType" (_bool->str "IncludeReviewsSummary") "OfferPage" "VariationPage"
   "RelatedItemPage" "RelationshipType" "ReviewPage" "ReviewSort" "TagPage" "TagsPerPage" "TagSort" "TruncateReviewsAt"]
  ;;;;;;;;;;;;;;;;;;
  "ItemSearch" ; item-search
  ["Actor" "Artist" "AudienceRating" "Author" "Availability" "Brand" "BrowseNode" "City" "Composer" "Condition" "Conductor"
   "Director" (_bool->str "IncludeReviewsSummary") "ItemPage" (encode-url "Keywords") "Manufacturer" "MaximumPrice" "MinimumPrice"
   "Neighborhood" "Orchestra" "PostalCode" "Power" "Publisher" "RelatedItemPage" "RelationshipType" "ReviewSort" "SearchIndex"
   "Sort" "TagPage" "TagsPerPage" "TagSort" "TextStream" "Title" "TruncateReviewsAt" "VariationPage"]
  ;;;;;;;;;;;;;;;;;;
  "BrowseNodeLookup" ["BrowseNodeId"] ; browse-node-lookup
  ;;;;;;;;;;;;;;;;;;
  "CartAdd" ["ASIN" "CartId" "HMAC" "Item" "Items" "OfferListingId" "Quantity"] ; cart-add
  ;;;;;;;;;;;;;;;;;;
  "CartClear" ["CartId" "HMAC"] ; cart-clear
  ;;;;;;;;;;;;;;;;;;
  "CartCreate" ["ASIN" "Item" "Items" "ListItemId" "OfferListingId" "Quantity"] ; cart-create
  ;;;;;;;;;;;;;;;;;;
  "CartGet" ["CartId" "CartItemId" "HMAC"] ; cart-get
  ;;;;;;;;;;;;;;;;;;
  "CartModify" ["Action" "CartId" "CartItemId" "HMAC" "Item" "Items" "Quantity"] ; cart-modify
  ;;;;;;;;;;;;;;;;;;
  "SellerListingLookup" ["Id" "IdType" "SellerId"] ; seller-listing-lookup
  ;;;;;;;;;;;;;;;;;;
  "SellerListingSearch" ["ListingPage" "OfferStatus" "SellerId" "Sort" "Title"] ; seller-listing-search
  ;;;;;;;;;;;;;;;;;;
  "SellerLookup" ["FeedbackPage" "SellerId"] ; seller-lookup
  ;;;;;;;;;;;;;;;;;;
  "SimilarityLookup" ["Condition" "ItemId" "SimilarityType"] ; similarity-lookup
  )

