(ns allegra.walmart
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clojure.set :refer [rename-keys]]
            [clj-http.client :as http]
            [taoensso.timbre :as log]
            [clojure.edn :as edn]))

(defn ok? [status] (= :ok (keyword status)))

(def categories
  {"Cell Phones" "1105910",
   "Photo Center" "5426",
   "Electronics" "3944",
   "Video Games" "2636",
   "Patio & Garden" "5428",
   "Household Essentials" "1115193",
   "Party & Occasions" "2637",
   "Baby" "5427",
   "Health" "976760",
   "Music" "4104",
   "Food" "976759",
   "Jewelry" "3891",
   "Gifts & Registry" "1094765",
   "Sports & Outdoors" "4125",
   "Movies & TV" "4096",
   "Toys" "4171",
   "Seasonal" "1085632",
   "Auto & Tires" "91083",
   "Office" "1229749",
   "Home Improvement" "1072864",
   "Books" "3920",
   "Pets" "5440",
   "Beauty" "1085666",
   "Home" "4044",
   "Clothing" "5438"})

(def toys (get categories "Toys")) 
(def key (-> "test-resources/properties.edn"
             (slurp)
             (edn/read-string)
             (:walmart-credentials)
             (:access-key)))

(def url "http://api.walmartlabs.com")
(def version "v1")

(defn get-items
  [upc]
  (let [url (str url "/" version "/items")
        query-params {"ApiKey" key
                      "upc" upc
                      "format" "json"}
        {:keys [status body] :as response} (http/get url {:query-params query-params})]
    (log/trace "Response:\n" (with-out-str (pprint response)))
    (if (= status 200)
      {:status :ok :body (json/read-str body)}
      {:status :error :body (json/read-str body)})))

(defn parse-json [s] (json/read-str s :key-fn keyword))

(defn search
  [start category]
  (let [url (str url "/" version "/search")
        query-params {"ApiKey" key
                      "format" "json"
                      "query" "*"
                      "categoryId" category
                      "start" start
                      "numItems" 25
                      "sort" "bestSeller"
                      "responseGroup" "full"}
        {:keys [status body] :as response} (http/get url {:query-params query-params
                                                          :throw-exceptions false})]
    (log/trace "Response:\n" (with-out-str (pprint response)))
    (case status
      200 {:status :ok :body (parse-json body)}
      400 {:status :bad :body (parse-json body)}
      {:status :error :body (parse-json body)})))

(defn search2
  [start category]
  (let [{:keys [status body]} (search start category)]
    (when (ok? status)
      (map :name (:items body)))))

(defn get-items
  ([start category]
   (get-items start [] category))
  
  ([start page category]
   (let [page (if (empty? page)
                (search2 start category)
                page)
         [current-item & remaining-items] page]
     (cons current-item (lazy-seq (get-items (inc start) remaining-items category))))))

;; TODO: validate criteria
(defn better-search
  [start {:keys [category query sort]}]
  (let [url (str url "/" version "/search")
        query-params {"ApiKey" key
                      "format" "json"
                      "categoryId" (get categories category)
                      "start" start
                      "sort" sort
                      "numItems" 25
                      "responseGroup" "full"}
        {:keys [status body] :as response} (http/get url {:query-params query-params
                                                          :throw-exceptions false})]
    (log/trace "Response:\n" (with-out-str (pprint response)))
    (case status
      200 {:status :ok :body (parse-json body)}
      400 {:status :bad :body (parse-json body)}
      {:status :error :body (parse-json body)})))

(defn search-seq
  ([criteria]
   (search-seq criteria 1 []))

  ([criteria start]
   (search-seq criteria start []))
  
  ([criteria start page]
   (lazy-seq
    (when-let [page (if (empty? page)
                      (better-search start criteria)
                      page)]
      (cons (first page) (search-seq criteria (inc start) (rest page)))))))


(defn item-seq
  ([category]
   (item-seq category 1 []))

  ([category start]
   (item-seq category start []))
  
  ([category start page]
   (lazy-seq
    (when-let [page (if (empty? page)
                      (search2 start category)
                      page)]
      (cons (first page) (item-seq category (inc start) (rest page)))))))

(defn taxonomy
  []
  (let [url (str url "/" version "/taxonomy")
        query-params {"ApiKey" key
                      "format" "json"}
        {:keys [status body] :as response} (http/get url {:query-params query-params
                                                          :throw-exceptions false})]
    (log/trace "Response:\n" (with-out-str (pprint response)))
    (if (= status 200)
      {:status :ok :body (json/read-str body)}
      {:status :error :body (json/read-str body)})))

(defn trends
  []
  (let [url (str url "/" version "/trends")
        query-params {"ApiKey" key
                      "format" "json"}
        {:keys [status body] :as response} (http/get url {:query-params query-params
                                                          :throw-exceptions false})]
    (log/trace "Response:\n" (with-out-str (pprint response)))
    (if (= status 200)
      {:status :ok :body (json/read-str body)}
      {:status :error :body (json/read-str body)})))

(trends)

(comment

  (map  transform (top 5 (get categories "Toys")))
  (into {} (map (fn [{:strs [id name]}] [id name]) (-> (taxonomy)
                                                       (:body)
                                                       (get "categories"))))

  (map #(get % "name") (-> (search)
                           (:body)
                           (get "items")))
  
  (into {} (map (fn [{:strs [upc name]}] [upc name]) (-> (search)
                                                         (:body)
                                                         (get "items"))))

  (map #(get % "name" ) (-> (search)
                          (:body)
                          (get "items")))
  
  
  (-> (get-items "630509406661")
      (:body)
      (get "items")
      first
      (dissoc "imageEntities"))

  
  (-> (get-items "630509406661")
      (:body)
      (get "items")
      first
      (keys))

  (-> (get-items "630509406661")
      (:body)
      (get "items")
      first
      (select-keys ["upc" "name" "salePrice" "productUrl"]))
  )

(comment
  (defn top
    [n category]
    (let [num-pages (quot n 25)
          starts (range 1 n 25)
          get-page (fn [start]
                     (let [result (search start category)]
                       (Thread/sleep 200)
                       (get-in result [:body "items"])))])
    (take n (flatten (map get-page starts)))))


(comment
  (defn get-items
    ([category]
     (get-items 1 [] category))
    
    ([start category]
     (get-items start [] category))
    
    ([start page category]
     (lazy-seq
      (let [page (if (empty? page)
                   (do
                     (println "Fetching page for item" start)
                     (map transform (get-in (search start category) [:body "items"])))
                   page) 
            [current-item & remaining-items] page]
        (cons current-item (get-items (inc start) remaining-items category)))))))
