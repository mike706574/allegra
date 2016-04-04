(ns allegra.walmart
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clj-http.client :as http]
            [taoensso.timbre :as log]
            [clojure.edn :as edn]))

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

(comment
  (-> (get-items "035000521019")
      (:body)
      (get "items")
      first
      (keys))

  (-> (get-items "035000521019")
      (:body)
      (get "items")
      first
      (select-keys ["upc" "name" "salePrice" "productUrl"])))

