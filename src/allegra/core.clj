(ns allegra.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [clj-amazon.core :as api]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.zip :as zip :refer [xml-zip]]
            [clojure.data.zip.xml :as zip-xml :refer [xml-> xml1-> text attr]]
            [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as appenders]))

(defn ppxml [xml]
  (let [in (javax.xml.transform.stream.StreamSource.
            (java.io.StringReader. xml))
        writer (java.io.StringWriter.)
        out (javax.xml.transform.stream.StreamResult. writer)
        transformer (.newTransformer 
                     (javax.xml.transform.TransformerFactory/newInstance))]
    (.setOutputProperty transformer 
                        javax.xml.transform.OutputKeys/INDENT "yes")
    (.setOutputProperty transformer 
                        "{http://xml.apache.org/xslt}indent-amount" "2")
    (.setOutputProperty transformer 
                        javax.xml.transform.OutputKeys/METHOD "xml")
    (.transform transformer in out)
    (-> out .getWriter .toString)))

(defn get-items-by-upc
  [{:keys [access-key secret-key associate-id] :as credentials} upc]
  (log/info "Processing UPC" upc)
  (let [raw-xml (api/with-signer (access-key, secret-key)
                  (api/item-lookup :item-id upc
                                   :id-type "UPC"
                                   :search-index "Blended"
                                   :associate-tag associate-id
                                   :response-group "ItemAttributes,Offers,SalesRank"))] 
    (log/trace "Retrieved XML:\n" (ppxml raw-xml))
    (xml-zip (xml/parse (java.io.ByteArrayInputStream. (.getBytes raw-xml "UTF-8"))))))

(defn numeric
  [node]
  (float (/ (Integer/parseInt (text node)) 100)))

(defn parse-item
  [item]
  (let [attributes (xml1-> item :ItemAttributes)
        height (xml1-> attributes :PackageDimensions :Height)
        width (xml1-> attributes :PackageDimensions :Width)
        weight (xml1-> attributes :PackageDimensions :Weight)
        asin (xml1-> item :ASIN text)
        buy-box-price (xml1-> item :OfferSummary :LowestNewPrice :Amount numeric)
        height (xml1-> attributes :PackageDimensions :Height numeric)
        length (xml1-> attributes :PackageDimensions :Length numeric)
        width (xml1-> attributes :PackageDimensions :Width numeric)
        weight (xml1-> attributes :PackageDimensions :Weight numeric)]
    {:upc (xml1-> attributes :UPC text)
     :asin asin
     :product-name (xml1-> attributes :Title text)     
     :product-category (xml1-> attributes :ProductTypeName text)
     :best-sellers-rank (xml1-> item :SalesRank text)     
     :product-url (str "http://www.amazon.com/gp/product/" asin)
     :offers-url (xml1-> item :Offers :MoreOffersUrl text)
     :buy-box-price buy-box-price
     :height height
     :length length
     :width width
     :weight weight}))

(defn parse-items
  [credentials upc]
  (try
    (let [root (get-items-by-upc credentials upc)
          error-code (xml1-> root :Items :Code text)
          valid? (xml1-> root :Items :Request :IsValid text)
          parse-error (fn [error]
                        {:code (xml1-> error :Code text)
                         :message (xml1-> error :Message text)})
          errors (xml-> root :Items :Request :Errors :Error)
          result (if (empty? errors)
                   (let [items (mapv parse-item (xml-> root :Items :Item))]
                     (log/info "Found" (count items) "items")
                     {:status :ok :upc upc :items items})
                   (let [errors (mapv parse-error errors)]
                     (log/info (count errors) "errors reported.")
                     {:status :error :upc upc :errors errors}))]
      result)
    (catch Exception e
      (log/error "Failed while processing UPC" upc)
      {:status :error
       :upc upc
       :errors [{:code "StupidityError" 
                 :message (.getMessage e)
                 :exception e}]})))

(def columns [:upc
              :asin
              :product-name
              :product-category
              :best-sellers-rank
              :product-url
              :offers-url
              :buy-box-price
              :height
              :length
              :width
              :weight])

(defn run
  [credentials upc-path out-path]
  (log/info "Reading UPCs from" upc-path)
  (with-open [rdr (io/reader upc-path)]
      (let [upcs (line-seq rdr)
            results (map (partial parse-items credentials) upcs)
            {successes :ok failures :error} (group-by :status results)
            items (flatten (map :items successes))
            to-line (fn [item] (reduce (fn [line k] (conj line (get item k))) [] columns))
            lines (map to-line items)
            with-header (conj lines (map name columns))] 
        (doseq [{:keys [upc errors]} failures]
          (log/info (str "Failed to retrieve items for UPC " upc ":"))
          (doseq [{:keys [code message exception]} errors]
            (log/error code ":" message)
            (when exception (log/error exception))))
        (log/info "Writing out" (count items) "items to" out-path)
        (with-open [wtr (io/writer out-path)]
          (csv/write-csv wtr with-header)) 
        (log/info "Successfully processed" (count successes) "UPCs -" (count failures) "failed"))))

(defn prompt-for-string
  [prompt]
  (log/info prompt)
  (read-line))

(defn -main
  [& [config-path upc-path out-path]]
  (log/info "*******************")
  (log/info "Starting Allegra...")
  (log/info "*******************")
  (try
    (let [config-path (or config-path (prompt-for-string "Enter path to configuration file: "))]
      (if-not (.exists (io/file config-path))
        (log/error (str "Configuration file not found:" config-path))
        (let [{:keys [log-path log-level credentials]} (edn/read-string (slurp config-path))
              log-path (or log-path (prompt-for-string "Enter path to log file: "))
              upc-path (or upc-path (prompt-for-string "Enter path to UPC file: "))
              out-path (or out-path (prompt-for-string "Enter path for output CSV: "))]
          (log/set-level! (or log-level :trace))
          (log/merge-config! {:appenders {:spit (appenders/spit-appender {:fname log-path})}})
          (run credentials upc-path out-path))))
    (catch Exception e
      (log/error "Processing failed.")
      (log/error e)))
  (prompt-for-string "Enter anything to exit:"))
