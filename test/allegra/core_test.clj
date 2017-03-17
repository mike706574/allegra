(ns allegra.core-test
  (:require [allegra.core :refer [run]]
            [clojure.test :refer :all]
            [clojure.string :refer [split]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def expected
  [["upc" "asin" "product-name" "product-category" "best-sellers-rank" "product-url" "offers-url" "buy-box-price" "height" "length" "width" "weight"]
   ["724999787512" "B010F029Y4" "Hasbro Pie Face Game" "TOYS_AND_GAMES" "6" "http://www.amazon.com/gp/product/B010F029Y4" "http://www.amazon.com/gp/offer-listing/B010F029Y4%3FSubscriptionId%3DAKIAJFQYE5IY2K6FQK6A%26tag%3Dmike0c7-20%26linkCode%3Dxm2%26camp%3D2025%26creative%3D386001%26creativeASIN%3DB010F029Y4" "17.99" "3.23" "10.63" "10.31" "1.19"]
   ["086002029171" "B00HVMPZCI" "Blurt!®" "TOYS_AND_GAMES" "133097" "http://www.amazon.com/gp/product/B00HVMPZCI" "http://www.amazon.com/gp/offer-listing/B00HVMPZCI%3FSubscriptionId%3DAKIAJFQYE5IY2K6FQK6A%26tag%3Dmike0c7-20%26linkCode%3Dxm2%26camp%3D2025%26creative%3D386001%26creativeASIN%3DB00HVMPZCI" "27.73" "3.0" "10.6" "10.5" "1.15"]
   ["086002029171" "B00SXOOD6W" "Blurt! Game" "TOYS_AND_GAMES" "" "http://www.amazon.com/gp/product/B00SXOOD6W" "http://www.amazon.com/gp/offer-listing/B00SXOOD6W%3FSubscriptionId%3DAKIAJFQYE5IY2K6FQK6A%26tag%3Dmike0c7-20%26linkCode%3Dxm2%26camp%3D2025%26creative%3D386001%26creativeASIN%3DB00SXOOD6W" "49.99" "3.0" "10.6" "10.4" "2.6"]
   ["151902985015" "B001QOGXPK" "EDUCATIONAL INSIGHTS BLURT!" "TOYS_AND_GAMES" "17133" "http://www.amazon.com/gp/product/B001QOGXPK" "http://www.amazon.com/gp/offer-listing/B001QOGXPK%3FSubscriptionId%3DAKIAJFQYE5IY2K6FQK6A%26tag%3Dmike0c7-20%26linkCode%3Dxm2%26camp%3D2025%26creative%3D386001%26creativeASIN%3DB001QOGXPK" "12.81" "2.7" "11.8" "6.9" "1.6"]
   ["085955090184" "B00OLGL4M2" "\"Doc McStuffins Kids Rollerskate" " Junior Size 6-12 with Knee Pads\"" "TOYS_AND_GAMES" "246525" "http://www.amazon.com/gp/product/B00OLGL4M2" "0" "50.58" "3.7" "11.4" "9.8" "1.6"]])

(comment
  (deftest test-run
    (let [credentials (-> "test-resources/properties.edn"
                          (slurp)
                          (edn/read-string)
                          (:credentials))
          in "in.txt"
          out "out.txt"]
      (run credentials "test-resources/upcs.txt" out)
      (with-open [rdr (io/reader out)]
        (is (= expected (mapv #(split % #",") (line-seq rdr))))))))
