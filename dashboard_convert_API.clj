(ns dashboard_convert_API
  (:require [clojure.data.json :as json]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clj-http.client :as client]
            [clojure.test :refer [deftest is]]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

;error handling for url failure
(defn try-post
  [url params]
  (try (client/post url params)
       (catch Exception e
         {:status :exception
          :body (.getMessage e)})))

;error handling for non-200 error
(defn get-auth-token
  [url params]
  (let [response (try-post url params)
        {:keys [status body]} response]
    (case status
      200 (get (json/read-str body) "access_token")
      (println "Non-200 Status"))))

;takes in a sequence of dashboard ids and outputs a sequence of widgets lists
(defn get-data
  [base-url ids token]
  (map #(let [response (client/get (str base-url % "/export/dash")
                              {:headers {:authorization (str "Bearer " token)}})
         {:keys [status body]} response]
     (case status
       200 (get (json/read-str body) "widgets")
       ;should retry if get a non-200 error, after max retry - error message should be descriptive or in log
       "Non-200 error")) ids))

;using id-list from config file
(def cli-options
  [])

;make map of alias key value pairs
(defn create-alias-map-widgets [widgets]
  (into {}
        (mapcat (fn [widget]
            (mapcat (fn [panel]
                (mapcat (fn [item]
                          (map (fn [kv]
                                 [(first kv) (get (second kv) "dim")])
                               (get-in item ["jaql" "context"])))
                        (get-in panel ["items"])))
                    (get-in widget ["metadata" "panels"])))
                widgets)))

(defn replace-formula [alias-map formula]
  (clojure.string/replace formula #"\[\w\w\w\w\w-\w\w\w\]" alias-map))

(defn convert-widget [alias-map widget]
  (mapcat (fn [panel]
            (mapv (fn [item item-num]
                    ;(println ">>> item " item-num)
                    (if (= item-num 0)
                      [(get widget "title")
                      (get widget "type")
                      (get panel "name")
                      (or (get-in item ["jaql" "dim"])
                          (get-in item ["jaql" "title"]))
                      (if (get-in item ["jaql" "dim"])
                        ""
                        (replace-formula alias-map (get-in item ["jaql" "formula"])))]
                      [""
                       ""
                       ""
                       (or (get-in item ["jaql" "dim"])
                           (get-in item ["jaql" "title"]))
                       (if (get-in item ["jaql" "dim"])
                         ""
                         (replace-formula alias-map (get-in item ["jaql" "formula"])))]))
                  (get panel "items") (range)))
          (get-in widget ["metadata" "panels"])))

(defn convert-widgets [widgets alias-map]
  (mapcat #(convert-widget alias-map %) widgets))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        config-path (first arguments)
        config-map (json/read-str (slurp config-path) :key-fn keyword)
        {:keys [base-url auth-url username password ids]} config-map
        params {:form-params {:username username
                              :password password}}
        token (get-auth-token auth-url params)
        widgets-seq (get-data base-url ids token)]
    (doall (map
       (fn [widgets id]
         (with-open [writer (io/writer (str id ".csv"))]
           (csv/write-csv writer
                          (convert-widgets widgets (create-alias-map-widgets widgets))))
       widgets-seq ids))))




