(ns collection-json.client
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]))

(defrecord Collection [version href links items queries template error])
(defrecord Link [rel href prompt render])
(defrecord Item [href data links])
(defrecord Data [name value prompt])
(defrecord Query [rel href prompt data name])
(defrecord CollectionError [title code message])

(defn- make-link
  [link]
  (if link (->Link (:rel link)
                   (:href link)
                   (:prompt link)
                   (:render link))))

(defn- make-data
  [data]
  (if data (->Data (:name data)
                   (:value data)
                   (:prompt data))))

(defn- make-item
  [item]
  (if item (->Item (:href item)
                   (map make-data (:data item))
                   (map make-link (:links item)))))

(defn- make-query
  [query]
  (if query (->Query (:rel query)
                     (:href query)
                     (:prompt query)
                     (map make-data (:data query))
                     (:name query))))

(defn- make-template
  [template]
  (if template (map make-data template)))

(defn- make-error
  [error]
  (if error (->CollectionError (:title error)
                               (:code error)
                               (:message error))))

(defn get-collection
  "Get the Collection+JSON at the specified URL. Options are a map as passed to clj-http.client"
  ([url] (get-collection url {}))
  ([url options]
     (let [resp (client/get url options)
           body (:collection (json/read-str (:body resp) :key-fn keyword))]
       (->Collection (:version body)
                     (:href body)
                     (map make-link (:links body))
                     (map make-item (:items body))
                     (map make-query (:queries body))
                     (make-template (:template body))
                     (make-error (:error body))))))

(defn follow-link
  [rel coll options]
  (let [link (first (filter #(= rel (:rel %)) (:links coll)))]
    (get-collection (:href link) options)))

(defn get-value
  [name item]
  (:value (first (filter #(= name (:name %)) (:data item)))))

(defn get-prompt
  [name item]
  (:prompt (first (filter #(= name (:name %)) (:data item)))))

(defn get-names
  [item]
  (map :name (:data item)))

(defn as-map
  [^Item item]
  (into {} (map #(hash-map (keyword %) (get-value % item)) (get-names item))))

(defn items-as-maps
  [coll]
  (map as-map (:items coll)))
