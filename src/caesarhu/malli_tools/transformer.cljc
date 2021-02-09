(ns caesarhu.malli-tools.transformer
  (:require
    [malli.core :as m]
    [malli.transform :as mt]))


(defn trans-value-transformer
  ([]
   (trans-value-transformer nil))
  ([{:keys [key transfers] :or {key :transfer}}]
   (let [get-transfer (fn [schema]
                        (let [transfer (some-> schema m/properties key)]
                          (if (some? transfer) transfer (some->> schema m/type (get transfers) (#(% schema))))))
         set-transfer {:compile (fn [schema _]
                                  (if-some [transfer (get-transfer schema)]
                                    (if (fn? transfer)
                                      (fn [x] (transfer x))
                                      (fn [x] transfer))))}
         add-transfers {:compile (fn [schema _]
                                   (let [transfers (->> (m/children schema)
                                                        (keep (fn [[k {transfer key} v]]
                                                                (if-some [transfer (if (some? transfer) transfer (get-transfer v))]
                                                                  [k transfer])))
                                                        (into {}))]
                                     (if (seq transfers)
                                       (fn [x]
                                         (if (map? x)
                                           (reduce-kv
                                             (fn [acc k v]
                                               (if-not (contains? x k)
                                                 (assoc acc k v)
                                                 acc))
                                             x transfers)
                                           x)))))}]
     (mt/transformer
       {:default-decoder set-transfer
        :default-encoder set-transfer}
       {:decoders {:map add-transfers}
        :encoders {:map add-transfers}}))))
