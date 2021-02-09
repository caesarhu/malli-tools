(ns caesarhu.malli-tools.time
  (:refer-clojure :exclude [time])
  (:require [malli.core :as m]
            [malli.transform :as mt]
            [tick.alpha.api :as t]
            #?(:cljs [java.time.format :refer [DateTimeFormatter]]))
  #?(:clj
     (:import [java.time.format DateTimeFormatter]
              [java.util Locale])))

(def format-key :malli/format)

(defn formatter?
  [x]
  (instance? DateTimeFormatter x))

(def default-format
  {:year "yyyy"
   :year-month "yyyy-MM"
   :date :iso-local-date
   :date-time :iso-local-date-time
   :time :iso-local-time
   :offset-date-time :iso-offset-date-time
   :zoned-date-time :iso-zoned-date-time
   :instant :iso-instant})

(def parse-fn-table
  {:year (fn [s formatter]
           (if (formatter? formatter)
             (cljc.java-time.year/parse s formatter)
             (t/year s)))
   :year-month (fn [s formatter]
                 (if (formatter? formatter)
                   (cljc.java-time.year-month/parse s formatter)
                   (t/year-month s)))
   :date (fn [s formatter]
           (if (formatter? formatter)
             (cljc.java-time.local-date/parse s formatter)
             (t/date s)))
   :date-time (fn [s formatter]
                (if (formatter? formatter)
                  (cljc.java-time.local-date-time/parse s formatter)
                  (t/date-time s)))
   :time (fn [s formatter]
           (if (formatter? formatter)
             (cljc.java-time.local-time/parse s formatter)
             (t/time s)))
   :offset-date-time (fn [s formatter]
                       (if (formatter? formatter)
                         (cljc.java-time.offset-date-time/parse s formatter)
                         (t/offset-date-time s)))
   :zoned-date-time (fn [s formatter]
                      (if (formatter? formatter)
                        (cljc.java-time.zoned-date-time/parse s formatter)
                        (t/zoned-date-time s)))
   :instant (fn [s _]
              (t/instant s))})

(def time-encoder
  {:compile
   (fn [schema _]
     (let [default (get default-format (m/type schema))
           {:keys [malli/format]
            :or {format default}} (m/properties schema)
           formatter (if (formatter? format)
                       format
                       (t/formatter format))]
       {:enter (fn [obj]
                 (if (m/validate schema obj)
                   (t/format formatter obj)
                   obj))}))})

(def time-decoder
  {:compile
   (fn [schema _]
     (let [default (get default-format (m/type schema))
           {:keys [malli/format]
            :or {format default}} (m/properties schema)
           formatter (if (formatter? format)
                       format
                       (t/formatter format))
           parse (get parse-fn-table (m/type schema))]
       {:enter (fn [s]
                 (if (string? s)
                   (parse s formatter)
                   s))}))})

(defn time-transformer
  "Custom malli transformer for working with times"
  []
  (mt/transformer
    {:encoders
     {:year time-encoder
      :year-month time-encoder
      :date time-encoder
      :date-time time-encoder
      :time time-encoder
      :offset-date-time time-encoder
      :zoned-date-time time-encoder
      :instant {:compile
                (fn [schema _]
                  (let [default (get default-format (m/type schema))
                        {:keys [malli/format]
                         :or {format default}} (m/properties schema)
                        formatter (if (formatter? format)
                                    format
                                    (t/formatter format))]
                    {:enter (fn [obj]
                              (if (m/validate schema obj)
                                (if (= format :iso-instant)
                                  (t/format formatter obj)
                                  (t/format formatter (t/zoned-date-time obj)))
                                obj))}))}}
     :decoders
     {:year time-decoder
      :year-month time-decoder
      :date time-decoder
      :date-time time-decoder
      :time time-decoder
      :offset-date-time time-decoder
      :zoned-date-time time-decoder
      :instant time-decoder}}))

(defn ->time-schema
  [v pred]
  (let [v-type (keyword (name v))]
    (m/-simple-schema
      {:pred pred
       :type v-type
       :type-properties {:encode/string #(str %)
                         :decode/string #((get parse-fn-table v-type) % nil)
                         :encode/json #(str %)
                         :decode/json #((get parse-fn-table v-type) % nil)}})))

(def year (->time-schema 'year t/year?))
(def year-month (->time-schema 'year-month t/year-month?))
(def date (->time-schema 'date t/date?))
(def date-time (->time-schema 'date-time t/date-time?))
(def time (->time-schema 'time t/time?))
(def offset-date-time (->time-schema 'offset-date-time t/offset-date-time?))
(def zoned-date-time (->time-schema 'zoned-date-time t/zoned-date-time?))
(def instant (->time-schema 'instant t/instant?))

(def time-schemas
  {:date date,
   :date-time date-time,
   :zoned-date-time zoned-date-time,
   :instant instant,
   :offset-date-time offset-date-time,
   :time time,
   :year year,
   :year-month year-month})
