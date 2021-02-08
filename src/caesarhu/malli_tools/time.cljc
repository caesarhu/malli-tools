(ns caesarhu.malli-tools.time
  (:refer-clojure :exclude [time])
  (:require [malli.core :as m]
            [malli.transform :as mt]
            [tick.alpha.api :as t])
  (:import (java.time Instant OffsetDateTime LocalDateTime ZonedDateTime LocalDate OffsetTime LocalTime YearMonth Year ZoneId)
           (java.util Date Locale)
           (java.time.format DateTimeFormatter)))

(def format-key :malli/format)

(def year (m/-simple-schema {:pred t/year? :type :year}))
(def year-month (m/-simple-schema {:pred t/year-month? :type :year-month}))
(def date (m/-simple-schema {:pred t/date? :type :date}))
(def date-time (m/-simple-schema {:pred t/date-time? :type :date-time}))
(def time (m/-simple-schema {:pred t/time? :type :time}))
(def offset-date-time (m/-simple-schema {:pred t/offset-date-time? :type :offset-date-time}))
(def zoned-date-time (m/-simple-schema {:pred t/zoned-date-time? :type :zoned-date-time}))
(def instant (m/-simple-schema {:pred t/instant? :type :instant}))

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
      :instant time-encoder}
     :decoders
     {:year time-decoder
      :year-month time-decoder
      :date time-decoder
      :date-time time-decoder
      :time time-decoder
      :offset-date-time time-decoder
      :zoned-date-time time-decoder
      :instant time-decoder}}))

(def time-schemas
  {:date date,
   :date-time date-time,
   :zoned-date-time zoned-date-time,
   :instant instant,
   :offset-date-time offset-date-time,
   :time time,
   :year year,
   :year-month year-month})