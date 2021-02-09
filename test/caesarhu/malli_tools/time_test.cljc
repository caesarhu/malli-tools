(ns caesarhu.malli-tools.time-test
  (:refer-clojure :exclude [time])
  (:require
    [clojure.test
     :refer [deftest is are testing run-tests]
     :refer-macros [deftest is are testing run-tests]]
    [tick.alpha.api :as t]
    [tick.locale-en-us]
    [malli.core :as m]
    [caesarhu.malli-tools.time :refer
     [date time date-time offset-date-time zoned-date-time year-month
      year instant time-transformer format-key]]
    [malli.transform :as mt])
  #?(:clj
     (:import [java.time.format DateTimeFormatter]
              [java.util Locale])))

(deftest construction-test
  (testing "Valid schemas are constructed"
    (are [x] (m/schema? (m/schema x))
             date
             time
             offset-date-time
             zoned-date-time
             date-time
             year-month
             year
             instant)))

(deftest validation-test
  (testing "schema validates objects"
    (are [x y] (m/validate x y)
               date (t/date)
               time (t/time)
               date-time (t/date-time)
               offset-date-time (t/offset-date-time)
               zoned-date-time (t/zoned-date-time)
               year-month (t/year-month)
               year (t/year)
               instant (t/instant))))

(deftest transformation-test
  (let [clock (t/instant "1999-12-13T14:15:16.178Z")
        zoned-clock (t/in clock "GMT+6")]
       (testing "encodes the objects"
         (are [expected schema value]
           (= expected (m/encode schema value time-transformer))
           "20:15:16.178" time (t/time zoned-clock)
           "1999-12-13T20:15:16.178" date-time (t/date-time zoned-clock)
           "1999-12-13T20:15:16.178+06:00" offset-date-time (t/offset-date-time zoned-clock)
           "1999-12-13T20:15:16.178+06:00[GMT+06:00]" zoned-date-time zoned-clock
           "1999-12-13" date (t/date zoned-clock)
           "1999-12" year-month (-> zoned-clock t/date t/year-month)
           "1999" year (t/year zoned-clock)
           "1999-12-13T14:15:16.178Z" instant clock))
       (testing "encode with custom formatters for the objects"
         (are [schema value]
           (= "0820" (m/encode (vector schema {format-key "hhHH"}) value time-transformer))
           time (t/time zoned-clock)
           date-time  (t/date-time zoned-clock)
           offset-date-time (t/offset-date-time zoned-clock)
           zoned-date-time zoned-clock)
         (are [schema value]
           (= "1999" (m/encode (vector schema {format-key "yyyy"}) value time-transformer))
           instant (t/instant zoned-clock)
           date (t/date zoned-clock)
           year-month (t/year-month (t/date zoned-clock))
           year (t/year zoned-clock)))
       (testing "decodes the objects"
         (are [value schema expected]
           (= expected (m/decode schema value time-transformer))
           "20:15:16.178" time (t/time zoned-clock)
           "1999-12-13T20:15:16.178" date-time (t/date-time zoned-clock)
           "1999-12-13T20:15:16.178+06:00" offset-date-time (t/offset-date-time zoned-clock)
           "1999-12-13T20:15:16.178+06:00[GMT+06:00]" zoned-date-time zoned-clock
           "1999-12-13" date (t/date zoned-clock)
           "1999-12" year-month (t/year-month (t/date zoned-clock))
           "1999" year (t/year zoned-clock)
           "1999-12-13T14:15:16.178Z" instant (t/instant zoned-clock)))))

(deftest string-transformer-test
  (let [clock (t/instant "1999-12-13T14:15:16.178Z")
        zoned-clock (t/in clock "GMT+6")]
    (testing "encode objects mt/string-transformer"
      (are [expected schema value]
        (= expected (m/encode schema value mt/string-transformer))
        "20:15:16.178" time (t/time zoned-clock)
        "1999-12-13T20:15:16.178" date-time (t/date-time zoned-clock)
        "1999-12-13T20:15:16.178+06:00" offset-date-time (t/offset-date-time zoned-clock)
        "1999-12-13T20:15:16.178+06:00[GMT+06:00]" zoned-date-time zoned-clock
        "1999-12-13" date (t/date zoned-clock)
        "1999-12" year-month (-> zoned-clock t/date t/year-month)
        "1999" year (t/year zoned-clock)
        "1999-12-13T14:15:16.178Z" instant clock))
    (testing "encode objects mt/string-transformer"
      (are [value schema expected]
        (= expected (m/decode schema value mt/string-transformer))
        "20:15:16.178" time (t/time zoned-clock)
        "1999-12-13T20:15:16.178" date-time (t/date-time zoned-clock)
        "1999-12-13T20:15:16.178+06:00" offset-date-time (t/offset-date-time zoned-clock)
        "1999-12-13T20:15:16.178+06:00[GMT+06:00]" zoned-date-time zoned-clock
        "1999-12-13" date (t/date zoned-clock)
        "1999-12" year-month (t/year-month (t/date zoned-clock))
        "1999" year (t/year zoned-clock)
        "1999-12-13T14:15:16.178Z" instant (t/instant zoned-clock)))))

(deftest json-transformer-test
  (let [clock (t/instant "1999-12-13T14:15:16.178Z")
        zoned-clock (t/in clock "GMT+6")]
    (testing "encode objects mt/string-transformer"
      (are [expected schema value]
        (= expected (m/encode schema value mt/json-transformer))
        "20:15:16.178" time (t/time zoned-clock)
        "1999-12-13T20:15:16.178" date-time (t/date-time zoned-clock)
        "1999-12-13T20:15:16.178+06:00" offset-date-time (t/offset-date-time zoned-clock)
        "1999-12-13T20:15:16.178+06:00[GMT+06:00]" zoned-date-time zoned-clock
        "1999-12-13" date (t/date zoned-clock)
        "1999-12" year-month (-> zoned-clock t/date t/year-month)
        "1999" year (t/year zoned-clock)
        "1999-12-13T14:15:16.178Z" instant clock))
    (testing "encode objects mt/string-transformer"
      (are [value schema expected]
        (= expected (m/decode schema value mt/json-transformer))
        "20:15:16.178" time (t/time zoned-clock)
        "1999-12-13T20:15:16.178" date-time (t/date-time zoned-clock)
        "1999-12-13T20:15:16.178+06:00" offset-date-time (t/offset-date-time zoned-clock)
        "1999-12-13T20:15:16.178+06:00[GMT+06:00]" zoned-date-time zoned-clock
        "1999-12-13" date (t/date zoned-clock)
        "1999-12" year-month (t/year-month (t/date zoned-clock))
        "1999" year (t/year zoned-clock)
        "1999-12-13T14:15:16.178Z" instant (t/instant zoned-clock)))))