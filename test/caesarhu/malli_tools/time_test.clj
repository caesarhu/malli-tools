(ns caesarhu.malli-tools.time-test
  (:refer-clojure :exclude [time])
  (:require [clojure.test :refer :all]
            [malli.core :as m]
            [tick.alpha.api :as t]
            [caesarhu.malli-tools.time :refer :all]
            [java-time :as jt])
  (:import (clojure.lang ExceptionInfo)
           (java.util Date)
           (java.time LocalTime LocalDateTime OffsetDateTime ZonedDateTime YearMonth Year Instant LocalDate OffsetTime Clock ZoneId)
           (java.time.format DateTimeFormatter)))

(deftest construction-test
  (testing "Valid schemas are constructed"
    (are [x] (satisfies? m/Schema (m/schema [x]))
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
  (let [clock (Clock/fixed (jt/instant #inst "1999-12-13T14:15:16.178") (ZoneId/of "GMT+6"))]
    (testing "encodes the objects"
      (are [expected schema value]
        (= expected (m/encode schema value time-transformer))
        "20:15:16.178" time (jt/local-time clock)
        "1999-12-13T20:15:16.178" date-time (jt/local-date-time clock)
        "1999-12-13T20:15:16.178+06:00" offset-date-time (jt/offset-date-time clock)
        "1999-12-13T20:15:16.178+06:00[GMT+06:00]" zoned-date-time (jt/zoned-date-time clock)
        "1999-12-13" date (jt/local-date clock)
        "1999-12" year-month (jt/year-month clock)
        "1999" year (jt/year clock)
        "1999-12-13T14:15:16.178Z" instant (jt/instant clock)))
    (testing "encode with custom formatters for the objects"
      (are [schema value]
        (= "0820" (m/encode (vector schema {format-key "hhHH"}) value time-transformer))
        time (jt/local-time clock)
        date-time  (jt/local-date-time clock)
        offset-date-time (jt/offset-date-time clock)
        zoned-date-time (jt/zoned-date-time clock))
      (are [schema value]
        (= "1999" (m/encode (vector schema {format-key "yyyy"}) value time-transformer))
        ;instant (jt/instant clock)
        date (jt/local-date clock)
        year-month (jt/year-month clock)
        year (jt/year clock)))))

(deftest malli-time-test
  (testing "testing caesarhu.malli-tools.time"
    (is (= "2000"
           (m/encode year (t/year 2000) time-transformer)))
    (is (= (t/year 2000)
           (m/decode year
                     (m/encode year (t/year 2000) time-transformer)
                     time-transformer)))
    (is (= "2000-02"
           (m/encode year-month (t/year-month "2000-02") time-transformer)))
    (is (= (t/year-month "2000-02")
           (m/decode year-month "2000-02" time-transformer)))
    (is (= "2000-02-08"
           (m/encode date (jt/local-date 2000 2 8) time-transformer)))
    (is (= (jt/local-date 2000 2 8)
           (m/decode date "2000-02-08" time-transformer)))
    (is (= "2021-02-08T17:50:22.357792"
           (m/encode date-time (t/date-time "2021-02-08T17:50:22.357792") time-transformer)))
    (is ())))
