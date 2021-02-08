(ns caesarhu.malli-tools.time-test
  (:refer-clojure :exclude [time])
  (:require [clojure.test :refer :all]
            [malli.core :as m]
            [tick.alpha.api :as t]
            [caesarhu.malli-tools.time :refer :all]
            [java-time :as jt]))

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
           (m/encode date-time (t/date-time "2021-02-08T17:50:22.357792") time-transformer)))))
