(ns user
  (:refer-clojure :exclude [time])
  (:require [malli.core :as m]
            [malli.transform :as mt]
            [tick.alpha.api :as t]
            [kaocha.repl :as k]))

;;; test

(defn unit-test
  []
  (k/run :unit))