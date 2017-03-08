(ns callie.dev
  (:import goog.date.DateTime
           goog.date.Interval
           goog.Uri
           goog.net.XhrIo)
  (:require [callie.core :as callie]))
  
(def rem-uri "http://rem-rest-api.herokuapp.com/api/events")

(def credentialed-xhr 
  (let [xhr (XhrIo.)]
    (.setWithCredentials xhr true)
    xhr))

(defn random-event [year month]
  ; months are zero indexed in goog.date but not this fn
  (let [event-date (DateTime. year (dec month) 1)
        rand-interval (Interval. Interval.HOURS (rand-int (* 28 24)))]
    (.add event-date rand-interval)
    {:title (.toString (random-uuid)) :start (.toIsoString event-date true)}))

(defn random-events [quantity year month]
  (for [x (range quantity)]
    (random-event year month)))

(defn put-random-event [year month]
  (let [;xhr (XhrIo.)
        data (-> (random-event year month)
                 clj->js
                 js/JSON.stringify)]
    ;(.setWithCredentials xhr true)
    ;(.send xhr
    ;       rem-uri
    ;       "POST"
    ;       data)))
    (XhrIo.send rem-uri
                #(print %)
                "POST"
                data 
                nil
                0
                true)))

(defn put-random-events [qty year month]
  (repeatedly qty #(put-random-event year month)))

(defn pop-random-events [qty year month]
  (callie/set-events! (clj->js (random-events qty year month))))
