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

(defn random-event 
  [year month]
  ; months are zero indexed in goog.date but not this fn
  (let [event-date (DateTime. year (dec month) 1)
        rand-interval (Interval. Interval.HOURS (rand-int (* 28 24)))]
    (.add event-date rand-interval)
    {:title (.toString (random-uuid))
     :start (.toIsoString event-date true)
     :className "addition-event-class"}))

(defn random-events [quantity year month]
  (for [x (range quantity)]
    (random-event year month)))

(defn pop-random-events [qty year month]
  (callie/set-events! (clj->js (random-events qty year month))))

(defn random-event-from-datetime
  [dt]
  ; months are zero indexed in goog.date but not this fn
  (let [init-date (.clone dt)
        rand-interval (Interval. Interval.HOURS (rand-int (* 42 24)))]
    (.add init-date rand-interval)
    {:title (.toString (random-uuid)) :start (.toIsoString init-date true) :className "extra-event-class"}))


(defn random-events-from-datetime [quantity dt]
  (for [x (range quantity)] 
    (random-event-from-datetime dt)))


(defn random-event-source [start end callback]
  (print "random-event-source called")
  (let [start (DateTime.fromRfc822String start)
        events (random-events-from-datetime 20 start)
        js-events (clj->js events)]
    (print js-events)
    (callback js-events)))


(defn set-random-event-source! []
  (callie.core/set-event-source! random-event-source))


(defn dummy-click [event x]
  (console.log "dummy-click-event")
  (console.log event)
  (console.log x))


(defn set-dummy-click! []
  (callie.core/set-event-click! dummy-click))
;(set-event-click! dummy-click)

(defn dummy-event-source [start end cb]
  (js/console.log "dummy-event-source called")
  ;(js/console.log "start param")
  ;(js/console.log start)
  ;(js/console.log "end param")
  ;(js/console.log end)
  ;(js/console.log "callback")
  ;(js/console.log cb)
  )

;(set-event-source! dummy-event-source)


