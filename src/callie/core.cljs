(ns callie.core
  (:import goog.date.Date
           goog.date.DateTime
           goog.i18n.DateTimeFormat
           goog.date.Interval
           goog.net.XhrIo
           goog.Uri)
  (:require [hipo.core :as hipo]
            [clojure.string :as string]
            [goog.dom :as dom]))

(enable-console-print!)

(def WEEKDAY-NAMES goog.i18n.DateTimeSymbols.STANDALONESHORTWEEKDAYS)
(def month-week-formatter (DateTimeFormat. "MMMM y"))
(def event-time-formatter (DateTimeFormat. "H:mm a"))

(defn this-month []
  (let [d (Date.)]
    (.setDate d 1)
    d))

(defonce calendar-state (atom {:active-date (this-month)
                               :events []
                               :event-source nil
                               :event-transform nil
                               :event-callback nil}))

(def plus-day
  (goog.date.Interval. goog.date.Interval.DAYS 1))

(def minus-day 
  (goog.date.Interval. goog.date.Interval.DAYS -1))

(def plus-month
  (goog.date.Interval. goog.date.Interval.MONTHS 1))

(def minus-month 
  (goog.date.Interval. goog.date.Interval.MONTHS -1))


;; Date math wrapers to make these functions immutable.
(defn next-day [d]
  (let [nd (.clone d)]
    (.add nd plus-day)
    nd))

(defn prev-day [d]
  (let [nd (.clone d)]
    (.add nd minus-day)
    nd))

(defn next-month [d]
  (let [nd (.clone d)]
    (.add nd plus-month)
    nd))

(defn prev-month [d]
  (let [nd (.clone d)]
    (.add nd minus-month)
    nd))


(defn next-month! []
  (swap! calendar-state update-in [:active-date] next-month))

(defn prev-month! []
  (swap! calendar-state update-in [:active-date] prev-month))

(defn this-month! []
  (swap! calendar-state assoc :active-date (this-month)))

(def today (Date.))

(defn first-day
  "First day to display on the calendar. Count back from the given date until
  the day is sunday (0)."
  [d]
  (loop [d d]
    (if (zero? (.getDay d))
      d
      (recur (prev-day d)))))

(defn is-event-on-date? 
  [event date]
  (let [event-date (Date. (:start event))]
    (.equals date event-date)))

(defn days-events-filter
  "Get the events for the given day"
  [d events]
  (filter #(is-event-on-date? % d) events))

(defn month-year-span [calendar-state]
  (let [active-date (:active-date calendar-state)]
    [:span {:id "cal-month-year"} (.format month-week-formatter active-date)]))

(defn is-today? [d]
  (.equals d today))

(defn is-active-month? [d calendar-state]
  (= (.getMonth (:active-date calendar-state))
     (.getMonth d)))

(def weekday-row 
  [:tr.weekdays
   (for [x WEEKDAY-NAMES] 
     [:th x])])

(defn day-classes [d calendar-state]
  (cond-> []
    (is-today? d) (conj "today")
    (is-active-month? d calendar-state) (conj "active-month")
    (not (is-active-month? d)) (conj "not-active-month")))

(defn event-div [ev calendar-state]
  [:div {:class ["event"]}
   [:a [:span.time (.format event-time-formatter (:start ev))]
       [:span.title (:title ev)]]])

(defn day-cell [d calendar-state]
  (let [days-events (days-events-filter d (:events calendar-state))]
    [:td 
     {:class (string/join " " (day-classes d calendar-state))} 
     [:span {:class "daynumber"} (.getDate d)]
     (for [ev days-events]
       (event-div ev calendar-state))]))

(defn calendar-body 
  "6 weeks of days (The body of the calendar.)"
  [calendar-state]
  (let [start-day (first-day (:active-date calendar-state))
        all-days (take 6 (partition 7 (iterate next-day start-day)))]
    (for [week all-days]
      [:tr 
       (for [day week] (day-cell day calendar-state))])))

(def prev-next-today-btns 
  (list 
    [:div {:class "btn-group pull-right"}
        [:button#cal-prev {:class "btn btn-default"
                           :type "button" 
                           :on-click prev-month!} "←"]
        [:button#cal-next {:class "btn btn-default"
                           :type "button"
                           :on-click next-month!} "→"]]
        [:button#cal-this-month {:class "btn btn-default pull-right"
                                 :type "button"
                                 :on-click this-month!} "This Month"]))

(defn calendar-hiccup
  "Hiccup tempate for the calendar"
  [calendar-state]
    [:div.callie-view
     prev-next-today-btns 
     [:h2 {:class "month-year"} (month-year-span calendar-state)]
     [:table.table.table-bordered
      [:thead weekday-row]
      [:tbody (calendar-body calendar-state)]]])


(defn inject-cal! [calendar-state] 
  (let [el (hipo/create (calendar-hiccup calendar-state))
        container (.getElementById js/document "calendar")] 
    (dom/removeChildren container)
    (.appendChild (.getElementById js/document "calendar") el)))

(add-watch calendar-state :redraw
          (fn [key atom old-state new-state]
            (js/console.log (pr (calendar-hiccup new-state)))
            (js/console.log new-state)
            (inject-cal! new-state))) 

(inject-cal! @calendar-state)


(defn set-event-source!  
  [uri]
  (swap! calendar-state update-in [:event-source] uri))

(defn read-events [events]
  (for [ev events]
      (-> (js->clj ev :keywordize-keys true)
          (update-in [:start] DateTime.fromRfc822String))))

(defn set-events!
  [events]
  (let [events (read-events events)]
    (swap! calendar-state assoc :events events)))

(defn set-event-transform!
  [f]
  (swap! calendar-state update-in [:event-transform] f))

;(defn fetch-events!
;  []
;  (when-let [fetch-fn (:event-source @calendar-state)]
;    (.send goog.net.XhrIo endpoint "GET" nil 


;(js/console.log @calendar-state)
;(js/console.log (:active-date @calendar-state))
;(js/console.log calendar-state)
;(js/console.log (calendar-hiccup (:active-date @calendar-state)))
;(cljs.pprint/pprint (calendar-hiccup (:active-date @calendar-state)))
;(def testhiccup 
;  (list [:div {:id "test1"}]
;        [:div {:id "test2"} (list [:div {:id "nesty1"}] 
;                                  [:div {:id "nesty2"}])]))
;(js/console.log testhiccup)
;(js/console.log (hipo/create testhiccup))


(defn on-js-reload []
  (js/console.log "reloading")
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
