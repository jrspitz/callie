(ns callie.core
  (:import goog.date.Date
           goog.i18n.DateTimeSymbols_en
           goog.date.Interval)
  (:require [hipo.core :as hipo]
            [clojure.string :as string]
            [goog.dom :as dom]))

(enable-console-print!)
(def DateSym goog.i18n.DateTimeSymbols_en)
;(js/console.log DateSym) 

(defn this-month []
  (let [d (Date.)]
    (.setDate d 1)
    d))

(defonce calendar-state (atom {:active-date (this-month)}))

(def a-day
  (goog.date.Interval. goog.date.Interval.DAYS 1))

(def plus-month
  (goog.date.Interval. goog.date.Interval.MONTHS 1))

(def minus-month 
  (goog.date.Interval. goog.date.Interval.MONTHS -1))

(defn next-day [d]
  (let [nd (.clone d)]
    (.add nd a-day)
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

(defn start-day
  "First day to display on the calendar.  It's got to be a sunday"
  [d]
  (let [a-day-ago (goog.date.Interval. goog.date.Interval.DAYS -1)]
    (loop [cur-day (.clone d)]
      (if (zero? (.getDay cur-day))
        cur-day
        (do (.add cur-day a-day-ago)
            (recur cur-day))))))

;; Month and Year string
(defn month-year-string [d]
  (let [m (.getMonth d)
        YYYY (.getFullYear d)]
    (str (aget DateSym.MONTHS m) " " YYYY)))

(defn month-year-span [d]
  [:span {:id "cal-month-year"} (month-year-string d)])

(defn is-today? [d]
  (.equals d today))

(defn is-active-month? [d]
  (= (.getMonth (:active-date @calendar-state))
     (.getMonth d)))

(def weekday-row 
  [:tr.weekdays
   (for [x DateSym.SHORTWEEKDAYS] 
     [:th x])])

(defn day-classes [d]
  (cond-> []
    (is-today? d) (conj "today")
    (is-active-month? d) (conj "active-month")
    (not (is-active-month? d)) (conj "not-active-month")))

(defn day-cell [d]
  [:td 
   {:class (string/join " " (day-classes d))} 
   [:span {:class "daynumber"} (.getDate d)]])

(defn calendar-body 
  "6 weeks of days (The body of the calendar.)"
  [d]
  (let [sd (start-day d)
        all-days (take 6 (partition 7 (iterate next-day sd)))]
    (for [week all-days]
      [:tr 
       (for [day week] [:td (day-cell day)])])))

(def prev-next-today-btns 
  (list 
    [:div {:class "btn-grp pull-right"}
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
  [d]
    [:div.callie-view
     prev-next-today-btns 
     [:h2 {:class "month-year"} (month-year-span d)]
     [:table.table.table-bordered
      [:thead weekday-row]
      [:tbody (calendar-body d)]]])

; Not using this.  Adding event listeners inline.
(defn addEventListeners! []
  (let [prev-btn (dom/getElement "cal-prev")
        next-btn (dom/getElement "cal-next")
        this-month-btn (dom/getElement "cal-this-month")]
    (.addEventListener this-month-btn "click" this-month!)
    (.addEventListener prev-btn "click" prev-month!)
    (.addEventListener next-btn "click" next-month!)))

(defn inject-cal! [d] 
  (let [el (hipo/create (calendar-hiccup d))
        container (.getElementById js/document "calendar")] 
  (dom/removeChildren container)
  (.appendChild (.getElementById js/document "calendar") el)))

(add-watch calendar-state :redraw
          (fn [key atom old-state new-state]
            (js/console.log (:active-date new-state))
            (inject-cal! (:active-date new-state)))) 

(inject-cal! (:active-date @calendar-state))

;(js/console.log @calendar-state)
;(js/console.log calendar-state)
;(js/console.log (calendar-hiccup (:active-date @calendar-state)))
(cljs.pprint/pprint (calendar-hiccup (:active-date @calendar-state)))
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
