(ns kerbal.core
  (:require [clojure.reflect :as reflect]
            [clojure.pprint :refer [print-table]]
            [clojure.string :as str])
  (:import [krpc.client Connection]
           [krpc.client.services KRPC SpaceCenter UI]))

(defn inspect [x]
  (->> x
       reflect/reflect
       :members
       (sort-by :name)))

(defonce ^:dynamic *connection* (atom nil))

(defn log! [& args]
  (apply println args)
  (.message (UI/newInstance @*connection*)
            (str/join " " args)
            30
            krpc.client.services.UI$MessagePosition/TOP_LEFT
            (org.javatuples.Triplet. 1.0 1.0 1.0)
            (float 16.0)))

(defn connect!
  ([host rpc-port stream-port]
   (connect! "Remote" host rpc-port stream-port))
  ([name host rpc-port stream-port]
   (reset! *connection*
           (Connection/newInstance name host rpc-port stream-port))))

(defn disconnect! []
  (.close @*connection*)
  (reset! *connection* nil))

(defn get-krpc []
  (KRPC/newInstance @*connection*))

(defn krpc-version []
  (-> (get-krpc) .getStatus .getVersion))

(defn get-clients []
  (.getClients (get-krpc)))

(defn get-space-center []
  (SpaceCenter/newInstance @*connection*))

(defn get-vessel []
  (let [space-center (get-space-center)]
    (.getActiveVessel space-center)))

(defn get-control [vessel]
  (.getControl vessel))

(defn get-auto-pilot [vessel]
  (.getAutoPilot vessel))

(defn get-flight
  ([]
   (get-flight (get-vessel) nil))
  ([vessel]
   (get-flight vessel nil))
  ([vessel frame]
   (.flight vessel frame)))

(defn next-stage! [vessel]
  (.activateNextStage (get-control vessel)))

(defn abort! [vessel]
  (.setAbort (get-control vessel) true))

(defn add-stream! [x n & args]
  (.addStream @*connection* x n (to-array args)))

(defn get-stage-resources [vessel stage]
  (.resourcesInDecoupleStage vessel stage false))


(defn stage-has-run-out-of-resource? [vessel stage resource-name]
  (let [resources (get-stage-resources vessel stage)]
    (and (> (.max resources resource-name) 0)
         (<= (.amount resources resource-name) 16.0))))

(defn stage-does-not-have-resource? [vessel stage resource-name]
  (let [resources (get-stage-resources vessel stage)]
    (<= (.amount resources resource-name) 0)))

(defn get-current-stage [vessel]
  (dec (.getCurrentStage (get-control vessel))))

(defn get-active-engines [vessel]
  (let [engines (.getEngines (.getParts (get-vessel)))
        active-engines (filter (fn [engine] (.getActive engine)) engines)]
    (doall active-engines)))

(comment
  (connect! "192.168.88.146" 50000 50001)
  (get-active-engines (get-vessel)))

(defn engine-has-fuel? [engine]
  (.getHasFuel engine))

(defn solid-rocket-engine?
  "Check if engine is solid fuelled, by assuming they can't be shut down."
  [engine]
  ;; - The part configs have a staging icons for solid and liquid engines,
  ;;   but unfortunately that cannot be accessed via kRPC.
  ;; - Alternatively we could check for propellant types.
  ;;   In the FASA Saturn V these are:
  ;;   SolidFuel (fairings), PSPC (ullage motors), HTPB (launch escape system)
  ;;    (clojure.set/superset?
  ;;      #{"SolidFuel" "PSPC" "HTPB"}
  ;;      (set (.getPropellantNames engine))]
  ;; - Third way might be to check how many propellants the engine has –
  ;;   solid engines always have one and liquid engines two propellants.
  (not (.getCanShutdown engine)))

(defn next-stage-has-release-clamps? [vessel stage]
  (let [clamps (.getLaunchClamps (.getParts vessel))
        clamps-in-next-stage (filter (fn [clamp]
                                       (= (.getStage (.getPart clamp)) stage))
                                     clamps)]
    (seq clamps-in-next-stage)))

(defn next-stage-has-fairing? [vessel stage]
  (let [fairings (.getFairings (.getParts vessel))
        fairings-in-next-stage (filter (fn [fairing]
                                         (= (.getStage (.getPart fairing)) stage))
                                       fairings)]
    (seq fairings-in-next-stage)))

(defn almost-out-of-electric-charge? [vessel]
  (< (.amount (.getResources (get-vessel)) "ElectricCharge")
     (* 0.1 (.max (.getResources (get-vessel)) "ElectricCharge"))))

(defn get-surface-altitude [vessel]
  (.getSurfaceAltitude (get-flight vessel (.getSurfaceReferenceFrame vessel))))


(defn last-stage? [vessel stage]
  (= stage 0))

(defn check-staging! [vessel]
  (let [stage (dec (.getCurrentStage (get-control vessel)))
        active-engines (get-active-engines vessel)
        solid-rocket-engines (filter solid-rocket-engine? active-engines)
        liquid-engines (remove solid-rocket-engine? active-engines)]
    (when-not (last-stage? vessel stage)
      (when (empty? active-engines)
        (log! "Staging" "– No engines in stage")
        (next-stage! vessel))
      (when (and (seq solid-rocket-engines)
                 (not-every? engine-has-fuel? solid-rocket-engines))
        (log! "Staging" "– Solid rocket out of fuel")
        (next-stage! vessel))
      (when (and (seq liquid-engines)
                 (not-every? engine-has-fuel? liquid-engines))
        (log! "Staging" "– Liquid engine out of fuel")
        (next-stage! vessel))
      (when (next-stage-has-release-clamps? vessel stage)
        (log! "Staging" "– Release clamps")
        (next-stage! vessel))
      (when (and (next-stage-has-fairing? vessel stage)
                 (or (> (get-surface-altitude vessel) 70000)
                     (almost-out-of-electric-charge? vessel)))
        (log! "Staging" "– Deploy fairing")
        (next-stage! vessel)))))

(defmacro while-waiting [condition & body]
  `(while ~condition
     ~@body
     (Thread/sleep 100)))

(defn launch-sequence! [vessel]
  (let [frame (.getSurfaceReferenceFrame vessel)
        flight (get-flight vessel frame)
        altitude (add-stream! flight "getMeanAltitude")
        auto-pilot (get-auto-pilot vessel)
        control (get-control vessel)
        initial-altitude (.get altitude)]

    (log! "T-5")
    (Thread/sleep 1000)

    (log! "T-4")
    (Thread/sleep 1000)

    (log! "T-3")
    (Thread/sleep 1000)

    (log! "T-2")
    (doto control
      (.setSAS true)
      (.setRCS false)
      (.setThrottle 100))
    (Thread/sleep 1000)

    (log! "T-1")
    (Thread/sleep 1000)

    (log! "T-0")
    (next-stage! vessel)
    (log! "Ignition")

    (while-waiting (< (.get altitude) (+ initial-altitude 1))
      (check-staging! vessel))
    (log! "Lift-off")))

(defn execute-gravity-turn!
  [vessel {:keys [altitude]}]
  (let [auto-pilot (get-auto-pilot vessel)
        control (get-control vessel)]
    (while-waiting (<= (.get altitude) 250)
      (check-staging! vessel))
    (doto control
      (.setSAS false))
    (doto auto-pilot
      (.targetPitchAndHeading 90 90)
      (.engage))
    (log! "Autopilot engaged")

    (while-waiting (<= (.get altitude) 1000)
      (check-staging! vessel))
    (doto auto-pilot
      (.setTargetRoll 180)
      (.targetPitchAndHeading 85 90))
    (log! "Gravity turn")

    (while-waiting (<= (.get altitude) 5000)
      (check-staging! vessel))
    (doto auto-pilot
      (.targetPitchAndHeading 75 90))
    (log! "Gravity turn")

    (while-waiting (<= (.get altitude) 10000)
      (check-staging! vessel))
    (doto auto-pilot
      (.targetPitchAndHeading 45 90))
    (log! "Gravity turn")))

(defn execute-coasting!
  [vessel target-orbit {:keys [apoapsis]}]
  (let [control (get-control vessel)]
    (while-waiting (<= (.get apoapsis) (* 0.9 target-orbit))
      (check-staging! vessel))
    (log! "Minimum apoapsis reached" (int (* 0.9 target-orbit)))
    (log! "Coasting")
    (.setThrottle control 0)))

(defn execute-circularize!
  [vessel target-orbit {:keys [altitude apoapsis]}]
  (let [auto-pilot (get-auto-pilot vessel)]
    (while-waiting (<= (.get altitude) 70000)
      (check-staging! vessel))
    (doto auto-pilot
      (.setTargetRoll 180)
      (.targetPitchAndHeading 0 90))
    (log! "Preparing to circularize, altitude" (int (.get altitude)))

    (while-waiting (<= (.get altitude) (* 0.95 (.get apoapsis)))
      (check-staging! vessel))
    (log! "Circularizing, altitude" (int (.get altitude)))))

(defn execute-finalize-circularization!
  [vessel target-orbit {:keys [altitude apoapsis periapsis time-to-apoapsis time-to-periapsis]}]
  (let [auto-pilot (get-auto-pilot vessel)
        control (get-control vessel)]
    (while-waiting (<= (.get periapsis) (* 0.95 target-orbit))
      (check-staging! vessel)
      (cond (< (.get time-to-apoapsis) 30)
            (.setThrottle control 100)

            (< (.get time-to-apoapsis) 60)
            (.setThrottle control 50)

            ;; after apoapsis node
            (< (.get time-to-periapsis) (.get time-to-apoapsis))
            (.setThrottle control 100)

            (> (.get time-to-apoapsis) 120)
            (.setThrottle control 0)))
    (.setThrottle control 0)
    (log! "Reached orbit!")))

(defn fly-to-orbit! [vessel target-orbit]
  (let [frame (.getSurfaceReferenceFrame vessel)
        flight (get-flight vessel frame)
        orbit (.getOrbit vessel)
        auto-pilot (get-auto-pilot vessel)
        control (get-control vessel)
        ut (add-stream! SpaceCenter "getUT")
        altitude (add-stream! flight "getMeanAltitude")
        apoapsis (add-stream! orbit "getApoapsisAltitude")
        periapsis (add-stream! orbit "getPeriapsisAltitude")
        time-to-apoapsis (add-stream! orbit "getTimeToApoapsis")
        time-to-periapsis (add-stream! orbit "getTimeToPeriapsis")
        streams {:ut ut
                 :altitude altitude
                 :apoapsis apoapsis
                 :periapsis periapsis
                 :time-to-apoapsis time-to-apoapsis
                 :time-to-periapsis time-to-periapsis}]

    (execute-gravity-turn! vessel streams)
    (execute-coasting! vessel target-orbit streams)
    (execute-circularize! vessel target-orbit streams)
    (execute-finalize-circularization! vessel target-orbit streams)))

(defonce flight (atom nil))

(defn go! []
  (reset! flight
          (future
            ;;(connect! "192.168.88.146" 50000 50001)
            (connect! "127.0.0.1" 50000 50001)
            (prn :version (krpc-version))
            (doto (get-vessel)
              (launch-sequence!)
              (fly-to-orbit! 100000))
            (disconnect!)))
  @flight)

(defn prompt! [& msgs]
  (println (str/join "\n" msgs))
  (println "Press enter to continue")
  (flush)
  (read-line))

(defn generate-connection-unique-prefix []
  (str (.getClientName (get-krpc)) (.hashCode @*connection*)))

(defn save!
  "Save a game with `savename` prefixed with `prefix` or a default based on your connection id."
  ([]
   (save! "clj-autosave"))
  ([savename]
   (save! (generate-connection-unique-prefix) savename))
  ([prefix savename]
   (.save (get-space-center) (str prefix savename))))

(defn load!
  "Load a saved game with `savename` prefixed with `prefix` or a default based on your connection id.
  Unfortunately there's no way to get a list of saved games via kRPC."
  ([]
   (load! "clj-autosave"))
  ([savename]
   (load! savename (generate-connection-unique-prefix)))
  ([savename prefix]
   (.load (get-space-center) (str prefix savename))))

(defn sasmode
  "Wrap the kRPC Java enum for SASMode.
  You can use either strings or keywords to refer to the `mode`:
  => (sasmode :PROGRADE)
  #object[krpc.client.services.SpaceCenter$SASMode 0x663cc771 \"PROGRADE\"]"
  [mode]
  (krpc.client.services.SpaceCenter$SASMode/valueOf (name mode)))

(defn orbital-frame [vessel]
  (.getReferenceFrame (.getBody (.getOrbit vessel))))

(defn get-first-satellite [vessel]
  (first (.getSatellites (.getBody (.getOrbit vessel)))))

(defn min-achiavable-inclination-to-first-satellite [vessel]
  (- (.getInclination (.getOrbit vessel))
     (.getInclination (.getOrbit (get-first-satellite vessel)))))

;; TODO function that can take orbits or objects that have orbits and calculates the relative inclination
(defn inclination-to-first-satellite [vessel]
  (.relativeInclination (.getOrbit vessel)
                        (.getOrbit (get-first-satellite vessel))))

;; TODO function to count time until next optimal launch time based on relative inclination
