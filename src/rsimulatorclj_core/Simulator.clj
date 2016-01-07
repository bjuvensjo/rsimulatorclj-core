(ns rsimulatorclj-core.Simulator
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

(defn normalize [request extension]
  (case extension
    "xml" (.substring (xml/emit-str (xml/parse-str request)) 38) ;; Remove xml prolog and format
    request))

(defn findMatch [rootPath rootRelativePath extension request]
  (let [matches (volatile! nil)]
    {:matchingRequestFile (first (filter #(and (.endsWith (.getName %) (str "Request." extension))
                                               (vreset! matches (re-matches (re-pattern (normalize (slurp %) extension)) (normalize request extension))))
                                         (file-seq (io/file (str rootPath "/" rootRelativePath)))))
     :matches @matches}))

(defn getResponse [matchingRequestFile matches]
  ((reduce (fn [accum match] [(.replaceAll (accum 0) (str "[$]+[{]+" (accum 1) "[}]+") match) (inc (accum 1))])
           [(slurp (io/file (.getParent matchingRequestFile) (.replaceFirst (.getName matchingRequestFile) "Request" "Response"))) 1]
           (if (coll? matches) (next matches) '()))
   0))

(defn getProperties [matchingRequestFile]
  (let [propertiesFile (io/file (.getParent matchingRequestFile) (.replaceFirst (.getName matchingRequestFile) "Request.*" ".properties"))]
    (if (.exists propertiesFile)
      (with-open [reader (io/reader propertiesFile)] 
        (let [props (java.util.Properties.)]
          (.load props reader)
          props)))))

(defn- applyScript [scriptMap scriptFile]
  (if (.exists scriptFile)
    ((load-file (.getAbsolutePath scriptFile)) scriptMap)
    scriptMap))

(defrecord SimulatorResponse [matchingRequestFile response properties])

(defn service [rootPath rootRelativePath request contentType]
  (let [scriptMap (applyScript {:rootPath rootPath :rootRelativePath rootRelativePath :request request :contentType contentType} (io/file (str rootPath "/GlobalRequest.clj")))]
    (or (:simulatorResponse scriptMap)
        (let [{matchingRequestFile :matchingRequestFile matches :matches} (findMatch rootPath rootRelativePath ({:txt "txt" :xml "xml"} (keyword contentType)) request)]
          (if matchingRequestFile
            (-> scriptMap
                (assoc :simulatorResponse (SimulatorResponse. matchingRequestFile (getResponse matchingRequestFile matches) (getProperties matchingRequestFile)))
                (applyScript (io/file (.getParent matchingRequestFile) (.replaceFirst (.getName matchingRequestFile) "Request.*" ".clj")))
                (applyScript (io/file (str rootPath "/GlobalResponse.clj")))
                :simulatorResponse))))))
