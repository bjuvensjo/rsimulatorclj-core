(ns rsimulatorclj-core.Simulator
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

(defn normalize [request extension]
  (case extension
    "xml" (.substring (xml/emit-str (xml/parse-str request)) 38) ;; Remove xml prolog and format
    request))

(defn findMatch [rootPath rootRelativePath extension request]
  (let [normalizedRequest (normalize request extension)]
    (loop [files (filter #(.endsWith (.getName %) (str "Request." extension)) (file-seq (io/file (str rootPath "/" rootRelativePath))))]
      (if files
        (let [matches (re-matches (re-pattern (normalize (slurp (first files)) extension)) normalizedRequest)]
          (if matches
            {:matchingRequestFile (first files) :matches matches}
            (recur (next files))))))))

(defn getResponse [matchingRequestFile matches]
  (loop [response (slurp (io/file (.getParent matchingRequestFile) (.replaceFirst (.getName matchingRequestFile) "Request" "Response")))
         matches (if (coll? matches) (next matches))
         index 1]
    (if matches
      (recur (.replaceAll response (str "[$]+[{]+" index "[}]+") (first matches)) (next matches) (inc index))
      response)))

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
