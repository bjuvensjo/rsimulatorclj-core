(ns rsimulatorclj-core.Simulator
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as timbre :refer [info warn]]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

(defn normalize
  "Normalize the request appropriate for the extension"
  {:test (fn []
           (is (= (normalize "foo" "txt") "foo"))
           (is (= (normalize " foo  " "txt") " foo  "))
           (is (= (normalize "<xml>\n<foo>foo</foo>   </xml>" "xml") "<xml><foo>foo</foo></xml>"))
           (is (= (normalize "<xml> foo </xml>" "xml") "<xml> foo </xml>")))}
  [request extension]
  (case extension
    "xml" (.substring (xml/emit-str (xml/parse-str request)) 38) ;; Remove xml prolog and format
    request))

(defn findMatch
  "Tries to find a match for the specified request in the files <rootPath>/<rootRelativePath>/*<extension>."
  {:test (fn []
           (is (= (:matches (findMatch "./test" "rsimulatorclj_core" "xml" "<xml>xml</xml>")) "<xml>xml</xml>"))
           (is (= (:matches (findMatch "./test" "rsimulatorclj_core" "xml" "<xml><foo>foo</foo><bar>bar</bar></xml>")) '("<xml><foo>foo</foo><bar>bar</bar></xml>" "foo" "bar")))
           (is (= (:matches (findMatch "./test" "rsimulatorclj_core" "xml" "<xml></xml>")) nil)))}
  [rootPath rootRelativePath extension request]
  (let [matches (volatile! nil)]
    {:matchingRequestFile (first (filter #(and (.endsWith (.getName %) (str "Request." extension))
                                               (vreset! matches (re-matches (re-pattern (normalize (slurp %) extension)) (normalize request extension))))
                                         (file-seq (io/file (str rootPath "/" rootRelativePath)))))
     :matches @matches}))

(defn getResponse
  "Get the response that corresponds to the matchingRequestFile replaced with matches."
  {:test (fn []
           (is (= (getResponse (io/file "./test/rsimulatorclj_core/Test1Request.xml") "<response>response</response>") "<response>response</response>\n"))
           (is (= (getResponse (io/file "./test/rsimulatorclj_core/Test2Request.xml") '("<xml><foo>foo</foo><bar>bar</bar></xml>" "foo" "bar")) "<response>foobar</response>\n")))}
  [matchingRequestFile matches]
  ((reduce (fn [accum match] [(.replaceAll (accum 0) (str "[$]+[{]+" (accum 1) "[}]+") match) (inc (accum 1))])
           [(slurp (io/file (.getParent matchingRequestFile) (.replaceFirst (.getName matchingRequestFile) "Request" "Response"))) 1]
           (if (coll? matches) (next matches) '()))
   0))

(defn getConfig
  "Get config that corresponds to the matchingRequestFile"
  {:test (fn []
           (is (= (getConfig (io/file "./test/rsimulatorclj_core/Test1Request.xml")) nil))
           (is (= (:foo (getConfig (io/file "./test/rsimulatorclj_core/Test2Request.xml"))) "foo")))}
  [matchingRequestFile]
  (let [configFile (io/file (.getParent matchingRequestFile) (.replaceFirst (.getName matchingRequestFile) "Request.*" "Config.clj"))]
    (if (.exists configFile)
      (read-string (slurp configFile)))))

(defn- applyScript [scriptMap scriptFile]
  (if (.exists scriptFile)
    ((load-file (.getAbsolutePath scriptFile)) scriptMap)
    scriptMap))

(defrecord SimulatorResponse [matchingRequestFile response config])

(defn service
  "Returns a response of the specified request if such can be found recursively in <rootPath>/<rootRelativePath> in a request/response pair with the specified extension."
  {:test (fn []
           (is (= (:response (service "./test/rsimulatorclj_core" "" "<xml></xml>" "xml")) nil))
           (let [simulatorResponse (service "./test/rsimulatorclj_core" "" "<xml>xml</xml>" "xml")]
             (is (= (.getName (:matchingRequestFile simulatorResponse)) "Test1Request.xml"))
             (is (= (:response simulatorResponse) "<response>response</response>\n"))
             (is (= (:properties simulatorResponse) nil)))
           (let [simulatorResponse (service "./test/rsimulatorclj_core" "" "<xml><foo>foo</foo><bar>bar</bar></xml>" "xml")]
             (is (= (.getName (:matchingRequestFile simulatorResponse)) "Test2Request.xml"))
             (is (= (:response simulatorResponse) "<xml><response>foobar</response></xml>"))
             (is (= (:foo (:config simulatorResponse)) "foo"))))}
  [rootPath rootRelativePath request contentType]
  (let [scriptMap {:rootPath rootPath :rootRelativePath rootRelativePath :request request :contentType contentType}
        _         (info "entry:" scriptMap)
        scriptMap (applyScript scriptMap (io/file (str rootPath "/GlobalRequest.clj")))]
    (or (:simulatorResponse scriptMap)
        (let [{matchingRequestFile :matchingRequestFile matches :matches} (findMatch rootPath rootRelativePath ({:txt "txt" :xml "xml"} (keyword contentType)) request)]
          (if matchingRequestFile
            (let [scriptMap (-> scriptMap
                                (assoc :simulatorResponse (SimulatorResponse. matchingRequestFile (getResponse matchingRequestFile matches) (getConfig matchingRequestFile)))
                                (applyScript (io/file (.getParent matchingRequestFile) (.replaceFirst (.getName matchingRequestFile) "Request.*" ".clj")))
                                (applyScript (io/file (str rootPath "/GlobalResponse.clj"))))]
              (info "exit:" scriptMap)
              (:simulatorResponse scriptMap))
            (warn "exit: No response found" scriptMap))))))
