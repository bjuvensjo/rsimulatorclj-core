(ns rsimulatorclj-core.Simulator-test
  (:require [clojure.test :refer :all]
            [rsimulatorclj-core.Simulator :refer :all]
            [clojure.java.io :as io]))

(deftest normalize-test-txt
  (are [expect actual] (= expect actual)
    "foo" (normalize "foo" "txt")))

(deftest normalize-test-xml
  (are [expect actual] (= expect actual)
    "<xml><foo>foo</foo></xml>" (normalize "<xml>\n<foo>foo</foo>   </xml>" "xml")
    "<xml> foo </xml>" (normalize "<xml> foo </xml>" "xml")))

(deftest findMatch-test
  (are [expect actual] (= expect actual)
    "<xml>xml</xml>" (:matches (findMatch "./test" "rsimulatorclj_core" "xml" "<xml>xml</xml>"))
    '("<xml><foo>foo</foo><bar>bar</bar></xml>" "foo" "bar") (:matches (findMatch "./test" "rsimulatorclj_core" "xml" "<xml><foo>foo</foo><bar>bar</bar></xml>"))
    nil (:matches (findMatch "./test" "rsimulatorclj_core" "xml" "<xml></xml>"))))

(deftest getResponse-test
  (are [expect actual] (= expect actual)
    "<response>response</response>\n" (getResponse (io/file "./test/rsimulatorclj_core/Test1Request.xml") "<response>response</response>")
    "<response>foobar</response>\n" (getResponse (io/file "./test/rsimulatorclj_core/Test2Request.xml") '("<xml><foo>foo</foo><bar>bar</bar></xml>" "foo" "bar"))))

(deftest getConfig-test
  (are [expect actual] (= expect actual)
    nil (getConfig (io/file "./test/rsimulatorclj_core/Test1Request.xml"))
    "foo" (:foo (getConfig (io/file "./test/rsimulatorclj_core/Test2Request.xml")))))

(deftest service-testNonMatching
  (are [expect actual] (= expect actual)    
    nil (:response (service "./test/rsimulatorclj_core" "" "<xml></xml>" "xml"))))

(deftest service-test1
  (let [simulatorResponse (service "./test/rsimulatorclj_core" "" "<xml>xml</xml>" "xml")]
    (are [expect actual] (= expect actual)
      "Test1Request.xml" (.getName (:matchingRequestFile simulatorResponse))
      "<response>response</response>\n" (:response simulatorResponse)
      nil (:properties simulatorResponse))))

(deftest service-test2
  (let [simulatorResponse (service "./test/rsimulatorclj_core" "" "<xml><foo>foo</foo><bar>bar</bar></xml>" "xml")]
    (are [expect actual] (= expect actual)
      "Test2Request.xml" (.getName (:matchingRequestFile simulatorResponse))
      "<xml><response>foobar</response></xml>" (:response simulatorResponse)
      "foo" (:foo (:config simulatorResponse)))))
