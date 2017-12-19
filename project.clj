(defproject anglican-examples "0.2.1-SNAPSHOT"
  :description "Anglican program examples expressed as Gorilla repl instances"
  :url "http://www.robots.ox.ac.uk/~fwood/anglican/"
  :license {:name "GNU General Public License Version 3; Other commercial licenses available."
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [clj-auto-diff "0.1.3"]
                 [anglican "1.0.0"]]
  :plugins [[lein-gorilla "0.4.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
