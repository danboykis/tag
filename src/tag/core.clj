(ns tag.core
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.data.xml :as dx]
            [xml-in.core :as xml])
  (:import [java.nio.file Files Paths]
           [java.nio.file.attribute FileAttribute]
           [java.time ZonedDateTime Instant ZoneId]
           [java.io File]
           [org.eclipse.jgit.api Git]
           [org.eclipse.jgit.lib Constants ObjectId]
           [org.eclipse.jgit.revwalk RevWalk RevTag RevCommit]
           [java.time.format DateTimeFormatter]))

(defn- fmv
  "apply f to each value v of map m"
  [m f]
  (into {}
        (for [[k v] m]
          [k (f v)])))

(defn pom? [pom]
  (or (.exists (io/file pom))
      (println "(!) could not find" pom "maven pom file, will skip maven intel")))

(defn- scoop-maven-intel [xpom]
  (when (pom? xpom)
    (let [pom        (-> (slurp xpom)
                         (dx/parse-str :namespace-aware false))
          in-project (xml/find-first pom [:project])
          project    (fn [k] (-> (xml/find-first in-project [k])
                                 first))]
      {:group-id    (project :groupId)
       :artifact-id (project :artifactId)
       :version     (project :version)
       :name        (project :name)
       :description (project :description)
       :url         (project :url)})))

(def dtf (DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:s yyyy Z"))

(defn short-commit-id [^ObjectId last-commit-id]
  "git rev-parse --short HEAD"
  (.name (.abbreviate last-commit-id 7)))

(defn tag-name [^Git git ^RevWalk rev-walk ^RevCommit commit]
  "git describe --abbrev=0"
  (let [commit->tag (reduce (fn [acc tag] (assoc acc (.getObject tag) tag))
                            {}
                            (map #(.parseTag rev-walk (.getObjectId %)) (.call (.tagList git))))]
    (when-let [t (get commit->tag commit)]
      (.getTagName ^RevTag t))))

(defn remote-origin-url [^Repository repo]
  "git config --get remote.origin.url"
  (.getString (.getConfig repo) "remote" "origin" "url"))

(defn commit-time [^RevCommit commit]
  "git log -1 --format=%cd"
  (.format (ZonedDateTime/ofInstant (Instant/ofEpochSecond (.getCommitTime commit)) (ZoneId/systemDefault)) dtf) )

(defn committer-name [^RevCommit commit]
  "git log -1 --pretty=format:'%an'"
  (.getName (.getCommitterIdent commit)))

(defn commit-message [^RevCommit commit]
  "git log -1 --pretty=%B"
  (.getName (.getCommitterIdent commit)))

(defn- scoop-git-intel []
  (with-open [git      (Git/open (File. ".git"))
              repo     (.getRepository git)
              rev-walk (RevWalk. repo)]
    (let [last-commit-id (.resolve repo Constants/HEAD)
          commit         (.parseCommit rev-walk last-commit-id)]
      (-> {:commit-id              (short-commit-id last-commit-id)
           :version/tag            (tag-name git rev-walk commit)
           :branch                 (.getBranch repo)        ;git rev-parse --abbrev-ref HEAD
           :repo-url               (remote-origin-url repo)
           :commit-time            (commit-time commit)
           "commit human (or not)" (committer-name commit)
           :commit-message         (commit-message commit)}
          (fmv #(some-> %
                        (s/replace #"\n" " ")
                        s/trim))))))

(defn describe
  ([app-name]
   (describe app-name {:about (str "..and " app-name " is my name")}))
  ([app-name {:keys [about]}]
   (let [maven (scoop-maven-intel "pom.xml")
         intel {:about
                {:app-name       app-name
                 "what do I do?" about}
                :git          (scoop-git-intel)
                :described-at (.format (ZonedDateTime/now) dtf)}]
     (if maven
       (assoc intel :maven maven)
       intel))))

(defn export-intel
  ([intel]
   (export-intel intel {}))
  ([intel {:keys [app-name path filename]
           :or   {filename "about.edn"
                  app-name "noname"
                  path     "target/about/META-INF/"}}]
   (let [fpath (str path app-name "/")]
     (Files/createDirectories (Paths/get fpath) (into-array FileAttribute []))
     (spit (str fpath filename)
           intel)
     {:intel-exported-to (str fpath filename)})))

(defn -main [& args]
  (when (< (count args) 2)
    (throw (ex-info "tag takes two params: 'app-name' and 'description'" {:args-passed args})))
  (let [[app-name & about] args]
    (-> (describe app-name {:about (->> about
                                        (interpose " ")
                                        (apply str))})
        (export-intel {:app-name app-name})
        println)))
