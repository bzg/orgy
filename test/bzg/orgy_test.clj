(ns bzg.orgy-test
  "Unit tests for orgy's pure helpers and URL-resolution paths."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [bzg.orgy :as orgy]))

;; Private fns are exercised via their vars.
(def normalize-rel-path        @#'orgy/normalize-rel-path)
(def resolve-rel-against-source @#'orgy/resolve-rel-against-source)
(def resolve-file-url          @#'orgy/resolve-file-url)
(def resolve-css-theme         @#'orgy/resolve-css-theme)
(def detect-monolingual?       @#'orgy/detect-monolingual?)
(def resolve-languages         @#'orgy/resolve-languages)
(def file-slug                 @#'orgy/file-slug)
(def file-lang                 @#'orgy/file-lang)
(def file-section              @#'orgy/file-section)
(def index-file?               @#'orgy/index-file?)
(def parse-tags-header         @#'orgy/parse-tags-header)
(def truncate                  @#'orgy/truncate)
(def escape-html               @#'orgy/escape-html)
(def iso->rfc822               @#'orgy/iso->rfc822)
(def date-desc                 @#'orgy/date-desc)

;; ---------------------------------------------------------------------------
;; normalize-rel-path
;; ---------------------------------------------------------------------------

(deftest normalize-rel-path-test
  (testing "collapses ./ segments"
    (is (= "foo/bar" (normalize-rel-path "./foo/./bar")))
    (is (= "foo" (normalize-rel-path "foo/."))))
  (testing "resolves ../ segments"
    (is (= "bar" (normalize-rel-path "foo/../bar")))
    (is (= "baz" (normalize-rel-path "foo/bar/../../baz"))))
  (testing "keeps unresolvable leading .."
    (is (= "../foo" (normalize-rel-path "../foo"))))
  (testing "handles simple paths unchanged"
    (is (= "foo/bar/baz" (normalize-rel-path "foo/bar/baz")))))

;; ---------------------------------------------------------------------------
;; resolve-rel-against-source
;; ---------------------------------------------------------------------------

(deftest resolve-rel-against-source-test
  (testing "with a post inside a section"
    (with-redefs [orgy/*input-dir* "/input"
                  orgy/*current-source-file* "/input/notes/hello.org"]
      (is (= "notes/photo.jpg"
             (resolve-rel-against-source "./photo.jpg")))
      (is (= "notes/photo.jpg"
             (resolve-rel-against-source "photo.jpg")))
      (is (= "other/thing.png"
             (resolve-rel-against-source "../other/thing.png")))))
  (testing "with a post at the root"
    (with-redefs [orgy/*input-dir* "/input"
                  orgy/*current-source-file* "/input/index.org"]
      (is (= "photo.jpg"
             (resolve-rel-against-source "./photo.jpg")))))
  (testing "with no source file bound"
    (with-redefs [orgy/*input-dir* "/input"
                  orgy/*current-source-file* nil]
      (is (= "photo.jpg"
             (resolve-rel-against-source "./photo.jpg"))))))

;; ---------------------------------------------------------------------------
;; resolve-file-url
;; ---------------------------------------------------------------------------

(defn- file-link [target] {:link-type :file :target target})
(defn- plain-link [url]   {:url url})

(deftest resolve-file-url-non-org
  (testing "relative asset from a sectioned post"
    (with-redefs [orgy/*input-dir* "/input"
                  orgy/*current-source-file* "/input/notes/hello.org"
                  orgy/*monolingual* true]
      (is (= "/notes/photo.jpg"
             (resolve-file-url (file-link "./photo.jpg"))))
      (is (= "/notes/photo.jpg"
             (resolve-file-url (file-link "photo.jpg"))))))
  (testing "static/ prefix is stripped"
    (is (= "/img/logo.png" (resolve-file-url (file-link "static/img/logo.png"))))
    (is (= "/img/logo.png" (resolve-file-url (file-link "/home/me/blog/static/img/logo.png")))))
  (testing "absolute paths pass through"
    (is (= "/notes/photo.jpg" (resolve-file-url (file-link "/notes/photo.jpg")))))
  (testing "non-file links return :url as-is"
    (is (= "https://example.com" (resolve-file-url (plain-link "https://example.com"))))))

(deftest resolve-file-url-org
  (testing "org link to same-section post (monolingual)"
    (with-redefs [orgy/*input-dir* "/input"
                  orgy/*current-source-file* "/input/notes/hello.org"
                  orgy/*monolingual* true]
      (is (= "/notes/other/"
             (resolve-file-url (file-link "./other.org"))))
      (is (= "/notes/other/"
             (resolve-file-url (file-link "other.org"))))))
  (testing "org link with explicit section (monolingual)"
    (with-redefs [orgy/*monolingual* true]
      (is (= "/essays/thinking/"
             (resolve-file-url (file-link "essays/thinking.org"))))))
  (testing "org link with language suffix (multilingual)"
    (with-redefs [orgy/*monolingual* false]
      (is (= "/fr/notes/bonjour/"
             (resolve-file-url (file-link "notes/bonjour.fr.org")))))))

;; ---------------------------------------------------------------------------
;; Filename helpers
;; ---------------------------------------------------------------------------

(deftest file-slug-test
  (is (= "post" (file-slug "post.org")))
  (is (= "post" (file-slug "post.fr.org")))
  (is (= "my-post" (file-slug "notes/my-post.org")))
  (is (= "my-post" (file-slug "notes/my-post.en.org"))))

(deftest file-lang-test
  (is (= nil (file-lang "post.org")))
  (is (= "fr" (file-lang "post.fr.org")))
  (is (= "en" (file-lang "notes/my-post.en.org"))))

(deftest file-section-test
  (with-redefs [orgy/*input-dir* "/input"]
    (is (= nil (file-section "/input/post.org")))
    (is (= "notes" (file-section "/input/notes/hello.org")))
    (is (= "notes" (file-section "/input/notes/sub/hello.org")))))

(deftest index-file?-test
  (is (index-file? "index.org"))
  (is (index-file? "index.fr.org"))
  (is (not (index-file? "_index.org")) "after tutorial cleanup")
  (is (not (index-file? "about.org")))
  (is (not (index-file? "notes/hello.org"))))

;; ---------------------------------------------------------------------------
;; parse-tags-header
;; ---------------------------------------------------------------------------

(deftest parse-tags-header-test
  (is (= ["emacs" "org-mode"]
         (parse-tags-header "#+title: Hi\n#+tags: emacs org-mode\n\nBody")))
  (is (= ["emacs"]
         (parse-tags-header "#+tags:   emacs   \n")))
  (is (nil? (parse-tags-header "#+title: Hi\n"))))

;; ---------------------------------------------------------------------------
;; truncate / escape-html
;; ---------------------------------------------------------------------------

(deftest truncate-test
  (is (= "short" (truncate "short" 10)))
  (is (= "exactly..." (truncate "exactly ten chars" 7))))

(deftest escape-html-test
  (is (= "&lt;a&gt;&amp;&quot;" (escape-html "<a>&\""))))

;; ---------------------------------------------------------------------------
;; iso->rfc822
;; ---------------------------------------------------------------------------

(deftest iso->rfc822-test
  (is (str/includes? (iso->rfc822 "2024-03-28") "Thu, 28 Mar 2024"))
  (is (nil? (iso->rfc822 nil)))
  (is (= "not-a-date" (iso->rfc822 "not-a-date")) "invalid dates pass through"))

;; ---------------------------------------------------------------------------
;; date-desc comparator
;; ---------------------------------------------------------------------------

(deftest date-desc-test
  (is (neg? (date-desc "2025-01-02" "2025-01-01")))
  (is (pos? (date-desc "2025-01-01" "2025-01-02")))
  (is (zero? (date-desc "2025-01-01" "2025-01-01")))
  (testing "nils sort last"
    (is (neg? (date-desc "2025-01-01" nil)))
    (is (pos? (date-desc nil "2025-01-01")))))

;; ---------------------------------------------------------------------------
;; resolve-css-theme
;; ---------------------------------------------------------------------------

(deftest resolve-css-theme-test
  (testing "https URL → :link"
    (is (= {:link "https://example.com/a.css"}
           (resolve-css-theme "https://example.com/a.css"))))
  (testing "pico-themes name → CDN :link"
    (is (str/includes? (:link (resolve-css-theme "teletype")) "teletype.css")))
  (testing "nil → nil"
    (is (nil? (resolve-css-theme nil)))))

;; ---------------------------------------------------------------------------
;; detect-monolingual?
;; ---------------------------------------------------------------------------

(deftest detect-monolingual?-test
  (testing "no config, one language in posts → monolingual"
    (is (detect-monolingual? {} [{:lang "en"}])))
  (testing "explicit multi-language config → not monolingual"
    (is (not (detect-monolingual? {:languages ["en" "fr"]} [{:lang "en"}]))))
  (testing "multiple langs in posts → not monolingual"
    (is (not (detect-monolingual? {} [{:lang "en"} {:lang "fr"}]))))
  (testing "map-shaped :menu implies multilingual"
    (is (not (detect-monolingual? {:menu {:en []}} [{:lang "en"}])))))

;; ---------------------------------------------------------------------------
;; resolve-languages
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; CUSTOM_ID / ID on section headers
;; ---------------------------------------------------------------------------

(def render-node @#'orgy/render-node)

(deftest section-custom-id-test
  (testing "CUSTOM_ID becomes id attribute"
    (is (str/includes?
         (render-node {:type :section :level 1
                       :title [{:type :text :value "Hello"}]
                       :properties {:custom_id "my-id"}
                       :children []})
         "<h2 id=\"my-id\">Hello</h2>")))
  (testing "ID is used when CUSTOM_ID is absent"
    (is (str/includes?
         (render-node {:type :section :level 1
                       :title [{:type :text :value "Hi"}]
                       :properties {:id "fallback"}
                       :children []})
         "<h2 id=\"fallback\">Hi</h2>")))
  (testing "CUSTOM_ID wins over ID"
    (is (str/includes?
         (render-node {:type :section :level 1
                       :title [{:type :text :value "T"}]
                       :properties {:id "a" :custom_id "b"}
                       :children []})
         "id=\"b\"")))
  (testing "no id when neither property is set"
    (is (str/includes?
         (render-node {:type :section :level 1
                       :title [{:type :text :value "T"}]
                       :children []})
         "<h2>T</h2>"))))

(deftest resolve-languages-test
  (testing "explicit config wins and is unioned with posts"
    (is (= ["en" "fr"]
           (resolve-languages {:languages ["en" "fr"]} [{:lang "en"}])))
    (is (= ["en" "fr"]
           (resolve-languages {:languages ["en"]} [{:lang "en"} {:lang "fr"}]))))
  (testing "posts inferred when no config (no phantom 'en')"
    (is (= ["fr"] (resolve-languages {} [{:lang "fr"}]))))
  (testing "empty fallback to en"
    (is (= ["en"] (resolve-languages {} [])))))
