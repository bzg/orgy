(ns bzg.orgy-test
  "Unit tests for orgy's pure helpers and URL-resolution paths."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [bzg.organ :as organ]
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
(def meta-str                  @#'orgy/meta-str)
(def ast->html                 @#'orgy/ast->html)
(def ast->text                 @#'orgy/ast->text)

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
  (is (= "&lt;a&gt;&amp;&quot;" (escape-html "<a>&\"")))
  (is (= "" (escape-html nil))))

;; ---------------------------------------------------------------------------
;; Blocks
;; ---------------------------------------------------------------------------

(deftest block-html-test
  (testing "an empty src block renders instead of throwing"
    (is (= "<pre><code></code></pre>"
           (ast->html (organ/parse-org "#+begin_src\n#+end_src\n")))))
  (testing "a comment block is never published"
    (let [ast (organ/parse-org "#+begin_comment\nsecret\n#+end_comment\n")]
      (is (= "" (ast->html ast)))
      (is (= "" (ast->text ast)))))
  (testing "only html export blocks are emitted"
    (is (= "<b>x</b>"
           (ast->html (organ/parse-org "#+begin_export html\n<b>x</b>\n#+end_export\n"))))
    (is (= ""
           (ast->html (organ/parse-org "#+begin_export latex\n\\newpage\n#+end_export\n")))))
  (testing "other blocks stay preformatted"
    (is (= "<pre>&lt;b&gt;</pre>"
           (ast->html (organ/parse-org "#+begin_example\n<b>\n#+end_example\n"))))))

;; ---------------------------------------------------------------------------
;; build! end to end
;; ---------------------------------------------------------------------------

(deftest build-test
  (let [root (str (fs/create-temp-dir {:prefix "orgy-test"}))
        out  (str root "/out")
        put  (fn [rel s] (fs/create-dirs (fs/parent (fs/path root rel)))
               (spit (str (fs/path root rel)) s))]
    (try
      (put "config.edn" (pr-str {:base-url "https://ex.org" :skip-dirs ["drafts"]}))
      (put "notes/a.org" "#+title: A\n#+date: 2024-01-01\n\nHello *world*.\n")
      (put "drafts/d.org" "#+title: D\n\nDraft.\n")
      (put "drafts/d.txt" "x")
      (with-out-str
        (orgy/build! {:input-dir root :output-dir out}))
      (testing ":skip-dirs from config.edn is honoured"
        (is (fs/exists? (str out "/notes/a/index.html")))
        (is (not (fs/exists? (str out "/drafts")))))
      (testing "feed content is HTML inside CDATA, not escaped HTML"
        (is (str/includes? (slurp (str out "/feed.xml"))
                           "<![CDATA[<p>Hello <strong>world</strong>.</p>]]>")))
      (finally (fs/delete-tree root)))))

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

;; ---------------------------------------------------------------------------
;; Document metadata and lists, as parsed by organ
;; ---------------------------------------------------------------------------

(deftest meta-str-test
  (let [meta (:meta (organ/parse-org (str "#+TITLE: First\n#+TITLE: second\n"
                                          "#+DATE: <2026-11-05 Thu>\n#+DATE: <2026-11-06 Fri>\n"
                                          "#+AUTHOR: Me\n")))]
    (testing "a repeated title is joined with a space"
      (is (= "First second" (meta-str meta :title))))
    (testing "a repeated date keeps the first one"
      (is (= "<2026-11-05 Thu>" (meta-str meta :date))))
    (testing "a single value is returned as is"
      (is (= "Me" (meta-str meta :author))))
    (testing "an absent key gives nil"
      (is (nil? (meta-str meta :description))))))

(deftest list-html-test
  (testing "an item with a [@N] cookie restarts the numbering"
    (is (= "<ol><li>a</li><li value=\"5\">b</li><li>c</li></ol>"
           (ast->html (organ/parse-org "1. a\n2. [@5] b\n3. c")))))
  (testing "a description item renders its children in the definition"
    (is (= "<dl><dt>Term</dt><dd>def<ul><li>sub</li></ul></dd></dl>"
           (ast->html (organ/parse-org "- Term :: def\n  - sub"))))))

(deftest list-text-test
  (testing "description items contribute their term and definition"
    (is (= "Term def sub" (ast->text (organ/parse-org "- Term :: def\n  - sub"))))))
