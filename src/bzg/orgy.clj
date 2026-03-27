;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0

(ns bzg.orgy
  "Static blog engine: org files → HTML via organ + selmer."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [selmer.parser :as selmer]
            [bzg.organ :as organ]))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(defn- load-config []
  (let [cfg   (when (fs/exists? "config.edn")
                (-> "config.edn" slurp clojure.edn/read-string))
        title (or (:title cfg) (-> "." fs/absolutize fs/file-name str))]
    {:title     title
     :base-url  (str/replace (or (:base-url cfg) "") #"/$" "")
     :copyright (or (:copyright cfg) "")
     :theme-url (:theme-url cfg)
     :languages (or (:languages cfg) ["en"])}))

;; ---------------------------------------------------------------------------
;; Default templates (used when templates/ dir is absent)
;; ---------------------------------------------------------------------------

(def ^:private default-templates
  {"base.html"
   "<!DOCTYPE html>
<html lang=\"{{lang}}\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>{% if title %}{{title}} — {% endif %}{{site.title}}</title>
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
  {% if theme-url %}<link rel=\"stylesheet\" href=\"{{theme-url}}\">{% endif %}
  <link rel=\"alternate\" type=\"application/rss+xml\" title=\"{{site.title}} ({{lang}})\" href=\"/{{lang}}/feed.xml\">
</head>
<body>
  <header class=\"container\">
    <nav>
      <ul>
        <li><strong><a href=\"/{{lang}}/\">{{site.title}}</a></strong></li>
      </ul>
      <ul>
        {% for item in menu %}
        <li><a href=\"{{item.url}}\">{{item.name}}</a></li>
        {% endfor %}
      </ul>
    </nav>
  </header>
  <main class=\"container\">
    {{body|safe}}
  </main>
  <footer class=\"container\">
    <p>{{site.copyright}}</p>
  </footer>
</body>
</html>"

   "post.html"
   "<article>
  <h1>{{title}}</h1>
  {% if date %}<time datetime=\"{{date}}\">{{date}}</time>{% endif %}
  {% if tags %}
  <div class=\"tags\">
    {% for tag in tags %}
    <a href=\"/{{lang}}/tags/{{tag}}/\">{{tag}}</a>
    {% endfor %}
  </div>
  {% endif %}
  <div class=\"content\">
    {{content|safe}}
  </div>
</article>"

   "list.html"
   "<h1>{{title}}</h1>
<ul class=\"post-list\">
  {% for post in posts %}
  <li>
    <a href=\"{{post.url}}\">{{post.title}}</a>
    {% if post.date %}<time datetime=\"{{post.date}}\">{{post.date}}</time>{% endif %}
  </li>
  {% endfor %}
</ul>"

   "tag.html"
   "<h1>{{tag}}</h1>
<ul class=\"post-list\">
  {% for post in posts %}
  <li>
    <a href=\"{{post.url}}\">{{post.title}}</a>
    {% if post.date %}<time datetime=\"{{post.date}}\">{{post.date}}</time>{% endif %}
  </li>
  {% endfor %}
</ul>"

   "tags-index.html"
   "<h1>Tags</h1>
<ul class=\"tag-list\">
  {% for entry in tags %}
  <li><a href=\"/{{lang}}/tags/{{entry.tag}}/\">{{entry.tag}}</a> ({{entry.count}})</li>
  {% endfor %}
</ul>"

   "feed.xml"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">
  <channel>
    <title>{{site.title}}</title>
    <link>{{site.base-url}}/{{lang}}/</link>
    <description>{{site.title}}</description>
    <atom:link href=\"{{site.base-url}}/{{lang}}/feed.xml\" rel=\"self\" type=\"application/rss+xml\"/>
    {% for post in posts %}
    <item>
      <title>{{post.title}}</title>
      <link>{{site.base-url}}{{post.url}}</link>
      <guid>{{site.base-url}}{{post.url}}</guid>
      {% if post.date %}<pubDate>{{post.date}}</pubDate>{% endif %}
      <description>{{post.summary}}</description>
    </item>
    {% endfor %}
  </channel>
</rss>"})

(defn- resolve-template
  "Return template string: from templates/ file if present, else default."
  [template-name]
  (let [file-path (str "templates/" template-name)]
    (if (fs/exists? file-path)
      (slurp file-path)
      (get default-templates template-name))))

(defn- render-template
  "Render a template by name."
  [template-name ctx]
  (-> template-name resolve-template (selmer/render ctx)))

(defn- render-page
  "Render a page template wrapped in the base layout."
  [template-name ctx]
  (let [body (render-template template-name ctx)]
    (render-template "base.html" (assoc ctx :body body))))

;; ---------------------------------------------------------------------------
;; Inline node rendering (organ 0.3.0 returns parsed inline node vectors)
;; ---------------------------------------------------------------------------

(def ^:private image-ext-re #"\.(?:png|jpg|jpeg|gif|svg|webp)$")

(defn- render-inline
  "Render a vector of organ inline nodes to HTML."
  [nodes]
  (when (seq nodes)
    (str/join
     (map (fn [node]
            (case (:type node)
              :text      (:value node)
              :bold      (str "<strong>" (render-inline (:children node)) "</strong>")
              :italic    (str "<em>" (render-inline (:children node)) "</em>")
              :underline (str "<u>" (render-inline (:children node)) "</u>")
              :strike    (str "<del>" (render-inline (:children node)) "</del>")
              :code      (str "<code>" (:value node) "</code>")
              :verbatim  (str "<code>" (:value node) "</code>")
              :link      (let [url (:url node)]
                           (if (re-find image-ext-re url)
                             (str "<img src=\"" url "\" alt=\""
                                  (or (organ/inline-text (:children node)) "") "\">")
                             (if (seq (:children node))
                               (str "<a href=\"" url "\">" (render-inline (:children node)) "</a>")
                               (str "<a href=\"" url "\">" url "</a>"))))
              :footnote-ref (str "<sup><a href=\"#fn-" (:label node)
                                 "\" id=\"fnref-" (:label node) "\">"
                                 (:label node) "</a></sup>")
              :footnote-inline ""
              :timestamp (:raw node)
              ""))
          nodes))))

;; ---------------------------------------------------------------------------
;; AST → HTML rendering
;; ---------------------------------------------------------------------------

(declare render-node)

(defn- render-children [children]
  (str/join "\n" (map render-node children)))

(defn- render-list-items [items ordered?]
  (let [tag (if ordered? "ol" "ul")]
    (str "<" tag ">\n"
         (str/join "\n"
                   (map (fn [item]
                          (str "<li>"
                               (if (:term item)
                                 (str "<dt>" (render-inline (:term item)) "</dt>"
                                      "<dd>" (render-inline (:definition item)) "</dd>")
                                 (str (render-inline (:content item))
                                      (when-let [children (:children item)]
                                        (str "\n" (render-children children)))))
                               "</li>"))
                        items))
         "\n</" tag ">")))

(defn- render-table [{:keys [rows has-header]}]
  (let [[header & body-rows] (if has-header rows (cons nil rows))]
    (str "<table>\n"
         (when (and has-header header)
           (str "<thead><tr>"
                (str/join (map #(str "<th>" (render-inline %) "</th>") header))
                "</tr></thead>\n"))
         "<tbody>\n"
         (str/join "\n"
                   (map (fn [row]
                          (str "<tr>"
                               (str/join (map #(str "<td>" (render-inline %) "</td>") row))
                               "</tr>"))
                        (if has-header body-rows rows)))
         "\n</tbody>\n</table>")))

(defn- render-node [node]
  (case (:type node)
    :paragraph
    (let [c (:content node)]
      (if-let [text (organ/inline-text c)]
        (if (re-matches #"^\s*#\+\w+(?:\[\])?\s*:.*" text)
          "" ;; skip org keywords that organ didn't parse as metadata
          (str "<p>" (render-inline c) "</p>"))
        ""))

    :section
    (let [level (min (:level node) 6)
          tag   (str "h" level)]
      (str "<" tag ">" (render-inline (:title node)) "</" tag ">\n"
           (render-children (:children node))))

    :src-block
    (str "<pre><code"
         (when (:language node) (str " class=\"language-" (:language node) "\""))
         ">"
         (-> (:content node)
             (str/replace "&" "&amp;")
             (str/replace "<" "&lt;")
             (str/replace ">" "&gt;"))
         "</code></pre>")

    :quote-block
    (str "<blockquote>" (render-children (:children node)) "</blockquote>")

    :block
    (str "<pre>" (-> (:content node)
                     (str/replace "&" "&amp;")
                     (str/replace "<" "&lt;")
                     (str/replace ">" "&gt;"))
         "</pre>")

    :list
    (if (:description node)
      (str "<dl>\n"
           (str/join "\n" (map (fn [item]
                                 (str "<dt>" (render-inline (:term item)) "</dt>\n"
                                      "<dd>" (render-inline (:definition item)) "</dd>"))
                               (:items node)))
           "\n</dl>")
      (render-list-items (:items node) (:ordered node)))

    :table
    (render-table node)

    :fixed-width
    (str "<pre>" (-> (:content node)
                     (str/replace "&" "&amp;")
                     (str/replace "<" "&lt;")
                     (str/replace ">" "&gt;"))
         "</pre>")

    :comment ""

    :footnote-def
    (str "<div class=\"footnote\" id=\"fn-" (:label node) "\">"
         "<sup>" (:label node) "</sup> "
         (render-inline (:content node))
         " <a href=\"#fnref-" (:label node) "\">↩</a></div>")

    :drawer ""

    ;; fallback
    (if (:children node)
      (render-children (:children node))
      "")))

(defn- ast->html
  "Render an organ AST document to HTML body content."
  [ast]
  (render-children (:children ast)))

;; ---------------------------------------------------------------------------
;; Content loading
;; ---------------------------------------------------------------------------

(defn- parse-tags-header
  "Parse #+tags: tag1 tag2 or #+tags[]: tag1 tag2 into a vector."
  [org-text]
  (when-let [[_ tags] (re-find #"(?m)^#\+tags(?:\[\])?:\s*(.+)$" org-text)]
    (str/split (str/trim tags) #"\s+")))

(defn- file-lang
  "Extract language from filename: foo.fr.org → \"fr\", foo.en.org → \"en\"."
  [path]
  (let [name (str (fs/file-name path))]
    (when-let [[_ lang] (re-matches #".*\.(\w{2})\.org$" name)]
      lang)))

(defn- file-slug
  "Extract slug from filename: some-post.fr.org → \"some-post\", post.org → \"post\"."
  [path]
  (let [name (str (fs/file-name path))]
    (or (second (re-matches #"(.+?)\.\w{2}\.org$" name))
        (second (re-matches #"(.+?)\.org$" name)))))

(defn- file-section
  "Determine content section from path: content/notes/foo.org → \"notes\"."
  [path]
  (let [parts (-> (fs/relativize "content" path) str (str/split #"/"))]
    (when (> (count parts) 1)
      (first parts))))

(defn- index-file?
  "True if file is an _index or index file."
  [path]
  (let [name (str (fs/file-name path))]
    (re-matches #"_?index(?:\.\w{2})?\.org" name)))

(defn- load-post
  "Parse a single org file into a post map."
  [path]
  (let [org-text (slurp (str path))
        ast      (organ/parse-org org-text)
        meta     (:meta ast)
        lang     (or (file-lang path) "en")
        slug     (file-slug path)
        section  (file-section path)
        tags     (or (parse-tags-header org-text) [])
        url      (if section
                   (str "/" lang "/" section "/" slug "/")
                   (str "/" lang "/" slug "/"))]
    {:title   (or (:title meta) slug)
     :date    (when-let [d (:date meta)]
               ;; Normalize Org timestamps like <2019-03-28 jeu.> to 2019-03-28
               (or (second (re-find #"(\d{4}-\d{2}-\d{2})" d)) d))
     :author  (:author meta)
     :tags    tags
     :lang    lang
     :slug    slug
     :section section
     :url     url
     :ast     ast
     :path    (str path)}))

(defn- load-posts
  "Load all org files from content/, excluding index files."
  []
  (->> (fs/glob "content" "**/*.org")
       (remove index-file?)
       (remove #(str/starts-with? (str (fs/file-name %)) "."))
       (map load-post)
       (sort-by :date (fn [a b] (compare (or b "") (or a ""))))))

;; ---------------------------------------------------------------------------
;; Site generation
;; ---------------------------------------------------------------------------

(defn- site-context [config lang]
  (let [langs (:languages config)
        menu  (cond-> []
                (some #{"notes"} (map :section []))
                (conj {:name "Notes" :url (str "/" lang "/notes/")})
                true identity)]
    {:site      config
     :lang      lang
     :menu      menu
     :theme-url (:theme-url config)}))

(defn- write-file! [path content]
  (let [parent (fs/parent path)]
    (when parent (fs/create-dirs parent)))
  (spit (str path) content))

(defn- render-post! [config post sections]
  (let [lang    (:lang post)
        langs   (:languages config)
        menu    (cond-> []
                  (contains? sections [lang "notes"])
                  (conj {:name "Notes" :url (str "/" lang "/notes/")})
                  (some #(not= % lang) langs)
                  (into (map (fn [l] {:name l :url (str "/" l "/")})
                             (remove #(= % lang) langs))))
        ctx     (merge (site-context config lang)
                       {:menu    menu
                        :title   (:title post)
                        :date    (:date post)
                        :tags    (:tags post)
                        :content (ast->html (:ast post))})
        html    (render-page "post.html" ctx)
        out     (str "public" (:url post) "index.html")]
    (write-file! out html)))

(defn- render-list! [config lang section posts sections]
  (let [langs (:languages config)
        title (or section "Posts")
        menu  (cond-> []
                (contains? sections [lang "notes"])
                (conj {:name "Notes" :url (str "/" lang "/notes/")})
                (some #(not= % lang) langs)
                (into (map (fn [l] {:name l :url (str "/" l "/")})
                           (remove #(= % lang) langs))))
        ctx   (merge (site-context config lang)
                     {:menu  menu
                      :title title
                      :posts (map #(select-keys % [:title :date :url]) posts)})
        html  (render-page "list.html" ctx)
        out   (if section
                (str "public/" lang "/" section "/index.html")
                (str "public/" lang "/index.html"))]
    (write-file! out html)))

(defn- render-tag-page! [config lang tag posts menu]
  (let [ctx  (merge (site-context config lang)
                    {:menu  menu
                     :tag   tag
                     :posts (map #(select-keys % [:title :date :url]) posts)})
        html (render-page "tag.html" ctx)
        out  (str "public/" lang "/tags/" tag "/index.html")]
    (write-file! out html)))

(defn- render-tags-index! [config lang tag-counts menu]
  (let [ctx  (merge (site-context config lang)
                    {:menu menu
                     :tags (map (fn [[tag count]] {:tag tag :count count})
                                (sort-by first tag-counts))})
        html (render-page "tags-index.html" ctx)
        out  (str "public/" lang "/tags/index.html")]
    (write-file! out html)))

(defn- truncate [s n]
  (if (> (count s) n)
    (str (subs s 0 n) "...")
    s))

(defn- render-feed! [config lang posts]
  (let [ctx {:site  config
             :lang  lang
             :posts (->> posts
                         (take 20)
                         (map (fn [p]
                                (let [plain (-> (:ast p) ast->html
                                               (str/replace #"<[^>]+>" "")
                                               str/trim)]
                                  {:title   (:title p)
                                   :url     (:url p)
                                   :date    (:date p)
                                   :summary (truncate plain 200)}))))}
        xml (render-template "feed.xml" ctx)
        out (str "public/" lang "/feed.xml")]
    (write-file! out xml)))

(defn- copy-static! []
  (when (fs/exists? "static")
    (fs/copy-tree "static" "public" {:replace-existing true})))

(defn build!
  "Build the static site into public/."
  []
  (let [config (load-config)
        posts  (load-posts)
        ;; Collect which [lang section] pairs exist for menu building
        sections (->> posts (map (juxt :lang :section)) set)]
    (println (str "Building " (count posts) " posts..."))

    ;; Render each post
    (doseq [post posts]
      (render-post! config post sections))

    ;; Per-language indexes
    (doseq [lang (:languages config)]
      (let [lang-posts (filter #(= (:lang %) lang) posts)
            langs      (:languages config)
            menu       (cond-> []
                         (contains? sections [lang "notes"])
                         (conj {:name "Notes" :url (str "/" lang "/notes/")})
                         (some #(not= % lang) langs)
                         (into (map (fn [l] {:name l :url (str "/" l "/")})
                                    (remove #(= % lang) langs))))]
        ;; Main index
        (render-list! config lang nil lang-posts sections)

        ;; Section indexes
        (doseq [[section sec-posts] (group-by :section lang-posts)
                :when section]
          (render-list! config lang section sec-posts sections))

        ;; Tag pages
        (let [tag-groups (->> lang-posts
                              (mapcat (fn [p] (map #(vector % p) (:tags p))))
                              (group-by first)
                              (reduce-kv (fn [m tag pairs] (assoc m tag (map second pairs))) {}))]
          (doseq [[tag tag-posts] tag-groups]
            (render-tag-page! config lang tag tag-posts menu))
          (render-tags-index! config lang
                              (->> tag-groups (map (fn [[tag ps]] [tag (count ps)])))
                              menu))

        ;; RSS feed
        (render-feed! config lang lang-posts)))

    ;; Copy static assets
    (copy-static!)

    (println (str "Site built in public/ (" (count posts) " posts)"))))

(defn serve!
  "Build and start a simple HTTP server."
  [{:keys [port] :or {port 1888}}]
  (build!)
  (println (str "Serving at http://localhost:" port))
  (let [server (java.net.ServerSocket. port)]
    (loop []
      (let [socket (.accept server)
            out    (.getOutputStream socket)
            in     (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream socket)))
            line   (.readLine in)
            path   (when line (second (str/split line #"\s+")))
            fpath  (str "public" (if (str/ends-with? (or path "") "/")
                                   (str path "index.html")
                                   path))
            exists (fs/exists? fpath)]
        (if exists
          (let [content (slurp fpath)
                ctype   (cond
                          (str/ends-with? fpath ".html") "text/html"
                          (str/ends-with? fpath ".css")  "text/css"
                          (str/ends-with? fpath ".xml")  "application/xml"
                          :else "application/octet-stream")
                resp    (str "HTTP/1.1 200 OK\r\nContent-Type: " ctype "; charset=utf-8\r\n\r\n" content)]
            (.write out (.getBytes resp))
            (.flush out))
          (let [resp "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\nNot found"]
            (.write out (.getBytes resp))
            (.flush out)))
        (.close socket)
        (recur)))))
