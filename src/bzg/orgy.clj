#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0

(ns bzg.orgy
  "Static blog engine: org files → HTML via organ + selmer."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [selmer.parser :as selmer]
            [bzg.organ :as organ]))

;; ---------------------------------------------------------------------------
;; Paths
;; ---------------------------------------------------------------------------

(def ^:dynamic *input-dir* ".")
(def ^:dynamic *output-dir* "public")
(def ^:dynamic *monolingual* false)
(def ^:dynamic ^:private *current-source-file*
  "Absolute path of the org file currently being rendered. Used by
   resolve-file-url to rewrite relative asset references to
   site-absolute URLs that survive the slug-subdirectory wrap."
  nil)

;; ---------------------------------------------------------------------------
;; Tunables
;; ---------------------------------------------------------------------------

(def ^:private feed-max-posts
  "Maximum number of posts included in an RSS feed."
  20)

(def ^:private homepage-recent-posts
  "Number of recent posts shown on the homepage fallback listing."
  10)

(def ^:private description-max-chars
  "Maximum length of meta description / OpenGraph summary."
  160)

(def ^:private search-body-max-chars
  "Maximum length of each entry's body in the search index."
  500)

(def ^:private watch-interval-ms
  "Polling interval of the serve! source watcher, in milliseconds."
  500)

(def ^:private default-serve-port
  "Default port for the local preview server."
  1888)

;; Compare post :date values descending; missing dates sort last.
(def ^:private date-desc (fn [a b] (compare (or b "") (or a ""))))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(defn- resolve-css-theme
  "Resolve a --theme value to {:link url} or {:inline css}.
   Priority: https URL → file:/// URL → .css file path → pico-themes name."
  [value]
  (when value
    (cond
      (str/starts-with? value "https://")
      {:link value}
      (str/starts-with? value "file:///")
      (let [path (subs value 7)]
        (if (fs/exists? path)
          {:inline (slurp path)}
          (throw (ex-info (str "CSS file not found: " path) {}))))
      (str/ends-with? value ".css")
      (when (fs/exists? value) {:inline (slurp value)})
      (not (str/includes? value " "))
      {:link (str "https://cdn.jsdelivr.net/gh/bzg/pico-themes@latest/" value ".css")}
      :else
      (throw (ex-info (str "Invalid --theme value: " value) {})))))

(defn- load-config [overrides]
  (let [cfg-path (or (:config-path overrides)
                     (let [p (str *input-dir* "/config.edn")] (when (fs/exists? p) p)))
        cfg      (when cfg-path (-> cfg-path slurp edn/read-string))
        dir-name (-> *input-dir* fs/file-name str (str/replace "_" " "))]
    ;; :languages is intentionally not defaulted: build! infers it
    ;; from posts and falls back to ["en"] only as a last resort, so
    ;; an entirely non-English blog doesn't get a phantom /en/ tree.
    (merge {:title          (or (:title cfg) dir-name)
            :base-url       (str/replace (or (:base-url cfg) "") #"/$" "")
            :copyright      (or (:copyright cfg) "")
            :resolved-theme (resolve-css-theme (or (:theme overrides) (:theme cfg)))
            :languages      (:languages cfg)
            :quick-search   (:quick-search cfg)
            :theme-toggle   (:theme-toggle cfg)
            :menu           (:menu cfg)}
           (dissoc overrides :config-path :theme))))

;; ---------------------------------------------------------------------------
;; Default templates (used when templates/ dir is absent)
;; ---------------------------------------------------------------------------

(def ^:private mpl-license
  "// @license magnet:?xt=urn:btih:3877d6d54b3accd4bc32f8a48bf32ebc0901502a&dn=mpl-2.0.txt MPL-2.0")

(def ^:private sun-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"5\"/><line x1=\"12\" y1=\"1\" x2=\"12\" y2=\"3\"/><line x1=\"12\" y1=\"21\" x2=\"12\" y2=\"23\"/><line x1=\"4.22\" y1=\"4.22\" x2=\"5.64\" y2=\"5.64\"/><line x1=\"18.36\" y1=\"18.36\" x2=\"19.78\" y2=\"19.78\"/><line x1=\"1\" y1=\"12\" x2=\"3\" y2=\"12\"/><line x1=\"21\" y1=\"12\" x2=\"23\" y2=\"12\"/><line x1=\"4.22\" y1=\"19.78\" x2=\"5.64\" y2=\"18.36\"/><line x1=\"18.36\" y1=\"5.64\" x2=\"19.78\" y2=\"4.22\"/></svg>")

(def ^:private moon-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z\"/></svg>")

(def ^:private rss-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1em\" height=\"1em\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" style=\"vertical-align:-.125em\"><circle cx=\"5\" cy=\"19\" r=\"2\" fill=\"currentColor\" stroke=\"none\"/><path d=\"M4 11a9 9 0 0 1 9 9\"/><path d=\"M4 4a16 16 0 0 1 16 16\"/></svg>")

(def ^:private base-css
  ".container{max-width:1080px}
header.container{padding-bottom:1rem;margin-bottom:1.2rem}
body>footer{text-align:center}
iframe{width:100%;aspect-ratio:4/3;display:block;margin:1em auto;border:none}
article img{display:block;margin:1em auto;max-width:90%}
article figure{margin:1em auto;width:fit-content;max-width:100%}
article .align-left{margin-left:0;margin-right:auto}
article .align-right{margin-left:auto;margin-right:0}
article .align-center{margin-left:auto;margin-right:auto}
article figure.align-left>figcaption{text-align:left}
article figure.align-right>figcaption{text-align:right}
article figure.align-center>figcaption{text-align:center}
article figure>figcaption{text-align:center}
article>header h1{margin:0 0 .25em}
article>header .post-meta{display:flex;flex-wrap:wrap;align-items:baseline;justify-content:flex-end;gap:.5em}
article>header .tags{display:flex;gap:.35em}
article>header .tags a{text-decoration:none}
article>header .tags mark{font-size:.85em}
.tags-clear{text-decoration:none;opacity:.4;font-size:1.1em}
.tags-clear:hover{opacity:1}
article>footer nav{display:flex;justify-content:space-between;margin-top:2em;padding-top:1em;border-top:1px solid var(--pico-muted-border-color)}
nav h1{margin:0;font-size:1.5rem;font-family:inherit}
article h2{font-size:1.3rem}
article h3{font-size:1.15rem}
article h4{font-size:1.05rem}
article h5,article h6{font-size:1rem}
{% if site.quick-search %}
.search-wrap{position:relative}
#search-input{margin-bottom:0;padding:.25rem .5rem;height:auto;font-size:.875rem;width:10rem;background-image:none}
.search-results{position:absolute;right:0;top:100%;background:var(--pico-background-color);border:1px solid var(--pico-muted-border-color);border-radius:4px;max-height:60vh;overflow-y:auto;width:320px;z-index:10;display:none;margin-top:.25em;padding:0;list-style:none}
.search-results li a{display:block;padding:.5em .75em;text-decoration:none}
.search-results li a:hover{background:var(--pico-primary-focus)}
.search-results li time{font-size:.8em;opacity:.6;margin-left:.5em}
.visually-hidden{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border:0}
{% endif %}
{% if site.theme-toggle %}
.theme-toggle{background:none;border:none;padding:.25rem;line-height:1;cursor:pointer;color:var(--pico-color)}
{% endif %}")

(def ^:private search-js
  (str mpl-license "
(function(){
  var input = document.getElementById('search-input'),
      list  = document.getElementById('search-results'),
      idx   = null;
  function esc(s){
    var d = document.createElement('div');
    d.textContent = s;
    return d.innerHTML;
  }
  function load(cb){
    if (idx) return cb();
    fetch('{{lang-prefix}}/search.json')
      .then(function(r){ return r.json(); })
      .then(function(d){ idx = d; cb(); });
  }
  function render(q){
    if (!q) { list.style.display = 'none'; return; }
    var lq = q.toLowerCase(),
        hits = idx.filter(function(p){
          return (p.t && p.t.toLowerCase().indexOf(lq) >= 0)
              || (p.b && p.b.toLowerCase().indexOf(lq) >= 0);
        }).slice(0, 15);
    if (!hits.length) { list.style.display = 'none'; return; }
    list.innerHTML = hits.map(function(p){
      return '<li role=\"option\"><a href=\"' + esc(p.u) + '\">'
           + esc(p.t || '')
           + (p.d ? '<time>' + esc(p.d) + '</time>' : '')
           + '</a></li>';
    }).join('');
    list.style.display = 'block';
  }
  input.addEventListener('input', function(){
    load(function(){ render(input.value.trim()); });
  });
  document.addEventListener('click', function(e){
    if (!e.target.closest('.search-wrap')) list.style.display = 'none';
  });
})();
// @license-end"))

(def ^:private theme-toggle-js
  (str mpl-license "
(function(){
  var sun  = '" sun-svg "',
      moon = '" moon-svg "',
      btn  = document.getElementById('theme-toggle'),
      root = document.documentElement,
      stored = localStorage.getItem('theme'),
      theme  = stored || (matchMedia('(prefers-color-scheme:dark)').matches ? 'dark' : 'light');
  function apply(t){
    root.setAttribute('data-theme', t);
    btn.innerHTML = (t === 'dark' ? sun : moon);
    localStorage.setItem('theme', t);
  }
  apply(theme);
  btn.addEventListener('click', function(e){
    e.preventDefault();
    apply(root.getAttribute('data-theme') === 'dark' ? 'light' : 'dark');
  });
})();
// @license-end"))

(def ^:private default-templates
  {"base.html"
   (str "<!DOCTYPE html>
<html lang=\"{{lang}}\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>{% if title %}{{title}} — {% endif %}{{site.title}}</title>
  {% if description %}<meta name=\"description\" content=\"{{description}}\">{% endif %}
  <meta property=\"og:title\" content=\"{% if title %}{{title}}{% else %}{{site.title}}{% endif %}\">
  {% if description %}<meta property=\"og:description\" content=\"{{description}}\">{% endif %}
  <meta property=\"og:type\" content=\"{% if date %}article{% else %}website{% endif %}\">
  {% if canonical %}<meta property=\"og:url\" content=\"{{canonical}}\">
  <link rel=\"canonical\" href=\"{{canonical}}\">{% endif %}
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
  {% if theme-link %}<link rel=\"stylesheet\" href=\"{{theme-link}}\">{% endif %}
  {% if theme-inline %}<style>{{theme-inline|safe}}</style>{% endif %}
  {% if site.base-url|not-empty %}<link rel=\"alternate\" type=\"application/rss+xml\" title=\"{{site.title}} ({{lang}})\" href=\"{{lang-prefix}}/feed.xml\">{% endif %}
  <link rel=\"sitemap\" type=\"application/xml\" href=\"/sitemap.xml\">
  <style>
" base-css "
  </style>
  {% if has-code %}<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11/build/styles/default.min.css\">{% endif %}
</head>
<body>
  <header class=\"container\">
    <nav aria-label=\"Main\">
      <ul>
        <li><h1><a href=\"{{lang-prefix}}/\">{{site.title}}</a></h1></li>
      </ul>
      <ul>
        {% for item in menu %}
        <li><a href=\"{{item.url}}\">{{item.description}}</a></li>
        {% endfor %}
        {% if site.quick-search %}
        <li class=\"search-wrap\">
          <label for=\"search-input\" class=\"visually-hidden\">Search</label>
          <input type=\"search\" id=\"search-input\" placeholder=\"\" autocomplete=\"off\" aria-controls=\"search-results\">
          <ul class=\"search-results\" id=\"search-results\" role=\"listbox\" aria-label=\"Search results\"></ul>
        </li>
        {% endif %}
        {% if site.theme-toggle %}
        <li><a href=\"#\" id=\"theme-toggle\" class=\"theme-toggle\" aria-label=\"Toggle theme\">" sun-svg "</a></li>
        {% endif %}
      </ul>
    </nav>
  </header>
  <main class=\"container\">
    {{body|safe}}
  </main>
  {% if site.copyright|not-empty %}
  <footer class=\"container\">
    <p>{{site.copyright}} — Made with <a href=\"https://codeberg.org/bzg/orgy\">Orgy</a>{% if site.base-url|not-empty %} — <a href=\"{{lang-prefix}}/feed.xml\">RSS</a>{% endif %}</p>
  </footer>
  {% endif %}
  {% if site.quick-search %}<script>" search-js "</script>{% endif %}
  {% if site.theme-toggle %}<script>" theme-toggle-js "</script>{% endif %}
  {% if has-code %}
  <script src=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11/build/highlight.min.js\"></script>
  {% for hl-lang in hl-langs %}
  <script src=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11/build/languages/{{hl-lang}}.min.js\"></script>
  {% endfor %}
  <script>" mpl-license " hljs.highlightAll(); // @license-end</script>
  {% endif %}
  {% if has-math %}
  <script>window.MathJax={tex:{inlineMath:[['\\\\(','\\\\)']],displayMath:[['\\\\[','\\\\]']]}};</script>
  <script src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\" async></script>
  {% endif %}
</body>
</html>")

   "post.html"
   "<article>
  {% if title %}
  <header>
    <h1>{{title}}</h1>
    <div class=\"post-meta\">
      {% if author %}<span class=\"author\">{{author}}</span>{% endif %}
      {% if date %}<time datetime=\"{{date}}\">{{date}}</time>{% endif %}
      {% if tags|not-empty %}
      <nav class=\"tags\" aria-label=\"Tags\">
        {% for tag in tags %}
        <a href=\"{{lang-prefix}}/tags/{{tag}}/\"><mark>{{tag}}</mark></a>
        {% endfor %}
        <a href=\"{{lang-prefix}}/tags/\" class=\"tags-clear\" aria-label=\"All tags\">&times;</a>
      </nav>
      {% endif %}
    </div>
  </header>
  {% endif %}
  <section>
    {{content|safe}}
  </section>
  {% if has-nav %}
  <footer>
    <nav aria-label=\"Adjacent posts\">
      {% if next %}<a href=\"{{next.url}}\">&larr; {{next.title}}</a>{% endif %}
      {% if prev %}<a href=\"{{prev.url}}\" style=\"margin-left:auto\">{{prev.title}} &rarr;</a>{% endif %}
    </nav>
  </footer>
  {% endif %}
</article>"

   "list.html"
   (str "<section>
  <h1>{{title|capitalize}}{% if site.base-url|not-empty %} <a href=\"{{lang-prefix}}/feed.xml\" aria-label=\"RSS feed\">" rss-svg "</a>{% endif %}</h1>
  <ul>
    {% for post in posts %}
    <li>
      <a href=\"{{post.url}}\">{{post.title}}</a>
      {% if post.date %}<time datetime=\"{{post.date}}\">{{post.date}}</time>{% endif %}
    </li>
    {% endfor %}
  </ul>
</section>")

   "tag.html"
   (str "<section>
  <h1>{{tag|capitalize}}{% if site.base-url|not-empty %} <a href=\"{{lang-prefix}}/tags/{{tag}}/feed.xml\" aria-label=\"RSS feed\">" rss-svg "</a>{% endif %}</h1>
  <ul>
    {% for post in posts %}
    <li>
      <a href=\"{{post.url}}\">{{post.title}}</a>
      {% if post.date %}<time datetime=\"{{post.date}}\">{{post.date}}</time>{% endif %}
    </li>
    {% endfor %}
  </ul>
</section>")

   "tags-index.html"
   "<section>
  <h1>Tags</h1>
  <ul>
    {% for entry in tags %}
    <li><a href=\"{{lang-prefix}}/tags/{{entry.tag}}/\">{{entry.tag}}</a> ({{entry.count}})</li>
    {% endfor %}
  </ul>
</section>"

   "feed.xml"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">
  <channel>
    <title>{{site.title}}</title>
    <link>{{site.base-url}}{{lang-prefix}}/</link>
    <description>{{site.title}}</description>
    <atom:link href=\"{{feed-url}}\" rel=\"self\" type=\"application/rss+xml\"/>
    {% for post in posts %}
    <item>
      <title>{{post.title}}</title>
      <link>{{site.base-url}}{{post.url}}</link>
      <guid>{{site.base-url}}{{post.url}}</guid>
      {% if post.date %}<pubDate>{{post.rfc822-date}}</pubDate>{% endif %}
      <description><![CDATA[{{post.content}}]]></description>
    </item>
    {% endfor %}
  </channel>
</rss>"

   "sitemap.xml"
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
  {% for entry in entries %}
  <url>
    <loc>{{entry.loc}}</loc>
    {% if entry.lastmod %}<lastmod>{{entry.lastmod}}</lastmod>{% endif %}
  </url>
  {% endfor %}
</urlset>"

   "redirect.html"
   "<!DOCTYPE html>
<html>
  <head><meta http-equiv=\"refresh\" content=\"0;url={{target}}\"></head>
  <body></body>
</html>"})

(defn- resolve-template
  "Return template string: from templates/ file if present, else default."
  [template-name]
  (let [file-path (str *input-dir* "/templates/" template-name)]
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
;; Helpers
;; ---------------------------------------------------------------------------

(def ^:private hljs-builtin
  "Languages included in the default highlight.js bundle."
  #{"bash" "c" "cpp" "csharp" "css" "diff" "go" "graphql" "ini" "java"
    "javascript" "json" "kotlin" "less" "lua" "makefile" "markdown" "objectivec"
    "perl" "php" "php-template" "plaintext" "python" "python-repl" "r" "ruby"
    "rust" "scss" "shell" "sql" "swift" "typescript" "vbnet" "wasm" "xml" "yaml"})

(def ^:private hljs-lang-map
  "Map Org source block language names to highlight.js names."
  {"emacs-lisp" "lisp" "elisp" "lisp"
   "sh" "bash" "zsh" "bash"
   "js" "javascript" "ts" "typescript"})

(defn- collect-page-features
  "Walk the AST once to collect per-page render flags:
   - :has-code / :hl-langs  — src-block languages for highlight.js
   - :has-math              — any LaTeX fragment or environment (triggers MathJax)"
  [ast]
  (let [langs (volatile! #{})
        math? (volatile! false)]
    (letfn [(walk [node]
              (when (map? node)
                (case (:type node)
                  :src-block         (when-let [l (some-> (:language node) str/lower-case)]
                                       (vswap! langs conj (get hljs-lang-map l l)))
                  :latex-fragment    (vreset! math? true)
                  :latex-environment (vreset! math? true)
                  nil)
                (when (coll? (:content node))  (run! walk (:content node)))
                (when (coll? (:title node))    (run! walk (:title node)))
                (when (coll? (:children node)) (run! walk (:children node)))
                (when (coll? (:items node))    (run! walk (:items node)))))]
      (run! walk (:children ast))
      {:has-code (boolean (seq @langs))
       :hl-langs (vec (remove hljs-builtin @langs))
       :has-math @math?})))

(defn- truncate [s n]
  (if (> (count s) n)
    (str (subs s 0 n) "...")
    s))

(defn- escape-html [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- normalize-rel-path
  "Collapse '.' and '..' segments in a relative path string. Leading
   '..' segments that cannot be resolved are kept."
  [path]
  (->> (str/split path #"/")
       (reduce (fn [acc part]
                 (cond
                   (or (= part "") (= part ".")) acc
                   (= part "..") (if (and (seq acc) (not= (peek acc) ".."))
                                   (pop acc)
                                   (conj acc part))
                   :else (conj acc part)))
               [])
       (str/join "/")))

(defn- resolve-rel-against-source
  "Resolve a relative path against the current source file's directory,
   returning a normalized path relative to *input-dir*."
  [target]
  (if-let [src *current-source-file*]
    (let [rel (str (fs/relativize *input-dir* src))
          idx (str/last-index-of rel "/")
          dir (if idx (subs rel 0 idx) "")]
      (normalize-rel-path (if (str/blank? dir) target (str dir "/" target))))
    (normalize-rel-path target)))

(defn- org-target->url
  "Convert a file:foo.org target into a slug URL. Relative targets
   resolve against the current source file first."
  [target]
  (let [resolved (if (str/starts-with? target "/")
                   (subs target 1)
                   (resolve-rel-against-source target))
        fname    (str (fs/file-name (fs/path resolved)))
        slug     (or (second (re-matches #"(.+?)\.\w{2}\.org$" fname))
                     (second (re-matches #"(.+?)\.org$" fname)))
        lang     (or (second (re-matches #".*\.(\w{2})\.org$" fname)) "en")
        parts    (str/split resolved #"/")
        section  (when (> (count parts) 1) (first parts))
        lp       (if *monolingual* "" (str "/" lang))]
    (if section (str lp "/" section "/" slug "/") (str lp "/" slug "/"))))

(defn- resolve-file-url
  "Rewrite file: links: .org targets become slug URLs, static/ paths
   are stripped, and relative assets become site-absolute URLs anchored
   at the current source file."
  [node]
  (if (not= :file (:link-type node))
    (:url node)
    (let [target (:target node)]
      (if (str/ends-with? target ".org")
        (org-target->url target)
        (let [expanded (str/replace-first target #"^~" (System/getProperty "user.home"))]
          (cond
            (re-find #"/static/" expanded)        (str "/" (second (re-find #"/static/(.+)$" expanded)))
            (str/starts-with? expanded "static/") (str/replace-first expanded #"^static/" "/")
            (str/starts-with? expanded "/")       expanded
            :else                                 (str "/" (resolve-rel-against-source expanded))))))))

(def ^:private image-ext-re #"\.(?:png|jpg|jpeg|gif|svg|webp)$")

(def ^:dynamic ^:private *ignored-dirs*
  "Directories to skip when scanning for content."
  #{"static" "public" "templates" ".git"})

(def ^:private ignored-root-files
  "Files orgy owns that must never leak into the output. Users keep
   unrelated tooling out via directory layout or --skip-dirs."
  #{"config.edn"})

(defn- iso->rfc822
  "Convert ISO date string (2024-03-28) to RFC 822 format for RSS."
  [iso-date]
  (when iso-date
    (try
      (let [dt (java.time.LocalDate/parse iso-date)
            zdt (.atStartOfDay dt (java.time.ZoneOffset/UTC))]
        (.format zdt (java.time.format.DateTimeFormatter/ofPattern "EEE, dd MMM yyyy HH:mm:ss Z" java.util.Locale/ENGLISH)))
      (catch Exception _ iso-date))))

;; ---------------------------------------------------------------------------
;; Inline node rendering (organ returns parsed inline node vectors)
;; ---------------------------------------------------------------------------

(def ^:private inline-wrap
  {:bold "strong" :italic "em" :underline "u" :strike "del"})

(declare render-inline)

(defn- render-link [node]
  (let [url (or (resolve-file-url node) "")
        e   escape-html]
    (if (re-find image-ext-re url)
      (let [alt (or (organ/inline-text (:children node)) "")]
        (str "<img src=\"" (e url) "\" alt=\"" (e alt) "\""
             (when (str/blank? alt) " role=\"presentation\"") ">"))
      (str "<a href=\"" (e url) "\">"
           (if (seq (:children node)) (render-inline (:children node)) (e url))
           "</a>"))))

(defn- render-inline
  "Render a vector of organ inline nodes to HTML."
  [nodes]
  (when (seq nodes)
    (str/join
     (map (fn [node]
            (let [t (:type node)]
              (cond
                (= t :text)            (escape-html (:value node))
                (inline-wrap t)        (let [tag (inline-wrap t)]
                                         (str "<" tag ">" (render-inline (:children node)) "</" tag ">"))
                (#{:code :verbatim} t) (str "<code>" (escape-html (:value node)) "</code>")
                (= t :link)            (render-link node)
                (= t :footnote-ref)    (let [l (escape-html (:label node))]
                                         (str "<sup><a href=\"#fn-" l "\" id=\"fnref-" l "\">" l "</a></sup>"))
                (= t :timestamp)       (:raw node)
                (= t :latex-fragment)  (let [v (escape-html (:value node))]
                                         (case (:kind node)
                                           (:paren :dollar)    (str "\\(" v "\\)")
                                           (:bracket :dollars) (str "\\[" v "\\]")
                                           v))
                :else                  "")))
          nodes))))

;; ---------------------------------------------------------------------------
;; AST → HTML rendering
;; ---------------------------------------------------------------------------

(declare render-node)

(defn- render-children [children]
  (str/join "\n" (map render-node children)))

(defn- render-list-items [items ordered?]
  (let [tag (if ordered? "ol" "ul")]
    (str "<" tag ">"
         (str/join (map (fn [item]
                          (str "<li>" (render-inline (:content item))
                               (when-let [kids (:children item)] (render-children kids))
                               "</li>"))
                        items))
         "</" tag ">")))

(defn- render-table [{:keys [rows has-header]}]
  (let [[header & body] (if has-header rows (cons nil rows))
        th (fn [c] (str "<th>" (render-inline c) "</th>"))
        td (fn [c] (str "<td>" (render-inline c) "</td>"))
        tr (fn [row tag-fn] (str "<tr>" (str/join (map tag-fn row)) "</tr>"))]
    (str "<table>"
         (when (and has-header header) (str "<thead>" (tr header th) "</thead>"))
         "<tbody>"
         (str/join (map #(tr % td) (if has-header body rows)))
         "</tbody></table>")))

(defn- attrs-str
  "Serialize a map of HTML attributes to a leading-space-prefixed string."
  [attrs]
  (str/join (map (fn [[k v]] (str " " (name k) "=\"" (escape-html v) "\"")) attrs)))

(defn- html-attrs [node] (attrs-str (get-in node [:affiliated :attr :html])))

(defn- render-image
  "Render a standalone image link as <img> (optionally wrapped in <figure>)."
  [link attrs caption]
  (let [url   (resolve-file-url link)
        alt   (or (:alt attrs) (organ/inline-text (:children link)) caption "")
        align (some-> (:align attrs) str/trim str/lower-case)
        cls   (when (#{"left" "right" "center"} align) (str "align-" align))
        extra (cond-> (dissoc attrs :alt :align)
                (and cls (not caption))
                (update :class #(if (str/blank? %) cls (str % " " cls))))
        img   (str "<img src=\"" (escape-html url) "\" alt=\"" (escape-html alt) "\""
                   (when (str/blank? alt) " role=\"presentation\"")
                   (attrs-str extra) ">")]
    (if caption
      (str "<figure" (when cls (str " class=\"" cls "\"")) ">"
           img "<figcaption>" (escape-html caption) "</figcaption></figure>")
      img)))

(def ^:private org-keyword-re #"^\s*#\+\w+(?:\[\])?\s*:.*")

(defn- render-paragraph [node]
  (let [c       (:content node)
        attrs   (get-in node [:affiliated :attr :html])
        caption (get-in node [:affiliated :caption])]
    (if-let [text (organ/inline-text c)]
      (cond
        ;; Skip org keywords that organ didn't parse as metadata
        (re-matches org-keyword-re text) ""
        ;; Standalone image link → <img>/<figure>
        (and (= 1 (count c))
             (= :link (:type (first c)))
             (re-find image-ext-re (or (:target (first c)) (:url (first c)) "")))
        (render-image (first c) attrs caption)
        ;; Regular paragraph
        :else (str "<p" (html-attrs node) ">" (render-inline c) "</p>"))
      "")))

(defn- render-node [node]
  (case (:type node)
    :paragraph    (render-paragraph node)
    :html-line    (:content node)
    :section      (let [level (inc (:level node))
                        body  (render-children (:children node))]
                    (if (<= level 6)
                      (str "<h" level ">" (render-inline (:title node)) "</h" level ">\n" body)
                      (str "<p><strong>" (render-inline (:title node)) "</strong></p>\n" body)))
    :src-block    (let [lang (some-> (:language node) str/lower-case (as-> l (get hljs-lang-map l l)))]
                    (str "<pre><code"
                         (when lang (str " class=\"language-" lang "\""))
                         ">" (escape-html (:content node)) "</code></pre>"))
    :quote-block  (str "<blockquote>" (render-children (:children node)) "</blockquote>")
    :block        (if (and (= :export (:block-type node))
                           (= "html" (some-> (:args node) str/lower-case)))
                    (:content node)
                    (str "<pre>" (escape-html (:content node)) "</pre>"))
    :list         (if (:description node)
                    (str "<dl>"
                         (str/join (map (fn [it]
                                          (str "<dt>" (render-inline (:term it)) "</dt>"
                                               "<dd>" (render-inline (:definition it)) "</dd>"))
                                        (:items node)))
                         "</dl>")
                    (render-list-items (:items node) (:ordered node)))
    :table        (render-table node)
    :fixed-width  (str "<pre>" (escape-html (:content node)) "</pre>")
    :latex-environment (str "<div class=\"math-display\">"
                            (escape-html (:content node))
                            "</div>")
    :footnote-def (let [lbl (escape-html (:label node))]
                    (str "<div class=\"footnote\" id=\"fn-" lbl "\"><sup>" lbl "</sup> "
                         (render-inline (:content node))
                         " <a href=\"#fnref-" lbl "\">↩</a></div>"))
    (:comment :drawer) ""
    ;; fallback
    (if (:children node) (render-children (:children node)) "")))

(defn- ast->html
  "Render an organ AST document to HTML body content."
  [ast]
  (render-children (:children ast)))

(declare node->text)

(defn- children-text [nodes]
  (str/join " " (map node->text nodes)))

(defn- node->text
  "Extract plain text from an AST node, recursing into children."
  [node]
  (case (:type node)
    :paragraph    (let [text (organ/inline-text (:content node))]
                    (if (and text (re-matches org-keyword-re text)) "" (or text "")))
    :section      (str (organ/inline-text (:title node)) " " (children-text (:children node)))
    :list         (children-text (:items node))
    :list-item    (str (organ/inline-text (:content node)) " " (children-text (:children node)))
    :quote-block  (children-text (:children node))
    (:src-block :fixed-width) (:content node)
    :block        (if (= :export (:block-type node)) "" (:content node))
    :table        (str/join " " (mapcat (fn [row] (map organ/inline-text row)) (:rows node)))
    :footnote-def (organ/inline-text (:content node))
    (:comment :drawer :html-line :latex-line :latex-environment) ""
    (if (:children node) (children-text (:children node)) "")))

(defn- ast->text
  "Extract plain text from an organ AST document."
  [ast]
  (-> (str/join " " (map node->text (:children ast)))
      (str/replace #"\s+" " ")
      str/trim))

;; ---------------------------------------------------------------------------
;; Content loading
;; ---------------------------------------------------------------------------

(defn- parse-tags-header [org-text]
  (when-let [[_ tags] (re-find #"(?m)^#\+tags:\s*(.+)$" org-text)]
    (str/split (str/trim tags) #"\s+")))

(defn- file-lang
  "foo.fr.org → \"fr\"; no suffix → nil."
  [path]
  (second (re-matches #".*\.(\w{2})\.org$" (str (fs/file-name path)))))

(defn- lang-prefix
  "\"/{lang}\" in multilingual mode, \"\" in monolingual mode."
  [lang]
  (if *monolingual* "" (str "/" lang)))

(defn- file-slug
  "some-post.fr.org → \"some-post\"."
  [path]
  (let [n (str (fs/file-name path))]
    (or (second (re-matches #"(.+?)\.\w{2}\.org$" n))
        (second (re-matches #"(.+?)\.org$" n)))))

(defn- file-section
  "First path component relative to the content root, or nil for root files."
  [path]
  (let [parts (-> (fs/relativize *input-dir* path) str (str/split #"/"))]
    (when (> (count parts) 1) (first parts))))

(defn- index-file?
  "True if file is an index.org (optionally language-suffixed)."
  [path]
  (let [name (str (fs/file-name path))]
    (re-matches #"index(?:\.\w{2})?\.org" name)))

(defn- prescan-org-file
  "Read an org file and extract lightweight metadata via regex.
   The file text is not retained — only :path, :lang, :tags, :draft?."
  [path]
  (let [org-text (slurp (str path))
        lang    (or (file-lang path)
                    (second (re-find #"(?m)^#\+language:\s*(\S+)" org-text))
                    "en")
        tags    (or (parse-tags-header org-text) [])
        draft?  (boolean (re-find #"(?mi)^#\+draft:\s*true\s*$" org-text))]
    {:path   (str path)
     :lang   lang
     :tags   tags
     :draft? draft?}))

(defn- load-post
  "Parse a pre-scanned org file into a full post map.
   Re-reads the file for organ parsing; prescan metadata avoids redundant extraction."
  [scanned]
  (let [{:keys [path lang tags]} scanned
        org-text (slurp path)
        ast      (organ/parse-org org-text)
        meta     (:meta ast)
        slug     (file-slug path)
        section  (file-section path)
        url      (if section
                   (str (lang-prefix lang) "/" section "/" slug "/")
                   (str (lang-prefix lang) "/" slug "/"))]
    {:title   (:title meta)
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
     :path    path}))

(defn- in-ignored-dir? [root path]
  (let [rel (str (fs/relativize root path))]
    (some #(str/starts-with? rel (str % "/")) *ignored-dirs*)))

(defn- list-org-files
  "List all org files from content root, excluding hidden/ignored dirs."
  []
  (let [root *input-dir*]
    (->> (distinct (concat (fs/glob root "*.org")
                           (fs/glob root "**/*.org")))
         (filter #(fs/regular-file? %))
         (remove #(str/starts-with? (str (fs/file-name %)) "."))
         (remove #(in-ignored-dir? root %)))))

(defn- load-posts
  "Parse pre-scanned org files into post maps, excluding drafts.
   Expects output of prescan-org-file."
  [scanned-files]
  (->> scanned-files
       (remove #(index-file? (:path %)))
       (remove :draft?)
       (map load-post)
       (sort-by :date date-desc)))

;; ---------------------------------------------------------------------------
;; Site generation
;; ---------------------------------------------------------------------------

(defn- build-menu
  "Build the nav menu for a given language. :menu may be a vector
   (shared) or a map keyed by language. When absent, all root pages
   and sections are shown."
  [lang posts sections langs config]
  (let [lp         (lang-prefix lang)
        menu-cfg   (let [m (:menu config)]
                     (cond (vector? m) m
                           (map? m)    (get m (keyword lang) (get m lang))))
        root-pages (->> posts
                        (filter #(and (= (:lang %) lang) (nil? (:section %))))
                        (map (fn [p] {:url (:url p)
                                      :description (or (:title p) (str/capitalize (:slug p)))
                                      :slug (:slug p)})))
        sec-links  (->> sections
                        (filter (fn [[l s]] (and (= l lang) s)))
                        (map (fn [[_ s]] {:url (str lp "/" s "/")
                                          :description (str/capitalize s)
                                          :slug s})))
        page-links (if menu-cfg
                     (let [by-slug (into {} (map (juxt :slug identity) (concat root-pages sec-links)))]
                       (->> menu-cfg
                            (keep #(if (map? %) (select-keys % [:url :description]) (by-slug %)))
                            vec))
                     (vec (concat root-pages (sort-by :description sec-links))))
        tags-link  (when (some (comp seq :tags) posts)
                     [{:url (str lp "/tags/") :description "Tags"}])
        lang-links (when-not *monolingual*
                     (for [l langs :when (not= l lang)]
                       {:url (str "/" l "/") :description (str/upper-case l)}))]
    (vec (concat (map #(dissoc % :slug) page-links) tags-link lang-links))))

(defn- site-context [config lang]
  (let [{:keys [link inline]} (:resolved-theme config)]
    {:site         config
     :lang         lang
     :lang-prefix  (lang-prefix lang)
     :theme-link   link
     :theme-inline inline}))

(defn- write-file! [path content]
  (when-let [p (fs/parent path)] (fs/create-dirs p))
  (spit (str path) content))

(defn- post-description [post]
  (truncate (ast->text (:ast post)) description-max-chars))

(defn- canonical-url [config url]
  (when-let [base (not-empty (:base-url config))] (str base url)))

(defn- page-title
  "Root-level pages have their first char upper-cased so lowercase
   nav slugs read as proper page headings. Section posts keep their
   title verbatim."
  [post]
  (let [t (or (:title post) "")]
    (if (or (:section post) (empty? t))
      t
      (str (str/upper-case (subs t 0 1)) (subs t 1)))))

(defn- post-summary [post]
  (select-keys post [:title :date :url]))

(defn- render-post! [config post menu prev-post next-post]
  (binding [*current-source-file* (:path post)]
    (let [lang (:lang post)
          ctx  (merge (site-context config lang)
                      (collect-page-features (:ast post))
                      {:menu        menu
                       :title       (page-title post)
                       :date        (:date post)
                       :author      (:author post)
                       :tags        (:tags post)
                       :description (post-description post)
                       :canonical   (canonical-url config (:url post))
                       :has-nav     (boolean (or prev-post next-post))
                       :prev        (when prev-post (select-keys prev-post [:title :url]))
                       :next        (when next-post (select-keys next-post [:title :url]))
                       :content     (ast->html (:ast post))})]
      (write-file! (str *output-dir* (:url post) "index.html")
                   (render-page "post.html" ctx)))))

(defn- render-list!
  "Render the index page for a (non-nil) section. The root index
   goes through render-index! instead."
  [config lang section posts menu]
  (write-file! (str *output-dir* (lang-prefix lang) "/" section "/index.html")
               (render-page "list.html"
                            (merge (site-context config lang)
                                   {:menu menu :title section :posts (map post-summary posts)}))))

(defn- find-index-file
  "Find an index org file: tries index.{lang}.org, then index.org."
  [lang]
  (some #(when (fs/exists? %) (str %))
        [(str *input-dir* "/index." lang ".org")
         (str *input-dir* "/index.org")]))

(defn- render-index! [config lang posts index-file menu]
  (let [lp  (lang-prefix lang)
        out (str *output-dir* lp "/index.html")
        ctx (merge (site-context config lang)
                   {:menu menu :canonical (canonical-url config (str lp "/"))})]
    (if index-file
      (binding [*current-source-file* index-file]
        (let [ast (organ/parse-org (slurp index-file))]
          (write-file! out (render-page "post.html"
                                        (merge ctx (collect-page-features ast)
                                               {:title       (:title (:meta ast))
                                                :description (truncate (ast->text ast) description-max-chars)
                                                :content     (ast->html ast)})))))
      (write-file! out (render-page "list.html"
                                    (merge ctx {:title (:title config)
                                                :posts (map post-summary (take homepage-recent-posts posts))}))))))

(defn- render-tag-page! [config lang tag posts menu]
  (write-file! (str *output-dir* (lang-prefix lang) "/tags/" tag "/index.html")
               (render-page "tag.html"
                            (merge (site-context config lang)
                                   {:menu menu :tag tag :posts (map post-summary posts)}))))

(defn- render-tags-index! [config lang tag-counts menu]
  (write-file! (str *output-dir* (lang-prefix lang) "/tags/index.html")
               (render-page "tags-index.html"
                            (merge (site-context config lang)
                                   {:menu menu
                                    :tags (map (fn [[t n]] {:tag t :count n})
                                               (sort-by first tag-counts))}))))


(defn- absolutize-urls
  "Prefix root-relative href= and src= attributes with base-url so
   feed content remains valid when aggregated outside the site.
   Already-absolute URLs (http://, https://, mailto:, #-fragments)
   are left untouched. The second replace is a safety net for
   attribute values that happen to be bare relative paths — in
   practice orgy always emits site-rooted URLs, but hand-written
   #+html: snippets may not."
  [html base-url]
  (-> html
      (str/replace #"((?:href|src)=\")/" (str "$1" base-url "/"))
      (str/replace #"((?:href|src)=\")(?!https?://|mailto:|#|/)" (str "$1" base-url "/"))))

(defn- render-feed!
  ([config lang posts]
   (render-feed! config lang posts (str *output-dir* (lang-prefix lang) "/feed.xml")))
  ([config lang posts out-path]
   (when-let [base (not-empty (:base-url config))]
     (let [rel-path (str "/" (fs/relativize *output-dir* out-path))
           ctx {:site     config
                :lang     lang
                :feed-url (str base rel-path)
                :posts    (->> posts
                               (take feed-max-posts)
                               (map (fn [p]
                                      {:title      (:title p)
                                       :url        (:url p)
                                       :date       (:date p)
                                       :rfc822-date (iso->rfc822 (:date p))
                                       :content    (str/replace (absolutize-urls (ast->html (:ast p)) base)
                                                                "]]>" "]]&gt;")})))}
           xml (render-template "feed.xml" ctx)]
       (write-file! out-path xml)))))

(defn- render-search-index! [lang posts]
  ;; Short keys minimize search.json payload: :t title, :u url,
  ;; :d date, :b body.
  (let [entries (map (fn [p]
                       {:t (:title p) :u (:url p) :d (:date p)
                        :b (truncate (ast->text (:ast p)) search-body-max-chars)})
                     posts)
        out     (str *output-dir* (lang-prefix lang) "/search.json")]
    (write-file! out (json/generate-string entries))))

(defn- render-sitemap! [config posts langs]
  (when-let [base (not-empty (:base-url config))]
    (let [sections (->> posts (map (juxt :lang :section)) set)
          all-tags (->> posts
                        (mapcat (fn [p] (map #(vector (:lang p) %) (:tags p))))
                        distinct)
          entries  (concat
                    ;; Language indexes
                    (map (fn [l] {:loc (str base (lang-prefix l) "/")}) langs)
                    ;; All posts
                    (map (fn [p] {:loc     (str base (:url p))
                                  :lastmod (:date p)}) posts)
                    ;; Section indexes
                    (->> sections
                         (filter (fn [[_ s]] s))
                         (map (fn [[l s]] {:loc (str base (lang-prefix l) "/" s "/")})))
                    ;; Tag indexes
                    (map (fn [l] {:loc (str base (lang-prefix l) "/tags/")}) langs)
                    ;; Individual tag pages
                    (map (fn [[l t]] {:loc (str base (lang-prefix l) "/tags/" t "/")}) all-tags))
          xml      (render-template "sitemap.xml" {:entries entries})]
      (write-file! (str *output-dir* "/sitemap.xml") xml))))

(defn- copy-static! []
  (let [static-dir (str *input-dir* "/static")]
    (when (fs/exists? static-dir)
      (fs/copy-tree static-dir *output-dir* {:replace-existing true}))))

(defn- copy-assets!
  "Copy non-org files from content root to output, preserving paths.
   Skips static/, public/, templates/, .git/, config files, and hidden files."
  []
  (let [root *input-dir*]
    (doseq [file (->> (distinct (concat (fs/glob root "*") (fs/glob root "**/*")))
                      (filter fs/regular-file?)
                      (remove #(str/ends-with? (str %) ".org"))
                      (remove #(str/starts-with? (str (fs/file-name %)) "."))
                      (remove #(in-ignored-dir? root %)))]
      (let [rel  (str (fs/relativize root file))
            dest (str *output-dir* "/" rel)]
        (when-not (contains? ignored-root-files rel)
          (fs/create-dirs (fs/parent dest))
          (fs/copy file dest {:replace-existing true}))))))

(defn- detect-monolingual?
  "Return true when there is no indication of multiple languages.
   Uses pre-scanned metadata to avoid re-reading files."
  [config scanned-files]
  (and (<= (count (:languages config)) 1)
       (not (map? (:menu config)))
       (<= (count (distinct (map :lang scanned-files))) 1)))

(defn- resolve-languages
  "Decide which languages to render. Explicit config wins and is
   unioned with post-inferred languages; otherwise we infer from posts;
   and as a last resort fall back to [\"en\"]."
  [config posts]
  (let [from-cfg   (seq (:languages config))
        from-posts (seq (distinct (map :lang posts)))]
    (cond
      from-cfg   (vec (distinct (concat from-cfg from-posts)))
      from-posts (vec from-posts)
      :else      ["en"])))

(defn- group-posts-by-tag
  "Build a {tag [posts…]} map for a language's posts, each tag's
   posts sorted newest-first."
  [posts]
  (->> posts
       (mapcat (fn [p] (map #(vector % p) (:tags p))))
       (group-by first)
       (reduce-kv (fn [m tag pairs]
                    (assoc m tag (sort-by :date date-desc (map second pairs))))
                  {})))

(defn- render-posts-in-section!
  "Render every post in a section, wiring prev/next links within the
   section. Posts are newest-first, so prev=newer and next=older."
  [config menu section sec-posts]
  (let [sec-vec (vec sec-posts)]
    (dotimes [i (count sec-vec)]
      (render-post! config
                    (sec-vec i)
                    menu
                    (when section (get sec-vec (dec i)))
                    (when section (get sec-vec (inc i)))))))

(defn- render-tags!
  "Render tag pages, per-tag RSS feeds and the tags index for a
   language. No-op if no post has any tag."
  [config lang lang-posts menu]
  (let [tag-groups (group-posts-by-tag lang-posts)]
    (when (seq tag-groups)
      (doseq [[tag tag-posts] tag-groups]
        (render-tag-page! config lang tag tag-posts menu)
        (render-feed! config lang tag-posts
                      (str *output-dir* (lang-prefix lang) "/tags/" tag "/feed.xml")))
      (render-tags-index! config lang
                          (map (fn [[tag ps]] [tag (count ps)]) tag-groups)
                          menu))))

(defn- render-language!
  "Render everything that belongs to a single language: posts,
   main index, section indexes, tag pages, RSS feed, and search index."
  [config lang posts sections langs]
  (let [lang-posts (filter #(= (:lang %) lang) posts)
        menu       (build-menu lang lang-posts sections langs config)]
    (doseq [[section sec-posts] (group-by :section lang-posts)]
      (render-posts-in-section! config menu section sec-posts))
    (render-index! config lang lang-posts (find-index-file lang) menu)
    (doseq [[section sec-posts] (group-by :section lang-posts)
            :when section]
      (render-list! config lang section sec-posts menu))
    (render-tags! config lang lang-posts menu)
    (render-feed! config lang lang-posts)
    (when (:quick-search config)
      (render-search-index! lang lang-posts))))

(defn- render-root-redirect!
  "In multilingual mode, write a minimal /index.html that redirects
   to the first language. No-op in monolingual mode."
  [langs]
  (when-not *monolingual*
    (let [html (render-template "redirect.html"
                                {:target (str "/" (first langs) "/")})]
      (write-file! (str *output-dir* "/index.html") html))))

(defn- copy-all-assets!
  "Copy user-provided files to the output directory. Non-org files
   from the content tree are copied first, then the static/ directory
   is overlaid — static/ is authoritative for site-wide assets
   (favicon, robots.txt, custom CSS…) and wins on conflict."
  []
  (copy-assets!)
  (copy-static!))

(defn build!
  "Build the static site."
  [{:keys [input-dir output-dir skip-dirs] :as overrides}]
  (binding [*input-dir*  (str (fs/canonicalize (or input-dir ".")))
            *output-dir* (str (fs/canonicalize (or output-dir "public")))]
    (let [config (load-config (dissoc overrides :input-dir :output-dir :skip-dirs))
          extra  (into (set skip-dirs) (:skip-dirs config))]
      (binding [*ignored-dirs* (into *ignored-dirs* extra)]
        (let [scanned (map prescan-org-file (list-org-files))
              mono?   (detect-monolingual? config scanned)]
          (binding [*monolingual* mono?]
            (let [live     (remove :draft? scanned)
                  n-pages  (count live)
                  posts    (load-posts scanned)
                  sections (->> posts (map (juxt :lang :section)) set)
                  langs    (resolve-languages config posts)]
              (println (str "Building " n-pages " page" (when (> n-pages 1) "s") "..."))
              (doseq [lang langs]
                (render-language! config lang posts sections langs))
              (render-root-redirect! langs)
              (render-sitemap! config posts langs)
              (copy-all-assets!)
              (println (str "Site built in " *output-dir*
                            " (" n-pages " page" (when (> n-pages 1) "s") ")")))))))))

(defn- source-fingerprint
  "Return a map of path→last-modified-ms for every file under
   input-dir that would contribute to a build. Hidden files, the
   output directory, and any directory in *ignored-dirs* are excluded
   — so --skip-dirs entries do not trigger spurious rebuilds."
  [input-dir output-dir]
  (let [out-abs (str (fs/canonicalize output-dir) "/")]
    (->> (distinct (concat (fs/glob input-dir "**/*") (fs/glob input-dir "*")))
         (filter fs/regular-file?)
         (remove #(str/starts-with? (str (fs/canonicalize %)) out-abs))
         (remove #(str/starts-with? (str (fs/file-name %)) "."))
         (remove #(in-ignored-dir? input-dir %))
         (reduce (fn [m f] (assoc m (str f) (.toMillis (fs/last-modified-time f)))) {}))))

(defn- watch-and-rebuild!
  "Poll for source changes every `interval-ms` and rebuild when detected."
  [opts input-dir output-dir interval-ms]
  (let [fp (atom (source-fingerprint input-dir output-dir))]
    (while true
      (Thread/sleep interval-ms)
      (let [fp' (source-fingerprint input-dir output-dir)]
        (when (not= @fp fp')
          (println "\nChange detected, rebuilding...")
          (try (build! opts)
               (catch Exception e
                 (println (str "Build error: " (.getMessage e)))))
          (reset! fp fp'))))))

(def ^:private content-types
  {"html" "text/html; charset=utf-8"
   "css"  "text/css; charset=utf-8"
   "xml"  "application/xml; charset=utf-8"
   "json" "application/json; charset=utf-8"
   "js"   "text/javascript; charset=utf-8"
   "png"  "image/png"  "jpg"  "image/jpeg" "jpeg" "image/jpeg"
   "gif"  "image/gif"  "svg"  "image/svg+xml" "webp" "image/webp"
   "ico"  "image/x-icon" "pdf" "application/pdf"})

(defn- content-type-for [fpath]
  (or (content-types (last (str/split fpath #"\."))) "application/octet-stream"))

(defn- serve-request [out-dir socket]
  (with-open [s socket]
    (let [out   (.getOutputStream s)
          line  (.readLine (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream s))))
          path  (when line (second (str/split line #"\s+")))
          fpath (str (fs/normalize
                      (str out-dir (if (str/ends-with? (or path "") "/")
                                     (str path "index.html") path))))]
      (if (and (str/starts-with? fpath (str out-dir "/")) (fs/exists? fpath))
        (let [body   (java.nio.file.Files/readAllBytes (java.nio.file.Path/of fpath (into-array String [])))
              header (.getBytes (str "HTTP/1.1 200 OK\r\nContent-Type: " (content-type-for fpath)
                                     "\r\nContent-Length: " (count body) "\r\n\r\n"))]
          (.write out header) (.write out body) (.flush out))
        (do (.write out (.getBytes "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\nNot found"))
            (.flush out))))))

(defn serve!
  "Build, start a simple HTTP server, and watch for file changes.
   Requests are served sequentially on a single daemon thread —
   fine for local preview, not for production traffic."
  [{:keys [port overrides] :or {port default-serve-port}}]
  (let [opts    (or overrides {})
        out-dir (str (fs/absolutize (or (:output-dir opts) "public")))
        in-dir  (str (fs/absolutize (or (:input-dir opts) ".")))
        server  (java.net.ServerSocket. port)]
    (build! opts)
    (println (str "Serving at http://localhost:" port))
    (println "Watching for changes... (Ctrl+C to stop)")
    (doto (Thread. (fn [] (while true (try (serve-request out-dir (.accept server))
                                           (catch Exception _)))))
      (.setDaemon true)
      (.start))
    (watch-and-rebuild! opts in-dir out-dir watch-interval-ms)))

(def ^:private default-config
  "{;; Site title (default: name of the current directory).
 :title     \"My Blog\"

 ;; Base URL for absolute links (RSS, sitemap, etc.). No trailing slash.
 ;; :base-url  \"https://example.com\"

 ;; Copyright notice displayed in the footer.
 :copyright \"© 2026 Author\"

 ;; Languages to generate indexes and feeds for.
 ;; If omitted, orgy infers them from post filenames
 ;; (post.fr.org → \"fr\") or #+language: headers.
 ;; :languages [\"en\" \"fr\"]

 ;; Menu entries: list of slugs or direct links to show in nav.
 ;; Matches root-level pages and section directories. Displayed in this order.
 ;; Can be a vector (same menu for all languages) or a map keyed by language.
 ;; If omitted, all root pages and sections are shown.
 ;; :menu [\"notes\" \"about\" {:url \"https://github.com/me\" :description \"GitHub\"}]
 ;; :menu {:en [\"notes\" \"about\"] :fr [\"notes\" \"a-propos\"]}

 ;; Enable client-side quick search (generates search.json per language).
 ;; :quick-search true

 ;; Show a light/dark theme toggle button in the navigation bar.
 ;; :theme-toggle true

 ;; Optional CSS theme, loaded after Pico 2.
 ;; Can be: a pico-themes name (e.g. \"teletype\"), an https:// URL,
 ;; a file:/// URL, or a path to a local .css file.
 ;; :theme \"teletype\"
 }")

(defn- write-if-absent! [path content label]
  (if (fs/exists? path)
    (println (str "  skip  " label " (already exists)"))
    (do (spit (str path) content)
        (println (str "  wrote " label)))))

(defn- init!
  "Initialize templates/ and config.edn in {input-dir}."
  [input-dir]
  ;; config.edn
  (write-if-absent! (str input-dir "/config.edn") default-config "config.edn")
  ;; templates
  (let [dir (str input-dir "/templates")]
    (fs/create-dirs dir)
    (doseq [[name content] default-templates]
      (write-if-absent! (str dir "/" name) content name)))
  (println (str "Initialized " input-dir "/")))

(def ^:private cli-options
  [["-i" "--input-dir DIR"   "Input directory containing org files"
    :default "."]
   ["-o" "--output-dir DIR"  "Output directory"
    :default "public"]
   ["-c" "--config FILE"     "Path to config.edn (default: input-dir or working dir)"
    :id :config-path]
   ["-C" "--config-write"    "Write a default config.edn in the current directory"]
   ["-s" "--skip-dirs DIRS"  "Comma-separated directories to skip (e.g. drafts,old)"
    :parse-fn #(str/split % #",")]
   ["-t" "--theme VALUE"     "CSS theme: name (pico-themes), https:// URL, file:/// URL, or path to a .css file"]
   ["-h" "--help"]])

(def ^:private usage-text
  "Usage: orgy [options] [command]

Commands:
  (none)           Build the static site
  serve [port]     Build and serve locally (default port: 1888)
  init             Export templates and config for customization
  clean            Remove the output directory
  help             Show this help

Options:")

(defn- ->overrides
  "Keep only the keys that represent user-provided overrides for load-config."
  [opts]
  (-> opts
      (select-keys [:input-dir :output-dir :config-path :theme :skip-dirs])
      (as-> m (into {} (remove #(nil? (val %)) m)))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)
        {:keys [help config-write input-dir output-dir]} options
        overrides (->overrides options)
        cmd       (first arguments)]
    (cond
      (seq errors)             (do (doseq [e errors] (println e)) (System/exit 1))
      (or help (= cmd "help")) (println (str usage-text "\n" summary))
      config-write             (write-if-absent! (str input-dir "/config.edn") default-config "config.edn")
      (= cmd "serve")          (serve! {:port (or (some-> (second arguments) parse-long) default-serve-port)
                                        :overrides overrides})
      (= cmd "init")           (init! input-dir)
      (= cmd "clean")          (do (fs/delete-tree output-dir)
                                   (println (str "Cleaned " output-dir "/")))
      :else                    (build! overrides))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
