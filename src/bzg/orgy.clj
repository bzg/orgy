;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0

(ns bzg.orgy
  "Static blog engine: org files → HTML via organ + selmer."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [selmer.parser :as selmer]
            [bzg.organ :as organ]))

;; ---------------------------------------------------------------------------
;; Paths
;; ---------------------------------------------------------------------------

(def ^:dynamic *input-dir* ".")
(def ^:dynamic *output-dir* "public")

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(defn- resolve-css-theme
  "Resolve a --theme value to {:link url} or {:inline css-content}.
  Priority: https URL → file:/// URL → .css file path → pico-themes name."
  [value]
  (when value
    (cond
      ;; 1. https:// URL → external <link>
      (str/starts-with? value "https://")
      {:link value}

      ;; 2. file:/// URL → inline local CSS
      (str/starts-with? value "file:///")
      (let [path (subs value 7)]
        (if (fs/exists? path)
          {:inline (slurp path)}
          (throw (ex-info (str "CSS file not found: " path) {}))))

      ;; 3. Path ending in .css → inline if file exists, ignore if not
      (str/ends-with? value ".css")
      (when (fs/exists? value)
        {:inline (slurp value)})

      ;; 4. Simple name without spaces → pico-themes CDN
      (not (str/includes? value " "))
      {:link (str "https://cdn.jsdelivr.net/gh/bzg/pico-themes@latest/"
                  value ".css")}

      :else
      (throw (ex-info (str "Invalid --theme value: " value) {})))))

(defn- load-config [overrides]
  (let [cfg-path (or (:config-path overrides)
                     (let [p (str *input-dir* "/config.edn")]
                       (when (fs/exists? p) (str p))))
        cfg   (when cfg-path
                (-> cfg-path slurp clojure.edn/read-string))
        title (or (:title cfg) (-> *input-dir* fs/absolutize fs/file-name str))]
    (let [theme-val (or (:theme overrides) (:theme cfg))]
      (merge {:title     title
              :base-url  (str/replace (or (:base-url cfg) "") #"/$" "")
              :copyright (or (:copyright cfg) "")
              :resolved-theme (resolve-css-theme theme-val)
              :languages (or (:languages cfg) ["en"])
              :menu      (:menu cfg)}
             (dissoc overrides :config-path :theme)))))

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
  {% if description %}<meta name=\"description\" content=\"{{description}}\">{% endif %}
  <meta property=\"og:title\" content=\"{% if title %}{{title}}{% else %}{{site.title}}{% endif %}\">
  {% if description %}<meta property=\"og:description\" content=\"{{description}}\">{% endif %}
  <meta property=\"og:type\" content=\"{% if date %}article{% else %}website{% endif %}\">
  {% if canonical %}<meta property=\"og:url\" content=\"{{canonical}}\">{% endif %}
  {% if canonical %}<link rel=\"canonical\" href=\"{{canonical}}\">{% endif %}
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
  {% if theme-link %}<link rel=\"stylesheet\" href=\"{{theme-link}}\">{% endif %}
  {% if theme-inline %}<style>{{theme-inline|safe}}</style>{% endif %}
  <link rel=\"alternate\" type=\"application/rss+xml\" title=\"{{site.title}} ({{lang}})\" href=\"/{{lang}}/feed.xml\">
  <link rel=\"sitemap\" type=\"application/xml\" href=\"/sitemap.xml\">
  <style>
    .container{max-width:1080px}
    header.container{padding-bottom:1rem;margin-bottom:1.2rem}
    body>footer{text-align:center}
    iframe{width:100%;aspect-ratio:4/3;display:block;margin:1em auto;border:none}
    article img{display:block;margin:1em auto;max-width:90%}
    time{padding:.125em .375em;border-radius:4px;background:color-mix(in srgb,var(--pico-background-color) 85%,var(--pico-color));color:color-mix(in srgb,var(--pico-color) 65%,var(--pico-background-color))}
    article>header{display:flex;flex-wrap:wrap;align-items:baseline;gap:.5em}
    article>header h1{flex:1;margin:0}
    article>header .tags{margin-left:auto;display:flex;gap:.35em}
    article>header .tags a{text-decoration:none}
    article>header .tags mark{font-size:.85em}
    .tags-clear{text-decoration:none;opacity:.4;font-size:1.1em}
    .tags-clear:hover{opacity:1}
    article>footer nav{display:flex;justify-content:space-between;margin-top:2em;padding-top:1em;border-top:1px solid var(--pico-muted-border-color)}
    .search-wrap{position:relative}
    nav h1{margin:0;font-size:1.5rem;font-family:inherit}
    article h2{font-size:1.3rem}
    article h3{font-size:1.15rem}
    article h4{font-size:1.05rem}
    article h5,article h6{font-size:1rem}
    #search-input{margin-bottom:0;padding:.25rem .5rem;height:auto;font-size:.875rem;width:10rem;background-image:none}
    .theme-toggle{background:none;border:none;padding:.25rem;line-height:1;cursor:pointer;color:var(--pico-color)}
    .search-results{position:absolute;right:0;top:100%;background:var(--pico-background-color);border:1px solid var(--pico-muted-border-color);border-radius:4px;max-height:60vh;overflow-y:auto;width:320px;z-index:10;display:none;margin-top:.25em;padding:0;list-style:none}
    .search-results li a{display:block;padding:.5em .75em;text-decoration:none}
    .search-results li a:hover{background:var(--pico-primary-focus)}
    .search-results li time{font-size:.8em;opacity:.6;margin-left:.5em}
  </style>
  {% if has-code %}<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11/build/styles/default.min.css\">{% endif %}
</head>
<body>
  <header class=\"container\">
    <nav>
      <ul>
        <li><h1><a href=\"/{{lang}}/\">{{site.title}}</a></h1></li>
      </ul>
      <ul>
        {% for item in menu %}
        <li><a href=\"{{item.url}}\">{{item.name}}</a></li>
        {% endfor %}
        <li class=\"search-wrap\">
          <input type=\"search\" id=\"search-input\" placeholder=\"\" autocomplete=\"off\">
          <ul class=\"search-results\" id=\"search-results\"></ul>
        </li>
        <li><a href=\"#\" id=\"theme-toggle\" class=\"theme-toggle\" aria-label=\"Toggle theme\"><svg id=\"theme-icon\" xmlns=\"http://www.w3.org/2000/svg\" width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"5\"/><line x1=\"12\" y1=\"1\" x2=\"12\" y2=\"3\"/><line x1=\"12\" y1=\"21\" x2=\"12\" y2=\"23\"/><line x1=\"4.22\" y1=\"4.22\" x2=\"5.64\" y2=\"5.64\"/><line x1=\"18.36\" y1=\"18.36\" x2=\"19.78\" y2=\"19.78\"/><line x1=\"1\" y1=\"12\" x2=\"3\" y2=\"12\"/><line x1=\"21\" y1=\"12\" x2=\"23\" y2=\"12\"/><line x1=\"4.22\" y1=\"19.78\" x2=\"5.64\" y2=\"18.36\"/><line x1=\"18.36\" y1=\"5.64\" x2=\"19.78\" y2=\"4.22\"/></svg></a></li>
      </ul>
    </nav>
  </header>
  <main class=\"container\">
    {{body|safe}}
  </main>
  {% if site.copyright|not-empty %}
  <footer class=\"container\">
    <p>{{site.copyright}}</p>
  </footer>
  {% endif %}
  <script>
  (function(){
    var input=document.getElementById('search-input'),
        list=document.getElementById('search-results'),
        idx=null,lang='{{lang}}';
    function esc(s){var d=document.createElement('div');d.textContent=s;return d.innerHTML;}
    function load(cb){
      if(idx)return cb();
      fetch('/'+lang+'/search.json').then(function(r){return r.json()})
        .then(function(d){idx=d;cb()});
    }
    function render(q){
      if(!q){list.style.display='none';return;}
      var lq=q.toLowerCase(),
          hits=idx.filter(function(p){return (p.t&&p.t.toLowerCase().indexOf(lq)>=0)||(p.b&&p.b.toLowerCase().indexOf(lq)>=0)})
               .slice(0,15);
      if(!hits.length){list.style.display='none';return;}
      list.innerHTML=hits.map(function(p){
        return '<li><a href=\"'+esc(p.u)+'\">'+esc(p.t||'')
          +(p.d?'<time>'+esc(p.d)+'</time>':'')+'</a></li>';
      }).join('');
      list.style.display='block';
    }
    input.addEventListener('input',function(){
      load(function(){render(input.value.trim())});
    });
    document.addEventListener('click',function(e){
      if(!e.target.closest('.search-wrap'))list.style.display='none';
    });
  })();
  (function(){
    var sun='<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"5\"/><line x1=\"12\" y1=\"1\" x2=\"12\" y2=\"3\"/><line x1=\"12\" y1=\"21\" x2=\"12\" y2=\"23\"/><line x1=\"4.22\" y1=\"4.22\" x2=\"5.64\" y2=\"5.64\"/><line x1=\"18.36\" y1=\"18.36\" x2=\"19.78\" y2=\"19.78\"/><line x1=\"1\" y1=\"12\" x2=\"3\" y2=\"12\"/><line x1=\"21\" y1=\"12\" x2=\"23\" y2=\"12\"/><line x1=\"4.22\" y1=\"19.78\" x2=\"5.64\" y2=\"18.36\"/><line x1=\"18.36\" y1=\"5.64\" x2=\"19.78\" y2=\"4.22\"/></svg>',
        moon='<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z\"/></svg>',
        btn=document.getElementById('theme-toggle'),
        root=document.documentElement,
        stored=localStorage.getItem('theme'),
        theme=stored||(matchMedia('(prefers-color-scheme:dark)').matches?'dark':'light');
    function apply(t){root.setAttribute('data-theme',t);btn.innerHTML=(t==='dark'?sun:moon);localStorage.setItem('theme',t);}
    apply(theme);
    btn.addEventListener('click',function(e){e.preventDefault();apply(root.getAttribute('data-theme')==='dark'?'light':'dark');});
  })();
  </script>
  {% if has-code %}<script src=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11/build/highlight.min.js\"></script>
  {% for hl-lang in hl-langs %}<script src=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11/build/languages/{{hl-lang}}.min.js\"></script>
  {% endfor %}<script>hljs.highlightAll();</script>{% endif %}
</body>
</html>"

   "post.html"
   "<article>
  {% if title %}<header><h1>{{title}}</h1>
  {% if author %}<span class=\"author\">{{author}}</span>{% endif %}
  {% if date %}<time datetime=\"{{date}}\">{{date}}</time>{% endif %}
  {% if tags|not-empty %}
  <nav class=\"tags\">
    {% for tag in tags %}
    <a href=\"/{{lang}}/tags/{{tag}}/\"><mark>{{tag}}</mark></a>
    {% endfor %}
    <a href=\"/{{lang}}/tags/\" class=\"tags-clear\" title=\"All tags\">&times;</a>
  </nav>
  {% endif %}
  </header>{% endif %}
  <section>
    {{content|safe}}
  </section>
  {% if has-nav %}
  <footer>
    <nav>
      {% if next %}<a href=\"{{next.url}}\">&larr; {{next.title}}</a>{% endif %}
      {% if prev %}<a href=\"{{prev.url}}\" style=\"margin-left:auto\">{{prev.title}} &rarr;</a>{% endif %}
    </nav>
  </footer>
  {% endif %}
</article>"

   "list.html"
   "<section>
  <h1>{{title|capitalize}}</h1>
  <ul>
    {% for post in posts %}
    <li>
      <a href=\"{{post.url}}\">{{post.title}}</a>
      {% if post.date %}<time datetime=\"{{post.date}}\">{{post.date}}</time>{% endif %}
    </li>
    {% endfor %}
  </ul>
</section>"

   "tag.html"
   "<section>
  <h1>{{tag|capitalize}}</h1>
  <ul>
    {% for post in posts %}
    <li>
      <a href=\"{{post.url}}\">{{post.title}}</a>
      {% if post.date %}<time datetime=\"{{post.date}}\">{{post.date}}</time>{% endif %}
    </li>
    {% endfor %}
  </ul>
</section>"

   "tags-index.html"
   "<section>
  <h1>Tags</h1>
  <ul>
    {% for entry in tags %}
    <li><a href=\"/{{lang}}/tags/{{entry.tag}}/\">{{entry.tag}}</a> ({{entry.count}})</li>
    {% endfor %}
  </ul>
</section>"

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
      {% if post.date %}<pubDate>{{post.rfc822-date}}</pubDate>{% endif %}
      <description>{{post.summary}}</description>
    </item>
    {% endfor %}
  </channel>
</rss>"})

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

(defn- collect-code-langs
  "Collect highlight.js language names from src-blocks in the AST.
   Returns {:has-code bool :hl-langs [extra langs to load]}."
  [ast]
  (letfn [(walk [acc node]
            (let [acc (if (= :src-block (:type node))
                        (if-let [l (:language node)]
                          (conj acc (get hljs-lang-map l l))
                          acc)
                        acc)]
              (reduce walk (reduce walk acc (:children node)) (:items node))))]
    (let [all (reduce walk #{} (:children ast))]
      {:has-code (boolean (seq all))
       :hl-langs (vec (remove hljs-builtin all))})))

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

(defn- resolve-file-url
  "For file: links, strip the prefix and rewrite static/ paths."
  [node]
  (if (= :file (:link-type node))
    (str/replace-first (:target node) #"^static/" "/")
    (:url node)))

(def ^:private image-ext-re #"\.(?:png|jpg|jpeg|gif|svg|webp)$")

(def ^:private ignored-dirs
  "Directories to skip when scanning for content."
  #{"static" "public" "templates" ".git"})

(def ^:private ignored-root-files
  "Root files to skip when copying assets."
  #{"config.edn" "bb.edn"})

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
;; Inline node rendering (organ 0.3+ returns parsed inline node vectors)
;; ---------------------------------------------------------------------------

(defn- render-inline
  "Render a vector of organ inline nodes to HTML."
  [nodes]
  (when (seq nodes)
    (str/join
     (map (fn [node]
            (case (:type node)
              :text      (escape-html (:value node))
              :bold      (str "<strong>" (render-inline (:children node)) "</strong>")
              :italic    (str "<em>" (render-inline (:children node)) "</em>")
              :underline (str "<u>" (render-inline (:children node)) "</u>")
              :strike    (str "<del>" (render-inline (:children node)) "</del>")
              :code      (str "<code>" (escape-html (:value node)) "</code>")
              :verbatim  (str "<code>" (escape-html (:value node)) "</code>")
              :link      (let [url (resolve-file-url node)]
                           (if (re-find image-ext-re url)
                             (str "<img src=\"" (escape-html url) "\" alt=\""
                                  (escape-html (or (organ/inline-text (:children node)) "")) "\">")
                             (if (seq (:children node))
                               (str "<a href=\"" (escape-html url) "\">" (render-inline (:children node)) "</a>")
                               (str "<a href=\"" (escape-html url) "\">" (escape-html url) "</a>"))))
              :footnote-ref (let [lbl (escape-html (:label node))]
                              (str "<sup><a href=\"#fn-" lbl
                                   "\" id=\"fnref-" lbl "\">"
                                   lbl "</a></sup>"))
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

(defn- html-attrs
  "Build an HTML attribute string from an affiliated :attr :html map."
  [node]
  (when-let [attrs (get-in node [:affiliated :attr :html])]
    (str/join (map (fn [[k v]] (str " " (name k) "=\"" (escape-html v) "\"")) attrs))))

(defn- render-node [node]
  (case (:type node)
    :paragraph
    (let [c     (:content node)
          attrs (get-in node [:affiliated :attr :html])]
      (if-let [text (organ/inline-text c)]
        (if (re-matches #"^\s*#\+\w+(?:\[\])?\s*:.*" text)
          "" ;; skip org keywords that organ didn't parse as metadata
          ;; Single image link with ATTR_HTML: apply attrs to <img>
          (if (and attrs
                   (= 1 (count c))
                   (= :link (:type (first c)))
                   (re-find image-ext-re (or (:target (first c)) (:url (first c)))))
            (let [link  (first c)
                  url   (resolve-file-url link)
                  alt   (or (:alt attrs)
                            (organ/inline-text (:children link))
                            "")
                  extra (dissoc attrs :alt)]
              (str "<img src=\"" (escape-html url) "\" alt=\"" (escape-html alt) "\""
                   (str/join (map (fn [[k v]] (str " " (name k) "=\"" (escape-html v) "\"")) extra))
                   ">"))
            (str "<p" (or (html-attrs node) "") ">" (render-inline c) "</p>")))
        ""))

    :html-line
    (:content node)

    :section
    (let [level (inc (:level node))]
      (if (<= level 6)
        (let [tag (str "h" level)]
          (str "<" tag ">" (render-inline (:title node)) "</" tag ">\n"
               (render-children (:children node))))
        (str "<p><strong>" (render-inline (:title node)) "</strong></p>\n"
             (render-children (:children node)))))

    :src-block
    (let [lang (some-> (:language node) (as-> l (get hljs-lang-map l l)))]
      (str "<pre><code"
           (when lang (str " class=\"language-" lang "\""))
           ">"
           (escape-html (:content node))
           "</code></pre>"))

    :quote-block
    (str "<blockquote>" (render-children (:children node)) "</blockquote>")

    :block
    (if (and (= :export (:block-type node)) (= "html" (:args node)))
      (:content node)
      (str "<pre>" (escape-html (:content node)) "</pre>"))

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
    (str "<pre>" (escape-html (:content node)) "</pre>")

    :comment ""

    :footnote-def
    (let [lbl (escape-html (:label node))]
      (str "<div class=\"footnote\" id=\"fn-" lbl "\">"
           "<sup>" lbl "</sup> "
           (render-inline (:content node))
           " <a href=\"#fnref-" lbl "\">↩</a></div>"))

    :drawer ""

    ;; fallback
    (if (:children node)
      (render-children (:children node))
      "")))

(defn- ast->html
  "Render an organ AST document to HTML body content."
  [ast]
  (render-children (:children ast)))

(defn- node->text
  "Extract plain text from an AST node, recursing into children."
  [node]
  (case (:type node)
    :paragraph  (let [text (organ/inline-text (:content node))]
                  (if (and text (re-matches #"^\s*#\+\w+(?:\[\])?\s*:.*" text))
                    ""
                    (or text "")))
    :section    (str (organ/inline-text (:title node)) " "
                     (str/join " " (map node->text (:children node))))
    :list       (str/join " " (map node->text (:items node)))
    :list-item  (str (organ/inline-text (:content node)) " "
                     (str/join " " (map node->text (:children node))))
    :quote-block (str/join " " (map node->text (:children node)))
    :src-block  (:content node)
    :block      (if (= :export (:block-type node)) "" (:content node))
    :fixed-width (:content node)
    :table      (str/join " " (mapcat (fn [row] (map organ/inline-text row)) (:rows node)))
    :footnote-def (organ/inline-text (:content node))
    (:comment :drawer :html-line) ""
    (if (:children node)
      (str/join " " (map node->text (:children node)))
      "")))

(defn- ast->text
  "Extract plain text from an organ AST document."
  [ast]
  (-> (str/join " " (map node->text (:children ast)))
      (str/replace #"\s+" " ")
      str/trim))

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
  "Determine content section from path relative to content root."
  [path]
  (let [parts (-> (fs/relativize *input-dir* path) str (str/split #"/"))]
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
    {:title   (:title meta)
     :draft   (= "true" (some-> (:draft meta) str/trim str/lower-case))
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

(defn- in-ignored-dir? [root path]
  (let [rel (str (fs/relativize root path))]
    (some #(str/starts-with? rel (str % "/")) ignored-dirs)))

(defn- load-posts
  "Load all org files from content root, excluding index files and ignored dirs."
  []
  (let [root *input-dir*]
    (->> (concat (fs/glob root "*.org")
                (fs/glob root "**/*.org"))
         (remove index-file?)
         (remove #(str/starts-with? (str (fs/file-name %)) "."))
         (remove #(in-ignored-dir? root %))
         (map load-post)
         (remove :draft)
         (sort-by :date (fn [a b] (compare (or b "") (or a "")))))))

;; ---------------------------------------------------------------------------
;; Site generation
;; ---------------------------------------------------------------------------

(defn- build-menu
  "Build nav menu for a given language.
   If config has :menu, only root pages/sections whose slug matches an
   entry in :menu are included (in :menu order). Otherwise all are shown."
  [lang posts sections langs config]
  (let [menu-filter (:menu config)
        ;; Root-level pages (no section, not index files) for this language
        root-pages (->> posts
                        (filter #(and (= (:lang %) lang) (nil? (:section %))))
                        (map (fn [p] {:name (or (:title p) (:slug p)) :url (:url p) :slug (:slug p)})))
        ;; Section links
        sec-links  (->> sections
                        (filter (fn [[l s]] (and (= l lang) s)))
                        (map (fn [[_ s]] {:name (str/capitalize s)
                                          :url  (str "/" lang "/" s "/")
                                          :slug s})))
        ;; Filter and order by :menu config, or show all
        ;; Entries can be slugs (strings) or direct links ({:name "X" :url "Y"})
        page-links (if menu-filter
                     (let [by-slug (into {} (map (fn [p] [(:slug p) p])
                                                 (concat root-pages sec-links)))]
                       (->> menu-filter
                            (keep #(if (map? %)
                                     (select-keys % [:name :url])
                                     (get by-slug %)))
                            vec))
                     (vec (concat root-pages (sort-by :name sec-links))))
        ;; Strip :slug before returning
        page-links (mapv #(dissoc % :slug) page-links)
        ;; Tags link
        tags-link  [{:name "Tags" :url (str "/" lang "/tags/")}]
        ;; Other language links
        lang-links (->> langs
                        (remove #(= % lang))
                        (map (fn [l] {:name (str/upper-case l) :url (str "/" l "/")})))]
    (vec (concat page-links tags-link lang-links))))

(defn- site-context [config lang]
  (let [{:keys [link inline]} (:resolved-theme config)]
    {:site         config
     :lang         lang
     :theme-link   link
     :theme-inline inline}))

(defn- write-file! [path content]
  (let [parent (fs/parent path)]
    (when parent (fs/create-dirs parent)))
  (spit (str path) content))

(defn- post-description [post]
  (truncate (ast->text (:ast post)) 160))

(defn- canonical-url [config url]
  (let [base (:base-url config)]
    (when (seq base) (str base url))))

(defn- render-post! [config post menu prev-post next-post]
  (let [lang      (:lang post)
        code-info (collect-code-langs (:ast post))
        title     (if (:section post)
                    (:title post)
                    (str/capitalize (or (:title post) "")))
        ctx       (merge (site-context config lang)
                         code-info
                         {:menu        menu
                          :title       title
                          :date        (:date post)
                          :author      (:author post)
                          :tags        (:tags post)
                          :description (post-description post)
                          :canonical   (canonical-url config (:url post))
                          :has-nav     (boolean (or prev-post next-post))
                          :prev        (when prev-post (select-keys prev-post [:title :url]))
                          :next        (when next-post (select-keys next-post [:title :url]))
                          :content     (ast->html (:ast post))})
        html    (render-page "post.html" ctx)
        out     (str *output-dir* (:url post) "index.html")]
    (write-file! out html)))

(defn- render-list! [config lang section posts menu]
  (let [title (or section "Posts")
        ctx   (merge (site-context config lang)
                     {:menu  menu
                      :title title
                      :posts (map #(select-keys % [:title :date :url]) posts)})
        html  (render-page "list.html" ctx)
        out   (if section
                (str *output-dir* "/" lang "/" section "/index.html")
                (str *output-dir* "/" lang "/index.html"))]
    (write-file! out html)))

(defn- find-index-file
  "Find an index org file for a given language.
   Looks for _index.{lang}.org, index.{lang}.org, _index.org, index.org."
  [lang]
  (let [root *input-dir*]
    (some #(when (fs/exists? %) (str %))
          [(str root "/_index." lang ".org")
           (str root "/index." lang ".org")
           (str root "/_index.org")
           (str root "/index.org")])))

(defn- render-index! [config lang posts index-file menu]
  (let [lang-url (str "/" lang "/")
        ctx (merge (site-context config lang)
                   {:menu menu :canonical (canonical-url config lang-url)})]
    (if index-file
      (let [org-text (slurp index-file)
            ast      (organ/parse-org org-text)
            meta     (:meta ast)
            ctx      (merge ctx
                            (collect-code-langs ast)
                            {:title       (:title meta)
                             :description (truncate (ast->text ast) 160)
                             :content     (ast->html ast)})
            html     (render-page "post.html" ctx)]
        (write-file! (str *output-dir* "/" lang "/index.html") html))
      (let [lang-posts (->> posts
                            (filter #(= (:lang %) lang))
                            (take 10))
            ctx        (merge ctx
                              {:title (:title config)
                               :posts (map #(select-keys % [:title :date :url]) lang-posts)})
            html       (render-page "list.html" ctx)]
        (write-file! (str *output-dir* "/" lang "/index.html") html)))))

(defn- render-tag-page! [config lang tag posts menu]
  (let [ctx  (merge (site-context config lang)
                    {:menu  menu
                     :tag   tag
                     :posts (map #(select-keys % [:title :date :url]) posts)})
        html (render-page "tag.html" ctx)
        out  (str *output-dir* "/" lang "/tags/" tag "/index.html")]
    (write-file! out html)))

(defn- render-tags-index! [config lang tag-counts menu]
  (let [ctx  (merge (site-context config lang)
                    {:menu menu
                     :tags (map (fn [[tag count]] {:tag tag :count count})
                                (sort-by first tag-counts))})
        html (render-page "tags-index.html" ctx)
        out  (str *output-dir* "/" lang "/tags/index.html")]
    (write-file! out html)))


(defn- render-feed! [config lang posts]
  (let [ctx {:site  config
             :lang  lang
             :posts (->> posts
                         (take 20)
                         (map (fn [p]
                                {:title      (:title p)
                                 :url        (:url p)
                                 :date       (:date p)
                                 :rfc822-date (iso->rfc822 (:date p))
                                 :summary    (truncate (ast->text (:ast p)) 200)})))}
        xml (render-template "feed.xml" ctx)
        out (str *output-dir* "/" lang "/feed.xml")]
    (write-file! out xml)))

(defn- render-search-index! [lang posts]
  (let [entries (map (fn [p]
                       {:t (:title p) :u (:url p) :d (:date p)
                        :b (truncate (ast->text (:ast p)) 500)})
                     posts)
        out     (str *output-dir* "/" lang "/search.json")]
    (write-file! out (json/generate-string entries))))

(defn- render-sitemap! [config posts langs]
  (let [base (:base-url config)]
    (when (seq base)
      (let [entries (concat
                     ;; Language indexes
                     (map (fn [l] {:url (str base "/" l "/")}) langs)
                     ;; All posts
                     (map (fn [p] {:url (str base (:url p)) :date (:date p)}) posts)
                     ;; Tag indexes
                     (map (fn [l] {:url (str base "/" l "/tags/")}) langs))
            xml (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
                     "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
                     (str/join "\n"
                               (map (fn [{:keys [url date]}]
                                      (str "  <url>\n    <loc>" url "</loc>"
                                           (when date (str "\n    <lastmod>" date "</lastmod>"))
                                           "\n  </url>"))
                                    entries))
                     "\n</urlset>")]
        (write-file! (str *output-dir* "/sitemap.xml") xml)))))

(defn- copy-static! []
  (let [static-dir (str *input-dir* "/static")]
    (when (fs/exists? static-dir)
      (fs/copy-tree static-dir *output-dir* {:replace-existing true}))))

(defn- copy-assets!
  "Copy non-org files from content root to output, preserving paths.
   Skips static/, public/, templates/, .git/, config files, and hidden files."
  []
  (let [root *input-dir*]
    (doseq [file (->> (concat (fs/glob root "*") (fs/glob root "**/*"))
                      (filter fs/regular-file?)
                      (remove #(str/ends-with? (str %) ".org"))
                      (remove #(str/starts-with? (str (fs/file-name %)) "."))
                      (remove #(in-ignored-dir? root %)))]
      (let [rel  (str (fs/relativize root file))
            dest (str *output-dir* "/" rel)]
        (when-not (contains? ignored-root-files rel)
          (fs/create-dirs (fs/parent dest))
          (fs/copy file dest {:replace-existing true}))))))

(defn build!
  "Build the static site."
  [{:keys [input-dir output-dir] :as overrides}]
  (when-not input-dir
    (throw (ex-info "Input directory is required (-i/--input-dir)" {})))
  (binding [*input-dir*  (str (fs/absolutize input-dir))
            *output-dir* (str (fs/absolutize (or output-dir "public")))]
    (let [config   (load-config (dissoc overrides :input-dir :output-dir))
          posts    (load-posts)
          sections (->> posts (map (juxt :lang :section)) set)
          langs    (distinct (concat (:languages config) (map :lang posts)))]
      (println (str "Building " (count posts) " posts..."))

      ;; Per-language indexes and posts
      (doseq [lang langs]
        (let [lang-posts (filter #(= (:lang %) lang) posts)
              menu       (build-menu lang lang-posts sections langs config)]

          ;; Render posts for this language (with prev/next links)
          ;; Posts are sorted newest-first: prev = newer, next = older
          (let [posts-vec (vec lang-posts)]
            (doseq [i (range (count posts-vec))]
              (let [post (posts-vec i)]
                (render-post! config post menu
                              (when (:section post) (get posts-vec (dec i)))
                              (when (:section post) (get posts-vec (inc i)))))))

          ;; Main index (from index file if present, else 10 latest posts)
          (render-index! config lang posts (find-index-file lang) menu)

          ;; Section indexes
          (doseq [[section sec-posts] (group-by :section lang-posts)
                  :when section]
            (render-list! config lang section sec-posts menu))

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
          (render-feed! config lang lang-posts)

          ;; Search index
          (render-search-index! lang lang-posts)))

      ;; Root index: redirect to first language
      (write-file! (str *output-dir* "/index.html")
                   (str "<!DOCTYPE html><html><head>"
                        "<meta http-equiv=\"refresh\" content=\"0;url=/"
                        (first langs) "/\">"
                        "</head><body></body></html>"))

      ;; Sitemap
      (render-sitemap! config posts langs)

      ;; Copy static assets and non-org files from content tree
      (copy-static!)
      (copy-assets!)

      (println (str "Site built in " *output-dir* " (" (count posts) " posts)")))))

(defn serve!
  "Build and start a simple HTTP server."
  [{:keys [port overrides] :or {port 1888}}]
  (let [opts    (or overrides {})
        out-dir (str (fs/absolutize (or (:output-dir opts) "public")))]
    (build! opts)
    (println (str "Serving at http://localhost:" port))
    (let [server (java.net.ServerSocket. port)]
      (loop []
        (try
          (with-open [socket (.accept server)]
            (let [out   (.getOutputStream socket)
                  in    (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream socket)))
                  line  (.readLine in)
                  path  (when line (second (str/split line #"\s+")))
                  fpath (str out-dir (if (str/ends-with? (or path "") "/")
                                      (str path "index.html")
                                      path))]
              (if (fs/exists? fpath)
                (let [ctype (cond
                              (str/ends-with? fpath ".html") "text/html; charset=utf-8"
                              (str/ends-with? fpath ".css")  "text/css; charset=utf-8"
                              (str/ends-with? fpath ".xml")  "application/xml; charset=utf-8"
                              (str/ends-with? fpath ".json") "application/json; charset=utf-8"
                              (str/ends-with? fpath ".js")   "text/javascript; charset=utf-8"
                              (str/ends-with? fpath ".png")  "image/png"
                              (str/ends-with? fpath ".jpg")  "image/jpeg"
                              (str/ends-with? fpath ".jpeg") "image/jpeg"
                              (str/ends-with? fpath ".gif")  "image/gif"
                              (str/ends-with? fpath ".svg")  "image/svg+xml"
                              (str/ends-with? fpath ".webp") "image/webp"
                              (str/ends-with? fpath ".ico")  "image/x-icon"
                              (str/ends-with? fpath ".pdf")  "application/pdf"
                              :else "application/octet-stream")
                      body   (java.nio.file.Files/readAllBytes (java.nio.file.Path/of fpath (into-array String [])))
                      header (.getBytes (str "HTTP/1.1 200 OK\r\nContent-Type: " ctype "\r\nContent-Length: " (count body) "\r\n\r\n"))]
                  (.write out header)
                  (.write out body)
                  (.flush out))
                (let [resp "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\nNot found"]
                  (.write out (.getBytes resp))
                  (.flush out)))))
          (catch Exception _))
        (recur)))))

(def ^:private default-config
  "{;; Site title (default: name of the current directory).
 :title     \"My Blog\"

 ;; Base URL for absolute links (RSS, sitemap, etc.). No trailing slash.
 ;; :base-url  \"https://example.com\"

 ;; Copyright notice displayed in the footer.
 :copyright \"© 2026 Author\"

 ;; Languages to generate indexes and feeds for (default: [\"en\"]).
 ;; Language is detected from filenames: post.fr.org → \"fr\", post.en.org → \"en\".
 ;; Files without a language suffix default to \"en\".
 :languages [\"en\"]

 ;; Menu entries: list of slugs or direct links to show in nav.
 ;; Matches root-level pages and section directories. Displayed in this order.
 ;; If omitted, all root pages and sections are shown.
 ;; :menu [\"notes\" \"about\" {:name \"GitHub\" :url \"https://github.com/me\"}]

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

(defn- parse-args [args]
  (loop [args args opts {} positional []]
    (if (empty? args)
      (assoc opts :positional positional)
      (let [[a & more] args]
        (case a
          ("-t" "--theme")
          (if-let [theme (first more)]
            (recur (rest more)
                   (assoc opts :theme theme)
                   positional)
            (throw (ex-info "Missing value for -t/--theme" {})))
          ("-i" "--input-dir")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :input-dir v) positional)
            (throw (ex-info "Missing value for -i/--input-dir" {})))
          ("-o" "--output-dir")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :output-dir v) positional)
            (throw (ex-info "Missing value for -o/--output-dir" {})))
          ("-c" "--config")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :config-path v) positional)
            (throw (ex-info "Missing value for -c/--config" {})))
          ("-h" "--help") (assoc opts :help true :positional positional)
          (recur more opts (conj positional a)))))))

(defn -main [& args]
  (let [{:keys [help input-dir output-dir config-path theme positional]} (parse-args args)
        overrides (cond-> {}
                    input-dir   (assoc :input-dir input-dir)
                    output-dir  (assoc :output-dir output-dir)
                    config-path (assoc :config-path config-path)
                    theme       (assoc :theme theme))
        cmd       (first positional)]
    (cond
      (or help (= cmd "help"))
      (println "Usage: orgy -i DIR [options] [command]

Options:
  -i, --input-dir DIR   Input directory containing org files (required)
  -o, --output-dir DIR  Output directory (default: ./public)
  -c, --config FILE     Path to config.edn (default: input-dir or working dir)
  -t, --theme VALUE     CSS theme: name (pico-themes), https:// URL,
                        file:/// URL, or path to a .css file

Commands:
  (none)           Build the static site
  serve [port]     Build and serve locally (default port: 1888)
  init             Export templates and config for customization
  clean            Remove the output directory
  help             Show this help")

      (= cmd "serve")
      (serve! {:port (if-let [p (second positional)]
                       (parse-long p)
                       1888)
               :overrides overrides})

      (= cmd "init")
      (if input-dir
        (init! input-dir)
        (println "Error: -i/--input-dir is required"))

      (= cmd "clean")
      (let [dir (or output-dir "public")]
        (fs/delete-tree dir)
        (println (str "Cleaned " dir "/")))

      :else
      (build! overrides))))
