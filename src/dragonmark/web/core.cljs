(ns ^:figwheel-always dragonmark.web.core
  (:require [goog.dom :as dom]
            [domina :as domina]
            [clojure.string :as s]
            [goog.string :as gstring]
            [goog.object]
            [dragonmark.util.core :as du]
            [clojure.string :as string]))

;; Copied from Enfocus

(defn nodes->coll
  "coverts a nodelist, node into a collection"
  [nl]
  (if (identical? nl js/window)
    [nl]
    (domina/nodes nl)))

(defn html-to-dom [html]
  (let [dfa (nodes->coll (domina/html-to-dom html))
        frag (. js/document (createDocumentFragment))]
    (doseq [df dfa]
      (dom/append frag df))
    frag))

;; done with stuff copied from Enfocus

(defn- better-seq
  "A version of seq that returns an empty list rather than nil"
  [v]
  (if-let [ret (seq v)]
    ret
    (list)))

(defprotocol ToHiccup
  "convert things to Hiccup format"
  (to-hiccup [value]))

(extend-protocol ToHiccup
  js/DocumentFragment
  (to-hiccup [frag] (to-hiccup (-> frag .-firstChild))))

(def ^{:private true
       :dynamic true}  **funcs** nil)

(defn- wrap-funcs
  "This function will do the binding with the function lookup if it's not already bound"
  [to-do]

  (if (and **funcs** (satisfies? IDeref **funcs**))
    (to-do)
    (binding [**funcs** (atom {})]
      (to-do))))

(defn- find-func
  "Looks up a key at returns the associated func if it exists"
  [key]
  (or
    (some-> **funcs** deref (get key key))
    key))

(defn- guid-for
  "Swaps the value for a guid"
  [value]
  (if (and **funcs** (satisfies? IDeref **funcs**))
    (let [guid (du/next-guid)]
      (swap! **funcs** assoc guid value)
      guid)
    value
    ))

(extend-protocol ToHiccup
  js/Element
  (to-hiccup [elem]
    (let [elem-name (.-localName elem)]
      (if (and (= "script" elem-name)
               (.getAttribute elem "magic"))
        (find-func (.getAttribute elem "magic"))            ;; retrieve an unchaged function hiccup node
        (filterv
          identity
          (into
            [(keyword elem-name)
             (to-hiccup (.-attributes elem))
             ]
            (map to-hiccup (filter identity (.-childNodes elem)))
            ))))))

;;; from http://stackoverflow.com/questions/28257750/how-to-convert-html-tag-with-style-to-hiccup-react-problems

(defn string->tokens
  "Takes a string with syles and parses it into properties and value tokens"
  [style]
  (->> (s/split style #";")
       (mapcat #(s/split % #":"))
       (map s/trim)))

(defn tokens->map
  "Takes a seq of tokens with the properties (even) and their values (odd)
   and returns a map of {properties values}"
  [tokens]
  (zipmap (keep-indexed #(if (even? %1) %2) tokens)
          (keep-indexed #(if (odd? %1) %2) tokens)))

(defn style->map
  "Takes an inline style attribute stirng and converts it to a React Style map"
  [style]
  (tokens->map (string->tokens style)))

;;; end-from

(extend-protocol ToHiccup
  js/NamedNodeMap
  (to-hiccup [attrs]
    (let [len (.-length attrs)]
      (if (= 0 len)
        nil
        (let [r (range 0 len)]
          (into
            {}
            (map
              (fn [i]
                (let [at (.item attrs i)
                      key (-> at .-name keyword)
                      value (-> at .-value find-func)]
                  (if (= :style key)
                    [key (style->map value)]
                    [key value])
                  ))
              r))))
      )))

(extend-protocol ToHiccup
  js/Text
  (to-hiccup [text]
    (.-data text)))

(defn inner?
  "Does this command refer to the inner node?"
  [cmd]
  (let [cmd (name cmd)]
    (or (<= 0 (.indexOf cmd "inner"))
        (<= 0 (.indexOf cmd "*")))
    ))

(defn append?
  "Is it an append command?"
  [cmd]
  (let [cmd (name cmd)]
    (gstring/endsWith cmd ">")))

(defn prepend?
  "Is it a prepend command?"
  [cmd]
  (let [cmd (name cmd)]
    (gstring/endsWith cmd "<")))

(defn remove?
  "Is it a remove command for attributes?"
  [cmd]
  (let [cmd (name cmd)]
    (gstring/endsWith cmd "--")))

(defn hiccup?
  "tests to see if it's a Hiccup value"
  [val]
  (and
    (boolean val) ;; not nil
    (vector? val)
    (some-> val first ifn?)))

;; copied from https://github.com/ibdknox/crate

;; From Weavejester's Hiccup: https://github.com/weavejester/hiccup/blob/master/src/hiccup/core.clj#L57
(def ^{:doc "Regular expression that parses a CSS-style id and class from a tag name." :private true}
re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(def xmlns {:xhtml "http://www.w3.org/1999/xhtml"
            :svg "http://www.w3.org/2000/svg"})

(declare elem-factory)

(defn as-content [parent content]
  (doseq[c content]
    (let [child (cond
                  (nil? c) nil
                  (map? c) (throw "Maps cannot be used as content")
                  (string? c) (dom/createTextNode c)
                  (vector? c) (elem-factory c)
                  ;;TODO: there's a bug in clojurescript that prevents seqs from
                  ;; being considered collections
                  (seq? c) (as-content parent c)
                  (.-nodeName c) c
                  (.-get c) (.get c 0)
                  :else (dom/createTextNode (str c)))]
      (when child
        (dom/appendChild parent child)))))

(defn dom-attr
  ([elem attrs]
   (when elem
     (if-not (map? attrs)
       (. elem (getAttribute (name attrs)))
       (do
         (doseq [[k v] attrs]
           (dom-attr elem k v))
         elem))))
  ([elem k v]
   (let [ks (name k)
         v (if (and
                 (gstring/startsWith ks "on")
                 (not (string? v)))
             (guid-for v)
             v
             )]
     (. elem (setAttribute (name k) v))
     elem)))

(defn- normalize-map-attrs [map-attrs]
  (into {} (map (fn [[n v]] (if (true? v) [n (name n)] [n v]))
                (filter (comp boolean second)
                        map-attrs))))

(defn- normalize-element
  "Ensure a tag vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (str tag " is not a valid tag name.")))
  (let [[_ tag id class] (re-matches re-tag (name tag))
        [nsp tag]     (let [[nsp t] (string/split tag #":")
                            ns-xmlns (xmlns (keyword nsp))]
                        (if t
                          [(or ns-xmlns nsp) t]
                          [(:xhtml xmlns) nsp]))
        tag-attrs        (into {}
                               (filter #(not (nil? (second %)))
                                       {:id (or id nil)
                                        :class (if class (string/replace class #"\." " "))}))
        map-attrs        (first content)]
    (if (map? map-attrs)
      [nsp tag (merge tag-attrs (normalize-map-attrs map-attrs)) (next content)]
      [nsp tag tag-attrs content])))


(def create-elem
  (if (.-createElementNS js/document)
    (fn [nsp tag]
      (.createElementNS js/document nsp tag))
    (fn [_ tag]
      (.createElement js/document tag))))

(defn elem-factory [tag-def]
  (if (and
        (vector? tag-def)
        (not (-> tag-def first keyword?))
        (-> tag-def first fn?))
    ;; if there's a function in the beginning of the Hiccup def, then put it in a script tag that'll
    ;; get reified unchanged
    (let [elem (.createElement js/document "script")]
      (.setAttribute elem "magic" (guid-for tag-def))
      elem)
    (let [[nsp tag attrs content] (normalize-element tag-def)
          elem (create-elem nsp tag)]
      (dom-attr elem attrs)
      (as-content elem content)
      elem)))

(defn html [& tags]
  (let [res (map elem-factory tags)]
    (if (second res)
      res
      (first res))))

;; End crate

(defn node-from-hiccup
  "Take a Hiccup data structure and turn it into a DOM node"
  [hic]
  (html hic))

(defn to-node
  "takes a data structure and converts it to a node.
  If it's a vec and not hiccup or a seq, then return
  a seq full of converted nodes"
  [data]
  (cond
    (instance? js/Node data) data
    (nil? data) data
    (string? data) (.createTextNode js/document data)
    (hiccup? data) (node-from-hiccup data)
    (fn? data) data
    :else (throw (str "Can't create node from " (pr-str data)))
    ))

(defn to-doc-frag
  "Takes the data structure and turns it into a DocumentFragment"
  [data]

  (cond
    (satisfies? IDeref data) (to-doc-frag @data)
    (instance? js/DocumentFragment data) data
    (instance? js/Node data) data
    (string? data) (html-to-dom data)
    (hiccup? data) (node-from-hiccup data)
    :else (throw (str "Can't create document fragment from " (pr-str data)))
    ))


(declare alter)

(defn- alter-string
  "Set the thing to a Text element"
  [str cmd node] (alter (to-node str) cmd node))

(defn- alter-vec
  "Deal with both collections and Hiccup literals"
  [vec cmd node]
  (if (hiccup? vec)
    (alter (to-node vec) cmd node)
    (alter (better-seq vec) cmd node)
    ))

(defn- alter-seq
  "it's a seq, so do seq-like things"
  [the-seq cmd node]
  (if (hiccup? the-seq)
    (alter (to-node the-seq) cmd node)

    (let [nodes (remove nil? (map to-node the-seq))]
      (cond
        (inner? cmd)
        (cond
          (append? cmd)
          (doseq [new-node nodes]
            (when (not (fn? new-node))
              (.appendChild node new-node)))

          (prepend? cmd)
          (doseq [new-node (reverse nodes)]
            (when (not (fn? new-node))
              (if (= 0 (count (.-childNodes node)))
                (.appendChild node new-node)
                (.insertBefore node new-node (.-firstChild node))
                )))

          :else
          (do
            (set! (.-innerHTML node) "")
            (doseq [new-node nodes ]
              (let [clone (.cloneNode node false)]
                (.removeAttribute clone "id")
                (.insertBefore  (.-parentElement node) clone node)
                (.appendChild clone new-node)))
            (.removeChild (.-parentElement node) node)
            ))

        (append? cmd)
        (doseq [new-node (reverse nodes)]
          (.insertBefore (.-parentElement node) new-node (.-nextSibling node)))

        (prepend? cmd)
        (doseq [new-node nodes]
          (.insertBefore (.-parentElement node) new-node node))

        :else
        (do
          (doseq [new-node nodes ]
            (if (fn? new-node)
              (let [clone (.cloneNode node true)]
                (.removeAttribute clone "id")
                (.insertBefore  (.-parentElement node) clone node)
                (alter new-node :! clone))
              (.insertBefore  (.-parentElement node) new-node node)
              ))
          (.removeChild (.-parentElement node) node)
          )

        )

      ))
  )

(defn- alter-nil
  "It's nil, so remove the element"
  [_ cmd node]
  (if (inner? cmd)
    (set! (.-innerHTML node) "")
    (.removeChild (.-parentElement node) node)))

(defn- alter-func
  "It's a function. Apply the function.

  If the function returns true (not truthy but true), retain the node.
  false delete the node.

  If the function has :raw in its metadata, just apply the function
  and trust it's done DOM mutation"
  [the-fn cmd node]
  (if (-> the-fn meta :raw)
    (the-fn node cmd)
    (let [res (the-fn node cmd)]
      (cond
        (true? res) node                                    ;; no-op
        (false? res) (alter nil cmd node)
        :else (alter res cmd node)))
    ))

(defn- alter-node
  "We've got a Node, so do the right thing with it"
  [new-node cmd node]
  (if (identical? new-node node)
    node                                                    ;; no-op
    (cond
      (inner? cmd)
      (cond
        (append? cmd)
        (.appendChild node new-node)

        (prepend? cmd)
        (if (= 0 (count (.-childNodes node)))
          (.appendChild node new-node)
          (.insertBefore node new-node (.-firstChild node))
          )

        :else
        (do
          (set! (.-innerHTML node) "")
          (.appendChild node new-node)
          ))

      (append? cmd)
      (.insertBefore (.-parentElement node) new-node (.-nextSibling node))

      (prepend? cmd)
      (.insertBefore (.-parentElement node) new-node node)

      :else
      (do
        (.insertBefore (.-parentElement node) new-node node)
        (.removeChild (.-parentElement node) node)
        ))))


(defn- fixed-name
  "Fixes the name of the attribute key"
  [k]
  (let [k (name k)
        len (count k)]
    (cond
      (gstring/endsWith k "--") (.substring k 0 (- len 2))
      (or
        (gstring/endsWith k ">")
        (gstring/endsWith k "<")) (.substring k 0 (- len 1))
      :else k)))

(defn- alter-map
  "Deal with altering the attributes"
  [the-map cmd node]
  (let [attrs (.-attributes node)]
    (doseq [[key v] the-map]
      (let [k (fixed-name key)
            ]
        (if (nil? v)
          (.removeNamedItem attrs k)

          (let [at (or (.getNamedItem attrs k)
                       (let [new-at (.createAttribute js/document k)]
                         (.setNamedItem attrs new-at)
                         new-at)
                       )
                old-str (.-value at)

                the-str (cond
                          (and (fn? v)
                               (not (gstring/startsWith k "on")))
                          (v old-str)

                          (and (fn? v)
                               (gstring/startsWith k "on"))
                          (guid-for v)

                          :else v)
                the-str (if (= "class" k) (str " " the-str " ") the-str)
                new-str (cond
                          (append? key) (str old-str the-str)
                          (prepend? key) (str the-str old-str)
                          (remove? key) (.replace old-str (.trim the-str) "")
                          :else the-str)]
            (set! (.-value at) new-str)
            ))))))

(defn alter
  "Given a value, a command, and a node, do the right thing"
  [val cmd node]
  (cond
    (nil? val) (alter-nil val cmd node)
    (string? val) (alter-string val cmd node)
    (map? val) (alter-map val cmd node)
    (vector? val) (alter-vec val cmd node)
    (seq? val) (alter-seq val cmd node)
    (instance? js/Node val) (alter-node val cmd node)
    (fn? val) (alter-func val cmd node)
    :else (throw (pr-str [val cmd node])))
  )

(defn do-select
  "Selects all the nodes based on the CSS or if '.' return the root"
  [dom css]
  (cond
    (= "." css) [dom]
    :else (.querySelectorAll dom css)))

(defn xf
  "Builds a function that is applied to an element
  that takes the query and applies the changes"
  ([css op] (xf css :! op))
  ([css cmd op]
   (with-meta
     (fn [dom]
       (wrap-funcs
         (fn []
           (let [convert? (or (string? dom) (vector? dom))
                 dom (to-doc-frag dom)]
             (doseq [node (do-select dom css)] (alter op cmd node))
             (if convert?
               (to-hiccup dom)
               dom)))))
     :raw)))


(defn xform
  "Runs a series of css transforms on the dom and returns a Hicup data structure
  dom -- can be a String (converted into HTML), Hiccup (converted to html) or a js/Node
  cmds -- a series of 2 or 3 element vectors with [css-selector operation] [css-selector :command operation]

  Where css-selector is a string that contains a CSS selector
        command is :* (replace the inner HTML), :*> (append inner), :*< prepend inner, :! replace (default), :> append, :< prepend
        operation is string (text node), Hiccup template, DOM Node, map (keys and values are used to update the selected element's attributes, command ignored)
                     or a function that takes the command and the found node and returns a node to be run with the command
  "
  {:pre [(every? vector? funcs)]}
  [dom & funcs]

  (wrap-funcs
    (fn []
      (if (satisfies? IDeref dom)
        (apply xform @dom funcs)
        (let [convert? (or (string? dom) (vector? dom))
              dom (to-doc-frag dom)]
          (doseq [[css cmd op] funcs]
            (let [[cmd op] (if (nil? op) [:! cmd] [cmd op])]
              (doseq [node (do-select dom css)] (alter op cmd node))))
          (if convert?
            (to-hiccup dom)
            dom)
          )))))
