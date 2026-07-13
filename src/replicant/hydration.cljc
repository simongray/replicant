(ns ^:no-doc replicant.hydration
  "Machinery behind `replicant.dom/hydrate`: adopt the DOM nodes already in
  the container during a first render, instead of creating new ones.

  On a first render, Replicant creates nodes in pre-order over the hiccup,
  which is document order over the corresponding server-rendered HTML. The
  hydrating renderer wraps a real renderer (the \"delegate\") and keeps a
  cursor into the existing DOM: when asked to create a node that matches the
  node at the cursor, it returns the existing node instead. Attribute writes
  on adopted nodes are no-ops - the server already rendered them - while
  everything else goes through the delegate. Mismatches self-heal by creating
  nodes for real, so hydrating is never worse than rendering from scratch.

  In Clojure, nodes are the atom-based nodes of `replicant.mutation-log`,
  which is how the JVM test suite exercises hydration."
  (:require [replicant.protocols :as replicant]
            [replicant.vdom :as vdom]
            #?(:clj [replicant.mutation-log :as mutation-log])))

;; Hydration needs a small read surface that isn't part of IRender, plus
;; splitText. Everything else goes through the delegate.

(defn ^:private element? [node]
  #?(:clj (some? (:tag-name @node))
     :default (= 1 (.-nodeType node))))

(defn ^:private text-node? [node]
  #?(:clj (contains? @node :text)
     :default (= 3 (.-nodeType node))))

(defn ^:private comment-node? [node]
  #?(:clj (contains? @node :comment)
     :default (= 8 (.-nodeType node))))

(defn local-name [node]
  #?(:clj (:tag-name @node)
     :default (.-localName node)))

(defn node-data [node]
  #?(:clj (let [node @node]
            (or (:text node) (:comment node)))
     :default (.-data node)))

(defn first-child [node]
  #?(:clj (first (:children @node))
     :default (.-firstChild node)))

(defn parent-node [node]
  #?(:clj (:parent (meta @node))
     :default (.-parentNode node)))

(defn next-sibling [node]
  #?(:clj (when-let [siblings (some-> (parent-node node) deref :children)]
            (second (drop-while #(not (identical? node %)) siblings)))
     :default (.-nextSibling node)))

(defn ^:private split-text
  "Split the text `node` at `offset`, leaving the prefix in `node` and the
  remainder in a new text node placed right after it. Returns the new node."
  [node offset]
  #?(:clj (let [parent (parent-node node)
                sibling (next-sibling node)
                remainder (atom {:text (subs (:text @node) offset)
                                 :replicant.mutation-log/id (swap! mutation-log/id inc)})]
            (swap! node update :text subs 0 offset)
            (if sibling
              (swap! parent update :children mutation-log/-insert-before remainder sibling)
              (swap! parent update :children #(conj (vec %) remainder)))
            (mutation-log/set-parent remainder parent)
            remainder)
     :default (.splitText node offset)))

;; The hydrating renderer

(defn ^:private whitespace-text? [node]
  (and (text-node? node)
       (= 0 (count (.trim ^String (node-data node))))))

(defn ^:private matching-tag?
  "localName is lower-cased for HTML elements and case-preserving for SVG
  (e.g. foreignObject), matching Replicant's tag names in both cases."
  [tag-name node]
  (and (element? node) (= tag-name (local-name node))))

(defn ^:private describe-node [node]
  (cond
    (nil? node) nil
    (element? node) {:tag (local-name node)}
    (text-node? node) {:text (node-data node)}
    :else {:comment (node-data node)}))

(defn ^:private drop-junk
  "Remove comment nodes - and whitespace-only text nodes when
  `whitespace-too?` - at the cursor. Junk can not simply be skipped over:
  leftover nodes would shift the indexes Replicant uses to address children
  on subsequent renders."
  [delegate cursor whitespace-too?]
  (loop []
    (let [node @cursor]
      (when (and node
                 (or (comment-node? node)
                     (and whitespace-too? (whitespace-text? node))))
        (vreset! cursor (next-sibling node))
        (replicant/remove-child delegate (parent-node node) node)
        (recur)))))

(defn ^:private matching-tag-ahead?
  "Does the first non-junk sibling after `node` match `tag-name`? Then `node`
  is a leftover from content that changed since the server render, and
  adoption should resume after it. Otherwise the hiccup contains a node the
  server render didn't have, and `node` may still be adopted by the next
  sibling in the hiccup."
  [tag-name node]
  (loop [node (next-sibling node)]
    (cond
      (nil? node) false
      (or (comment-node? node) (whitespace-text? node)) (recur (next-sibling node))
      :else (matching-tag? tag-name node))))

(defn ^:private created-nodes []
  #?(:clj (volatile! #{})
     :default (js/Set.)))

(defn ^:private track-created [created node]
  #?(:clj (vswap! created conj node)
     :default (.add created node)))

(defn ^:private created? [created node]
  #?(:clj (contains? @created node)
     :default (.has created node)))

(defn create-renderer
  "Create a hydrating renderer adopting the DOM nodes inside `el`, wrapping
  the real renderer `delegate`. Mismatches are recorded in the volatile under
  the returned renderer's `:issues` key. Throws from `append-child` on
  structural divergence; callers should respond by clearing `el` and
  rendering from scratch."
  [delegate el]
  (let [cursor (volatile! (first-child el))
        ;; When an element mismatches, it and its entire subtree are created
        ;; for real, during which no adoption can happen: the cursor parks
        ;; here and goes back into service when the subtree is done (see
        ;; append-child).
        parked (volatile! nil)
        created (created-nodes)
        issues (volatile! [])
        pass (fn [f]
               (fn [_ & args]
                 (apply f delegate args)))
        ;; The server already rendered all attributes on adopted nodes: leave
        ;; them be. This also keeps :replicant/mounting transitions from
        ;; running on adopted nodes during hydration. Nodes created to heal a
        ;; mismatch are brand new, however, and need their attributes for
        ;; real.
        created-only (fn [f]
                       (fn [this el & args]
                         (when (created? created el)
                           (apply f delegate el args))
                         this))]
    (with-meta
      {:issues issues}
      {`replicant/create-element
       (fn [_ tag-name options]
         (drop-junk delegate cursor true)
         (loop []
           (let [node @cursor]
             (cond
               (and node (matching-tag? tag-name node))
               (do
                 (vreset! cursor (first-child node))
                 node)

               (and node (matching-tag-ahead? tag-name node))
               (do
                 (vswap! issues conj {:type :tag-mismatch
                                      :expected tag-name
                                      :found (describe-node node)})
                 (vreset! cursor (next-sibling node))
                 (replicant/remove-child delegate (parent-node node) node)
                 (drop-junk delegate cursor true)
                 (recur))

               :else
               (let [fresh (replicant/create-element delegate tag-name options)]
                 (when node
                   (vswap! issues conj {:type :tag-mismatch
                                        :expected tag-name
                                        :found (describe-node node)})
                   (vreset! parked node)
                   (vreset! cursor nil))
                 (track-created created fresh)
                 fresh)))))

       `replicant/create-text-node
       (fn [_ text]
         (drop-junk delegate cursor false)
         (let [node @cursor]
           (if (and node
                    (text-node? node)
                    (.startsWith ^String (node-data node) text))
             (do
               ;; The HTML parser merges adjacent hiccup strings into a single
               ;; text node; split off the part this vdom node accounts for.
               ;; The remainder stays in place for the next create-text-node.
               (when-not (= text (node-data node))
                 (split-text node (count text)))
               (vreset! cursor nil)
               node)
             (let [fresh (replicant/create-text-node delegate text)]
               ;; Hiccup "" has no server-rendered counterpart, so an empty
               ;; text node is always created, and is not a mismatch.
               (when (and node (not= "" text))
                 (vswap! issues conj {:type :text-mismatch
                                      :expected text
                                      :found (describe-node node)}))
               (track-created created fresh)
               fresh))))

       `replicant/append-child
       (fn [this parent child]
         (if (created? created child)
           (do
             (when-not (created? created parent)
               ;; A created node just landed in an adopted parent (or the
               ;; root), so any subtree that parked the cursor is done.
               (when-let [node @parked]
                 (vreset! cursor node)
                 (vreset! parked nil)))
             (let [node @cursor]
               (if (and node (identical? parent (parent-node node)))
                 (replicant/insert-before delegate parent child node)
                 (replicant/append-child delegate parent child))))
           (do
             ;; An adopted node is already where it should be; just move the
             ;; cursor to the next adoption candidate. The parent check guards
             ;; the positional child indexing all subsequent renders rely on.
             (when-not (identical? parent (parent-node child))
               (throw (ex-info "Adopted node has unexpected parent - the DOM does not match the hiccup structurally"
                               {:child (describe-node child)
                                :expected-parent (describe-node parent)})))
             (vreset! cursor (next-sibling child))))
         this)

       `replicant/set-attribute (created-only replicant/set-attribute)
       `replicant/remove-attribute (created-only replicant/remove-attribute)
       `replicant/set-style (created-only replicant/set-style)
       `replicant/remove-style (created-only replicant/remove-style)
       `replicant/add-class (created-only replicant/add-class)
       `replicant/remove-class (created-only replicant/remove-class)

       ;; Attaching event handlers to the adopted nodes is the point of the
       ;; exercise; the rest is environmental. insert-before etc are only used
       ;; when there is a previous vdom, so they can't be reached through a
       ;; hydrating (first) render - delegate defensively.
       `replicant/set-event-handler (pass replicant/set-event-handler)
       `replicant/remove-event-handler (pass replicant/remove-event-handler)
       `replicant/attached? (pass replicant/attached?)
       `replicant/insert-before (pass replicant/insert-before)
       `replicant/remove-child (pass replicant/remove-child)
       `replicant/replace-child (pass replicant/replace-child)
       `replicant/remove-all-children (pass replicant/remove-all-children)
       `replicant/get-child (pass replicant/get-child)
       `replicant/on-transition-end (pass replicant/on-transition-end)
       `replicant/next-frame (pass replicant/next-frame)
       `replicant/remember (pass replicant/remember)
       `replicant/recall (pass replicant/recall)})))

(defn sweep
  "Walk the DOM in `el` and the hydrated `vdoms` (the :vdom vector returned
  by reconcile) in lockstep once, and remove - through `renderer` - any DOM
  node the vdom does not account for, e.g. junk trailing an element's last
  hydrated child, which the cursor never reaches."
  [renderer el vdoms]
  (loop [dom-node (first-child el)
         vdoms (seq vdoms)]
    (if (nil? vdoms)
      (when dom-node
        (let [next-node (next-sibling dom-node)]
          (replicant/remove-child renderer el dom-node)
          (recur next-node nil)))
      (let [v (first vdoms)]
        (cond
          (nil? v)
          (recur dom-node (next vdoms))

          ;; Aliases occupy a vdom node but no DOM node of their own: descend
          ;; into the expansion at the same DOM position.
          (qualified-keyword? (vdom/tag-name v))
          (recur dom-node (cons (first (vdom/children v)) (next vdoms)))

          :else
          (do
            (when (and dom-node
                       (element? dom-node)
                       (nil? (vdom/text v))
                       (not (:innerHTML (vdom/attrs v))))
              (sweep renderer dom-node (vdom/children v)))
            (recur (some-> dom-node next-sibling) (next vdoms))))))))
