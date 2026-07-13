(ns replicant.hydration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.walk :as walk]
            [replicant.core :as r]
            [replicant.hydration :as hydration]
            [replicant.mutation-log :as mutation-log]
            [replicant.test-helper :as h]))

;; Hydration runs against the atom-based nodes of replicant.mutation-log.
;; "Server-rendered DOM" is built by rendering hiccup through a mutation-log
;; renderer and normalizing the result to look like freshly parsed HTML.

(defn element [tag-name]
  (atom {:tag-name tag-name ::mutation-log/id (swap! mutation-log/id inc)}))

(defn text-node [text]
  (atom {:text text ::mutation-log/id (swap! mutation-log/id inc)}))

(defn comment-node [text]
  (atom {:comment text ::mutation-log/id (swap! mutation-log/id inc)}))

(defn insert-node
  "Insert `child` in `parent` before `reference`, or last."
  [parent child & [reference]]
  (if reference
    (swap! parent update :children mutation-log/-insert-before child reference)
    (swap! parent update :children #(conj (vec %) child)))
  (mutation-log/set-parent child parent))

(defn mock-renderer [el]
  (mutation-log/create-renderer {:element el}))

(defn normalize
  "Make a rendered tree look like freshly parsed HTML: merge adjacent text
  nodes, drop empty ones, and strip event handlers and memory."
  [el]
  (swap! el dissoc :on :replicant/memory)
  (swap! el update :children
         (fn [children]
           (reduce
            (fn [children child]
              (let [prev (peek children)]
                (cond
                  (not (contains? @child :text))
                  (conj children child)

                  (= "" (:text @child))
                  children

                  (and prev (contains? @prev :text))
                  (do (swap! prev update :text str (:text @child))
                      children)

                  :else
                  (conj children child))))
            []
            children)))
  (doseq [child (:children @el)]
    (when (:tag-name @child)
      (normalize child)))
  el)

(defn server-dom
  "Build the mock equivalent of parsed, server-rendered HTML for `hiccup`."
  [hiccup & [opts]]
  (let [el (element "body")]
    (r/reconcile (mock-renderer el) el hiccup nil opts)
    (normalize el)
    el))

(defn hydrate-into [el hiccup & [opts]]
  (let [delegate (mock-renderer el)
        hydrator (hydration/create-renderer delegate el)
        res (r/reconcile hydrator el hiccup nil opts)]
    (hydration/sweep delegate el (:vdom res))
    (assoc res :el el :issues @(:issues hydrator) :delegate delegate)))

(defn hydrate-world
  "Server-render `server-hiccup`, then hydrate `client-hiccup` into it."
  ([hiccup] (hydrate-world hiccup hiccup))
  ([server-hiccup client-hiccup & [opts]]
   (hydrate-into (server-dom server-hiccup opts) client-hiccup opts)))

(defn fresh-world
  "What a from-scratch render produces in an empty element."
  [hiccup & [opts]]
  (let [el (element "body")
        renderer (mock-renderer el)]
    (-> (r/reconcile renderer el hiccup nil opts)
        (assoc :el el :renderer renderer))))

(defn rerender [world hiccup & [opts]]
  (let [renderer (mock-renderer (:el world))]
    (-> (r/reconcile renderer (:el world) hiccup (:vdom world)
                     (merge {:unmounts (:unmounts world)
                             :unmount-hooks (:unmount-hooks world)}
                            opts))
        (assoc :el (:el world) :renderer renderer))))

(defn snapshot
  "Snapshot the world's DOM for comparisons. Nodes that have had children
  added and removed keep an empty :children, unlike nodes that never had
  children - erase that difference."
  [world]
  (walk/postwalk
   (fn [x]
     (cond-> x
       (and (map? x) (empty? (:children x))) (dissoc :children)))
   (h/get-snapshot (:el world))))

(defn created-by-delegate [world]
  (->> @(:log (:delegate world))
       (filter (comp #{:create-element :create-text-node} first))))

(defn descendants-of [el]
  (mapcat (fn [child]
            (cons child (when (:tag-name @child)
                          (descendants-of child))))
          (:children @el)))

(deftest hydration-test
  (testing "Adopts server-rendered nodes instead of creating new ones"
    (let [el (server-dom [:div [:p "One"] [:span "Two"]])
          before (descendants-of el)
          world (hydrate-into el [:div [:p "One"] [:span "Two"]])
          after (descendants-of el)]
      (is (= (snapshot (fresh-world [:div [:p "One"] [:span "Two"]]))
             (snapshot world)))
      (is (empty? (:issues world)))
      (is (empty? (created-by-delegate world)))
      (is (= 5 (count after)))
      (is (every? true? (map identical? before after)))))

  (testing "Attaches event handlers to adopted nodes"
    (let [f (fn [_e])
          hiccup [:div [:button {:on {:click f}} "Click"]]
          world (hydrate-world hiccup)
          button (-> world :el deref :children first deref :children first)]
      (is (= f (get-in @button [:on :click])))
      (is (empty? (created-by-delegate world)))))

  (testing "Splits text nodes the HTML parser merged"
    (let [hiccup [:p "Hello, " "world" "!"]
          world (hydrate-world hiccup)
          p (-> world :el deref :children first)]
      (is (= ["Hello, " "world" "!"]
             (mapv (comp :text deref) (:children @p))))
      (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
      (is (empty? (:issues world)))
      (is (empty? (created-by-delegate world)))))

  (testing "Creates text nodes for empty hiccup strings"
    (let [hiccup [:p "" "Text" ""]
          world (hydrate-world hiccup)]
      (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
      (is (empty? (:issues world)))))

  (testing "Tolerates nil children"
    (let [hiccup [:div nil [:p "Text"] nil [:span "More"]]
          world (hydrate-world hiccup)]
      (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
      (is (empty? (:issues world)))
      (is (empty? (created-by-delegate world)))))

  (testing "Flattens seq children"
    (let [hiccup [:ul (map (fn [x] [:li x]) ["a" "b" "c"])]
          world (hydrate-world hiccup)]
      (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
      (is (empty? (created-by-delegate world)))))

  (testing "Hydrates keyed children"
    (let [hiccup [:ul [:li {:replicant/key 1} "a"] [:li {:replicant/key 2} "b"]]
          world (hydrate-world hiccup)]
      (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
      (is (empty? (created-by-delegate world)))))

  (testing "Hydrates a top-level list of nodes"
    (let [hiccup (list [:h1 "Title"] [:p "Text"])
          world (hydrate-world hiccup)]
      (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
      (is (empty? (created-by-delegate world)))))

  (testing "Hydrating an empty element degrades to a fresh render"
    (let [hiccup [:div [:p "Text"]]
          world (hydrate-into (element "body") hiccup)]
      (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
      (is (empty? (:issues world)))
      (is (= 3 (count (created-by-delegate world))))))

  (testing "Hydrates SVG with case-sensitive tag names"
    (let [hiccup [:svg {:viewBox "0 0 100 100"}
                  [:path {:d "M0 0"}]
                  [:foreignObject [:p "Text"]]]
          world (hydrate-world hiccup)]
      (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
      (is (empty? (:issues world)))
      (is (empty? (created-by-delegate world))))))

(deftest junk-nodes-test
  (testing "Removes comments and whitespace between elements"
    (let [hiccup [:div [:p "One"] [:span "Two"]]
          el (server-dom hiccup)
          div (first (:children @el))
          p (first (:children @div))]
      (insert-node div (comment-node "server junk") p)
      (insert-node div (text-node "\n  ") (hydration/next-sibling p))
      (let [world (hydrate-into el hiccup)]
        (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
        (is (empty? (:issues world)))
        (is (empty? (created-by-delegate world))))))

  (testing "Removes comments before text nodes"
    (let [hiccup [:p "Text"]
          el (server-dom hiccup)
          p (first (:children @el))]
      (insert-node p (comment-node "x") (first (:children @p)))
      (let [world (hydrate-into el hiccup)]
        (is (= (snapshot (fresh-world hiccup)) (snapshot world)))
        (is (empty? (:issues world))))))

  (testing "Sweeps trailing junk the cursor can't see"
    (let [hiccup [:div [:p "One"]]
          el (server-dom hiccup)
          div (first (:children @el))]
      (insert-node div (comment-node "trailing"))
      (insert-node div (text-node "\n"))
      (insert-node el (comment-node "after root"))
      (let [world (hydrate-into el hiccup)]
        (is (= (snapshot (fresh-world hiccup)) (snapshot world)))))))

(deftest mismatch-test
  (testing "Replaces text that differs from the server render"
    (let [world (hydrate-world [:p "Server text"] [:p "Client text"])]
      (is (= (snapshot (fresh-world [:p "Client text"])) (snapshot world)))
      (is (= [{:type :text-mismatch
               :expected "Client text"
               :found {:text "Server text"}}]
             (:issues world)))))

  (testing "Replaces elements that differ from the server render"
    (let [world (hydrate-world [:div [:p "Text"]] [:div [:h2 "Text"]])]
      (is (= (snapshot (fresh-world [:div [:h2 "Text"]])) (snapshot world)))
      (is (= [{:type :tag-mismatch
               :expected "h2"
               :found {:tag "p"}}]
             (:issues world)))))

  (testing "Resumes adoption after a mismatched element"
    (let [el (server-dom [:div [:p "One"] [:span "Two"] [:p "Three"]])
          p3 (-> @el :children first deref :children last)
          world (hydrate-into el [:div [:p "One"] [:em "Two"] [:p "Three"]])
          div (first (:children @el))]
      (is (= (snapshot (fresh-world [:div [:p "One"] [:em "Two"] [:p "Three"]]))
             (snapshot world)))
      (is (= [:tag-mismatch :tag-mismatch] (map :type (:issues world))))
      ;; The em is new, but the server-rendered p following it is adopted
      (is (identical? p3 (last (:children @div))))))

  (testing "Changed text does not sabotage adoption of its siblings"
    (let [el (server-dom [:div "May 25th" [:section [:h2 "Extensive"] [:p "Content"]]])
          section (-> @el :children first deref :children second)
          world (hydrate-into el [:div "July 12th" [:section [:h2 "Extensive"] [:p "Content"]]])
          div (first (:children @(:el world)))]
      (is (= (snapshot (fresh-world [:div "July 12th" [:section [:h2 "Extensive"] [:p "Content"]]]))
             (snapshot world)))
      (is (identical? section (second (:children @div))))))

  (testing "Replacement nodes get their attributes, classes and styles"
    (let [client [:div {:class "wrap" :id "w" :style {:color "red"}}
                  [:a {:href "/x" :class "link"} "Link"]]
          world (hydrate-world [:span "mismatch"] client)]
      (is (= (snapshot (fresh-world client)) (snapshot world)))
      (is (= [:tag-mismatch] (mapv :type (:issues world))))))

  (testing "Replacement nodes get :innerHTML"
    (let [client [:div {:innerHTML "<b>raw</b>"}]
          world (hydrate-world [:span "mismatch"] client)]
      (is (= (snapshot (fresh-world client)) (snapshot world)))))

  (testing "Creates nodes missing from the server render"
    (let [world (hydrate-world [:div [:p "One"]] [:div [:p "One"] [:p "Two"]])]
      (is (= (snapshot (fresh-world [:div [:p "One"] [:p "Two"]])) (snapshot world)))))

  (testing "Removes nodes not in the client hiccup"
    (let [world (hydrate-world [:div [:p "One"] [:p "Two"]] [:div [:p "One"]])]
      (is (= (snapshot (fresh-world [:div [:p "One"]])) (snapshot world))))))

(deftest inner-html-test
  (testing "Adopts :innerHTML elements without touching their contents"
    (let [hiccup [:div [:div {:innerHTML "<h2>Hello</h2>"}] [:p "After"]]
          el (server-dom hiccup)
          html-div (-> @el :children first deref :children first)
          ;; Simulate the browser having parsed the innerHTML markup
          parsed (element "h2")]
      (insert-node html-div parsed)
      (insert-node parsed (text-node "Hello"))
      (let [world (hydrate-into el hiccup)]
        (is (empty? (:issues world)))
        (is (empty? (created-by-delegate world)))
        (is (identical? parsed (first (:children @html-div))))))))

(deftest life-cycle-test
  (testing "Calls mount hooks once, with the adopted node"
    (let [mounted (atom [])
          hiccup (fn [f] [:div [:p {:replicant/on-mount f} "Text"]])
          el (server-dom (hiccup (fn [_])))
          p (-> @el :children first deref :children first)
          _ (hydrate-into el (hiccup #(swap! mounted conj %)))
          events @mounted]
      (is (= 1 (count events)))
      (is (= :replicant.life-cycle/mount (:replicant/life-cycle (first events))))
      (is (identical? p (:replicant/node (first events))))))

  (testing "Memory written in a hydration mount hook is recalled on update"
    (let [recalled (atom nil)
          world (hydrate-world
                 [:p "Text"]
                 [:p {:replicant/on-mount (fn [e] ((:replicant/remember e) :hello))}
                  "Text"])
          _ (rerender world [:p {:replicant/on-render (fn [e] (reset! recalled (:replicant/memory e)))
                                 :title "Poke"}
                             "Text"])]
      (is (= :hello @recalled))))

  (testing "Does not apply :replicant/mounting attributes during hydration"
    (let [hiccup [:div {:class :a :replicant/mounting {:class :b}} "Text"]
          world (hydrate-world hiccup)
          div (-> world :el deref :children first)]
      (is (= #{"a"} (:classes @div)))
      (is (empty? (:issues world))))))

(deftest alias-test
  (testing "Hydrates aliases"
    (let [aliases {:ui/box (fn [attrs children] (into [:div.box (dissoc attrs :replicant/alias-data)] children))}
          hiccup [:div [:ui/box "Boxed " [:em "content"]] [:p "Two"]]
          opts {:aliases aliases}
          world (hydrate-world hiccup hiccup opts)]
      (is (= (snapshot (fresh-world hiccup opts)) (snapshot world)))
      (is (empty? (:issues world)))
      (is (empty? (created-by-delegate world))))))

(deftest subsequent-render-test
  (testing "Renders after hydration behave like renders after a fresh render"
    (let [h1 [:ul [:li {:replicant/key 1} "One"]
              [:li {:replicant/key 2} "Two"]
              [:li {:replicant/key 3} "Three"]]
          h2 [:ul [:li {:replicant/key 3} "Three"]
              [:li {:replicant/key 1} "One!"]
              [:li {:replicant/key 4} "Four"]]]
      (is (= (snapshot (rerender (fresh-world h1) h2))
             (snapshot (rerender (hydrate-world h1) h2))))))

  (testing "Re-rendering the same hiccup after hydration is a no-op"
    (let [hiccup [:div {:title "Hi"} [:p "One"] [:span "Two"]]
          world (rerender (hydrate-world hiccup) hiccup)]
      (is (empty? @(:log (:renderer world)))))))

;; Generative tests: hydrating server-rendered hiccup must be
;; indistinguishable from rendering it from scratch, both immediately and
;; through a subsequent render.

(def gen-text
  (gen/frequency [[7 gen/string-alphanumeric]
                  [1 (gen/elements [" " "a b c" "x \"y\" z"])]]))

(def gen-attrs
  (gen/map (gen/elements [:title :id :data-thing])
           (gen/not-empty gen/string-alphanumeric)
           {:max-elements 2}))

(def gen-hiccup
  (gen/recursive-gen
   (fn [inner]
     (gen/let [tag (gen/elements [:div :p :span :ul :li :section :h1])
               attrs gen-attrs
               children (gen/vector (gen/one-of [inner (gen/return nil)]) 0 4)]
       (into [tag attrs] children)))
   gen-text))

(defspec hydration-equals-fresh-render 100
  (prop/for-all [hiccup gen-hiccup]
    (let [hydrated (hydrate-world hiccup)]
      (and (empty? (:issues hydrated))
           (= (snapshot (fresh-world hiccup))
              (snapshot hydrated))))))

(defspec rerender-after-hydration-equals-rerender-after-fresh-render 50
  (prop/for-all [h1 gen-hiccup
                 h2 gen-hiccup]
    (= (snapshot (rerender (fresh-world h1) h2))
       (snapshot (rerender (hydrate-world h1) h2)))))

;; Attributes are excluded here: adoption matches nodes on tag and text only,
;; so a diverging server render can leave stale attributes on adopted nodes.
;; Attribute fidelity is only guaranteed under the documented contract (the
;; server rendered the same hiccup) and on nodes hydration creates itself
;; (covered by the mismatch tests above).
(defn structure-only [snapshot]
  (walk/postwalk
   (fn [x]
     (cond-> x
       (map? x) (select-keys [:tag-name :text :comment :children])))
   snapshot))

(defspec hydrating-a-diverged-server-render-heals-the-structure 50
  (prop/for-all [server gen-hiccup
                 client gen-hiccup]
    (= (structure-only (snapshot (fresh-world client)))
       (structure-only (snapshot (hydrate-world server client))))))
