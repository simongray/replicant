(ns replicant.hydration-demo
  "In-browser test harness for replicant.dom/hydrate. Renders hiccup to an
  HTML string with replicant.string, parses it into a container with
  innerHTML - the same thing a server-rendered page does - then hydrates and
  asserts that the DOM was adopted, not rebuilt. Serves as the manual/visual
  verification for hydration; the automated matrix lives in
  replicant.hydration-test."
  (:require [replicant.dom :as d]
            [replicant.string :as rs]))

(defonce !results (atom []))

(def results-el (delay (let [el (js/document.createElement "div")]
                         (js/document.body.appendChild el)
                         el)))

(defn render-results []
  (let [results @!results]
    (d/render
     @results-el
     [:div
      [:h1 "Hydration scenarios"]
      [:p (str (count (filter :pass? results)) "/" (count results) " passed")]
      [:table
       [:thead [:tr [:th "Scenario"] [:th "Result"] [:th "Details"]]]
       [:tbody
        (for [{:keys [name pass? details]} results]
          [:tr {:replicant/key name}
           [:td name]
           [:td {:style {:color (if pass? "green" "red")
                         :font-weight "bold"}}
            (if pass? "PASS" "FAIL")]
           [:td [:code (str details)]]])]]])))

(defn report! [name pass? & [details]]
  (swap! !results conj {:name name :pass? (boolean pass?) :details details})
  (render-results)
  (set! js/window.__hydrationResults
        (clj->js (mapv #(select-keys % [:name :pass? :details])
                       (mapv #(update % :details str) @!results)))))

;; DOM comparison

(defn attr-map [el]
  (->> (.-attributes el)
       array-seq
       (map (fn [a] [(.-name a) (.-value a)]))
       ;; createElementNS does not reflect an xmlns attribute, while
       ;; server-rendered SVG markup carries one; it is semantically inert
       ;; in both cases.
       (remove (comp #{"xmlns"} first))
       (into {})))

(defn node-summary [node]
  (case (.-nodeType node)
    1 {:tag (.-localName node)
       :attrs (attr-map node)
       :children (mapv node-summary (array-seq (.-childNodes node)))}
    3 (.-data node)
    8 [:comment (.-data node)]
    [:node-type (.-nodeType node)]))

(defn summarize [el]
  (mapv node-summary (array-seq (.-childNodes el))))

(defn server-el
  "Create a container holding parsed server-rendered HTML for `hiccup`."
  [hiccup & [opt]]
  (let [el (js/document.createElement "div")]
    (set! (.-innerHTML el) (rs/render hiccup opt))
    el))

(defn fresh-el
  "What replicant.dom/render produces from scratch."
  [hiccup]
  (let [el (js/document.createElement "div")]
    (d/render el hiccup)
    el))

(defn capture-warnings [f]
  (let [warnings (atom [])
        orig (.-warn js/console)]
    (set! (.-warn js/console)
          (fn [& args]
            (swap! warnings conj (mapv str args))
            (.apply orig js/console (to-array args))))
    (let [result (f)]
      (set! (.-warn js/console) orig)
      {:result result :warnings @warnings})))

(defn hydrated=fresh?
  "Hydrate `hiccup` into its own server-rendered markup and compare the result
  to a from-scratch render. Returns [pass? details]."
  [hiccup & [server-hiccup opt]]
  (let [el (server-el (or server-hiccup hiccup) opt)
        {:keys [warnings]} (capture-warnings #(d/hydrate el hiccup))
        expected (summarize (fresh-el hiccup))
        actual (summarize el)]
    [(and (= expected actual)
          (or (some? server-hiccup) (empty? warnings)))
     (cond
       (not= expected actual) {:expected expected :actual actual}
       (and (nil? server-hiccup) (seq warnings)) {:unexpected-warnings warnings}
       :else nil)]))

;; Scenarios

(defn test-basic-adoption []
  (let [hiccup [:div [:h1 "Title"] [:p "Paragraph " [:a {:href "/x"} "link"]]]
        el (server-el hiccup)
        h1-before (.querySelector el "h1")
        a-before (.querySelector el "a")]
    (d/hydrate el hiccup)
    (report! "Adopts server-rendered nodes"
             (and (identical? h1-before (.querySelector el "h1"))
                  (identical? a-before (.querySelector el "a"))
                  (= (summarize (fresh-el hiccup)) (summarize el))))))

(defn test-event-handlers []
  (let [clicks (atom 0)
        hiccup (fn [n] [:div [:p "Clicked " (str n) " times"]
                        [:button {:on {:click (fn [_] (swap! clicks inc))}} "Click"]])
        el (server-el (hiccup 0))
        button-before (.querySelector el "button")]
    (d/hydrate el (hiccup 0))
    (.click (.querySelector el "button"))
    (.click (.querySelector el "button"))
    (report! "Click handler works on adopted node"
             (and (identical? button-before (.querySelector el "button"))
                  (= 2 @clicks))
             {:clicks @clicks})))

(defn test-text-splitting []
  (let [hiccup [:p "Hello, " "world" "!"]
        el (server-el hiccup)
        [pass? details] [(let [p (.querySelector el "p")]
                           (d/hydrate el hiccup)
                           (= ["Hello, " "world" "!"]
                              (mapv #(.-data %) (array-seq (.-childNodes p)))))
                         nil]]
    (report! "Splits parser-merged text nodes" pass? details)))

(defn test-update-after-text-splitting []
  (let [hiccup (fn [who] [:p "Hello, " who "!"])
        el (server-el (hiccup "world"))]
    (d/hydrate el (hiccup "world"))
    (d/render el (hiccup "hydration"))
    (report! "Updates split text nodes"
             (= (summarize (fresh-el (hiccup "hydration"))) (summarize el))
             {:actual (summarize el)})))

(defn test-empty-strings []
  (let [[pass? details] (hydrated=fresh? [:p "" "Text" ""])]
    (report! "Handles empty string children" pass? details)))

(defn test-escaped-text []
  (let [[pass? details] (hydrated=fresh? [:p "Quotes \" & <angles> & 'apostrophes'"])]
    (report! "Escaped text round-trips" pass? details)))

(defn test-inner-html []
  (let [hiccup [:div [:div {:innerHTML "<b>bold</b> markup"}] [:p "After"]]
        el (server-el hiccup)
        b-before (.querySelector el "b")]
    (d/hydrate el hiccup)
    (d/render el [:div [:div {:innerHTML "<b>bold</b> markup"}] [:p "After!"]])
    (report! "Leaves :innerHTML contents alone"
             (and (identical? b-before (.querySelector el "b"))
                  (= "After!" (.-textContent (.querySelector el "p")))))))

(defn test-svg []
  (let [hiccup [:svg {:viewBox "0 0 100 100"}
                [:circle {:cx "50" :cy "50" :r "40"}]
                [:foreignObject {:x "0" :y "0"} [:p "HTML in SVG"]]]
        el (server-el hiccup)
        circle-before (.querySelector el "circle")
        fo-before (.querySelector el "foreignObject")
        [pass? details] [(do (d/hydrate el hiccup)
                             (and (identical? circle-before (.querySelector el "circle"))
                                  (identical? fo-before (.querySelector el "foreignObject"))
                                  (= (summarize (fresh-el hiccup)) (summarize el))))
                         {:actual (summarize el)}]]
    (report! "Hydrates SVG incl. foreignObject" pass? details)))

(defn test-form-input []
  (let [hiccup [:form [:input {:type "text" :value "server value"}]]
        el (server-el hiccup)
        input (.querySelector el "input")]
    ;; The user typed before the JS bundle loaded
    (set! (.-value input) "user input")
    (d/hydrate el hiccup)
    (report! "Preserves user input in forms"
             (and (identical? input (.querySelector el "input"))
                  (= "user input" (.-value input)))
             {:value (.-value input)})))

(defn test-boolean-attributes []
  (let [hiccup [:div [:input {:type "checkbox" :checked true}]
                [:button {:disabled true} "Nope"]]
        el (server-el hiccup)]
    (d/hydrate el hiccup)
    (let [input (.querySelector el "input")
          button (.querySelector el "button")]
      (d/render el [:div [:input {:type "checkbox"}]
                    [:button "Yep"]])
      (report! "Boolean attributes update after hydration"
               (and (identical? input (.querySelector el "input"))
                    (false? (.-checked input))
                    (false? (.-disabled button)))
               {:checked (.-checked input) :disabled (.-disabled button)}))))

(defn test-indented-markup []
  (let [hiccup [:div [:ul [:li "One"] [:li "Two"]] [:p "Text"]]
        el (server-el hiccup {:indent 2})
        ul-before (.querySelector el "ul")]
    (capture-warnings #(d/hydrate el hiccup))
    (report! "Survives indented server markup"
             (and (identical? ul-before (.querySelector el "ul"))
                  (= (summarize (fresh-el hiccup)) (summarize el)))
             {:actual (summarize el)})))

(defn test-comment-junk []
  (let [hiccup [:div [:p "One"] [:p "Two"]]
        el (js/document.createElement "div")]
    (set! (.-innerHTML el) "<!-- ssr marker --><div><p>One</p><!-- x --><p>Two</p><!-- y --></div>")
    (let [{:keys [warnings]} (capture-warnings #(d/hydrate el hiccup))]
      (report! "Removes comment nodes"
               (and (= (summarize (fresh-el hiccup)) (summarize el))
                    (empty? warnings))
               {:actual (summarize el) :warnings warnings}))))

(defn test-mismatch-healing []
  (let [server [:div [:p "Rendered May 25th"] [:section [:h2 "Section"] [:p "Content"]]]
        client [:div [:p "Rendered July 12th"] [:section [:h2 "Section"] [:p "Content"]]]
        el (server-el server)
        section-before (.querySelector el "section")
        {:keys [warnings]} (capture-warnings #(d/hydrate el client))]
    (report! "Self-heals content mismatches (with warning)"
             (and (= (summarize (fresh-el client)) (summarize el))
                  (identical? section-before (.querySelector el "section"))
                  (seq warnings))
             {:warnings (count warnings)})))

(defn test-alien-dom []
  (let [client [:div {:class "post" :id "main"}
                [:h1 "Actual"]
                [:p "Content with " [:a {:href "/about"} "a link"]]]
        el (js/document.createElement "div")]
    (set! (.-innerHTML el) "<table><tr><td>Completely</td><td>different</td></tr></table>")
    (capture-warnings #(d/hydrate el client))
    (report! "Recovers from completely different DOM (incl. attributes)"
             (= (summarize (fresh-el client)) (summarize el))
             {:actual (summarize el)})))

(defn test-multiple-roots []
  (let [[pass? details] (hydrated=fresh? (list [:h1 "Title"] [:p "One"] [:p "Two"]))]
    (report! "Hydrates a top-level list of nodes" pass? details)))

(defn test-keyed-update []
  (let [items (fn [xs] [:ul (for [x xs] [:li {:replicant/key x} x])])
        el (server-el (items ["a" "b" "c"]))
        lis-before (array-seq (.querySelectorAll el "li"))]
    (d/hydrate el (items ["a" "b" "c"]))
    (d/render el (items ["c" "a" "d"]))
    (let [lis-after (array-seq (.querySelectorAll el "li"))]
      (report! "Keyed reorder after hydration reuses nodes"
               (and (= (summarize (fresh-el (items ["c" "a" "d"]))) (summarize el))
                    ;; "c" and "a" survive from the server render
                    (identical? (nth lis-before 2) (nth lis-after 0))
                    (identical? (nth lis-before 0) (nth lis-after 1)))
               {:actual (summarize el)}))))

(defn test-double-hydrate []
  (let [hiccup (fn [n] [:p "Version " (str n)])
        el (server-el (hiccup 1))]
    (d/hydrate el (hiccup 1))
    (d/hydrate el (hiccup 2))
    (report! "Second hydrate behaves like render"
             (= (summarize (fresh-el (hiccup 2))) (summarize el))
             {:actual (summarize el)})))

(defn test-unmount []
  (let [hiccup [:div [:p "Text"]]
        el (server-el hiccup)]
    (d/hydrate el hiccup)
    (d/unmount el)
    (report! "Unmount works after hydration"
             (= "" (.-innerHTML el))
             {:html (.-innerHTML el)})))

(defn test-aliases []
  (let [aliases {:ui/box (fn [attrs children]
                           (into [:div.box (dissoc attrs :replicant/alias-data)] children))}
        hiccup [:div [:ui/box "Boxed " [:em "content"]] [:p "After"]]
        el (server-el hiccup {:aliases aliases})
        em-before (.querySelector el "em")
        {:keys [warnings]} (capture-warnings #(d/hydrate el hiccup {:aliases aliases}))]
    (report! "Hydrates aliases"
             (and (identical? em-before (.querySelector el "em"))
                  (empty? warnings))
             {:warnings warnings})))

(defn test-mount-hook []
  (let [mounted (atom nil)
        el (server-el [:div [:p "Text"]])
        p-before (.querySelector el "p")]
    (d/hydrate el [:div [:p {:replicant/on-mount #(reset! mounted (:replicant/node %))} "Text"]])
    (report! "Mount hooks fire with the adopted node"
             (identical? p-before @mounted))))

(defn test-mounting-transition []
  (let [hiccup [:div {:class :final
                      :replicant/mounting {:class :enter}} "Text"]
        el (server-el hiccup)]
    (d/hydrate el hiccup)
    (let [div (.querySelector el "div")
          immediately-clean? (not (.contains (.-classList div) "enter"))]
      (js/setTimeout
       (fn []
         (report! ":replicant/mounting does not run on hydration"
                  (and immediately-clean?
                       (not (.contains (.-classList div) "enter"))
                       (.contains (.-classList div) "final"))
                  {:class (.-className div)}))
       100))))

(defn ^:export main []
  (reset! !results [])
  (test-basic-adoption)
  (test-event-handlers)
  (test-text-splitting)
  (test-update-after-text-splitting)
  (test-empty-strings)
  (test-escaped-text)
  (test-inner-html)
  (test-svg)
  (test-form-input)
  (test-boolean-attributes)
  (test-indented-markup)
  (test-comment-junk)
  (test-mismatch-healing)
  (test-alien-dom)
  (test-multiple-roots)
  (test-keyed-update)
  (test-double-hydrate)
  (test-unmount)
  (test-aliases)
  (test-mount-hook)
  (test-mounting-transition))
