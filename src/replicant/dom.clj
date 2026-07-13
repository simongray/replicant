(ns replicant.dom)

;; This namespace makes it possible to use these functions in cljc files.

(defn ^:export render
  "Render `hiccup` in DOM element `el`. Replaces any pre-existing content not
  created by this function. Subsequent calls with the same `el` will update the
  rendered DOM by comparing `hiccup` to the previous `hiccup`."
  [el hiccup & [{:keys [aliases alias-data]}]])

(defn ^:export hydrate
  "Like `render`, but expects `el` to already contain the DOM that `hiccup`
  renders to - typically the output of `replicant.string/render` called with
  the same `hiccup`. Adopts the existing nodes instead of rebuilding them."
  [el hiccup & [{:keys [aliases alias-data]}]])

(defn ^:export unmount
  "Unmounts elements in `el`, and clears internal state."
  [el])

(defn ^:export set-dispatch!
  "Register a global dispatch function for event handlers and life-cycle hooks
  that are not functions. See data-driven event handlers and life-cycle hooks in
  the user guide for details."
  [f])
