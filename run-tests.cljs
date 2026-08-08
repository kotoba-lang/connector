#!/usr/bin/env nbb
;; nbb test runner.
;;
;;   nbb --classpath "src:test:../org-anthropic-mcp/src" run-tests.cljs
;;
;; org-anthropic-mcp is on the classpath because `connector.mcp` requires it —
;; it is a declared dependency in deps.edn, not an optional extra.
(require '[clojure.test :as t] 'connector.core-test 'connector.mcp-test)

(let [{:keys [fail error]} (t/run-tests 'connector.core-test 'connector.mcp-test)]
  (js/process.exit (if (pos? (+ fail error)) 1 0)))
