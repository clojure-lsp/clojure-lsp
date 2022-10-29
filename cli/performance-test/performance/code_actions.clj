(ns performance.code-actions
  (:require
   [clojure.java.io :as io]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]
   [performance.helper :as perf.h]))

(defn run [project threshold-ms]
  (lsp/start-process! (str "performance-test/" project "/"))
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions fixture/default-init-options
                   :capabilities          {:window    {:showDocument {:support true}}
                                           :workspace {:workspaceEdit {:documentChanges true}}}}
                  (h/file->uri (io/file (perf.h/project->abs-path project)))))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "")))
