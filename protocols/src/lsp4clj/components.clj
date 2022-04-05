(ns lsp4clj.components
  (:import
   (lsp4clj.protocols.logger ILSPLogger)
   (lsp4clj.protocols.producer ILSPProducer)))

(defn ->components
  [db*
   ^ILSPLogger logger
   ^ILSPProducer producer]
  {:db* db*
   :logger logger
   :producer producer})
