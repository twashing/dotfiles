;; A collection of tools for gptel, based on the examples at
;; https://github.com/karthink/gptel/issues/514
;;
;; Note that running gptel-make-tool here does not automatically set tools to be used in
;; gptel. You can set tools to be used from the UI.

;; More possible tools, per Gemini
;;
;; extract_text_from_html: Takes HTML content (likely obtained from `read_url`) and
;; extracts the plain text, removing HTML tags.  Uses Emacs' built-in HTML parsing or a
;; dedicated library.
;;
;; fetch_json_from_url: Fetches data from a URL that returns JSON. Parses the JSON
;; and returns an Emacs Lisp object (list or hash-table).  Essential for interacting with
;; APIs.
;;
;; search_web: Performs a web search using a specified query.  Could use a search
;; engine API (e.g., Google, DuckDuckGo) or a library like `url.el`.  Returns a list of
;; search results (titles and URLs).
;;
;; search_emacs_documentation: Searches the Emacs documentation for a given keyword
;; or topic. Returns a list of relevant documentation entries.  Useful for helping the LLM
;; understand Emacs functionality.
;;
;; get_current_time: Returns the current date and time in a specified format.
;; Useful for timestamps and time-sensitive operations.
;;
;; translate_text_with_api: Uses a translation API (e.g., Google Translate, DeepL)
;; to translate text between languages.  Takes the text and target language as arguments.
;;
;; summarize_text_with_algorithm: Uses a local summarization algorithm (not an LLM)
;; to summarize text. This could be faster and cheaper than using the LLM for simple
;; summarization.


(provide 'jm-gptel-tools)

;;; Tools to build

