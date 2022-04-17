;;; ox-distill.el --- Distill-HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-
;;; Commentary:

;; This library implements a Distill-styled HTML back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

;;; Dependencies
(require 'cl-lib)
(require 'format-spec)
(require 'ox)

(defun org-distill--byline (info)
  (let ((first (format "
<meta charset=\"utf-8\">
<script src=\"https://distill.pub/template.v1.js\"></script>

<script type=\"text/front-matter\">
 title: \"%s\"
 description: \"%s\"
 authors:\n"
		       (org-export-data (plist-get info :title) info)
		       (org-export-data (plist-get info :subtitle) info)))
	(second
	 (let ((authors (split-string (org-export-data (plist-get info :author) info) ","))
	       (aemails (split-string (org-export-data (plist-get info :email) info) ","))
	       (str "")
	       )
	   (dotimes (idx (length authors) str)
	     (setq str
		   (concat str (format " - %s: %s\n" (nth idx authors) (nth idx aemails)))))))
	(third
	 (let ((affiliations (split-string (org-export-data (plist-get info :distill-affiliation) info) ","))
	       (affiliaturls (split-string (org-export-data (plist-get info :distill-affiliation-url) info) ","))
	       (str "")
	       )
	   (dotimes (idx (length affiliations) str)
	     (setq str
		   (concat str (format " - %s: %s\n" (nth idx affiliations) (nth idx affiliaturls)))))))
	)
    (concat first second " affiliations:\n" third "</script>\n")
    )
  )

(custom-set-variables
 '(org-html-head-include-default-style nil)
 '(org-html-head "")
 '(org-html-divs
   '((preamble "div" "preamble")
    (content "dt-article" "content")
    (postamble "div" "postamble")))
 '(org-html-postamble nil)
 '(org-html-preamble nil)
 )

(org-export-define-derived-backend 'distill 'html
  :menu-entry
  '(?d "Export to Distill-HTML"
       ((?D "As Distill-HTML buffer" org-distill-export-as-html)
	(?d "As HTML file" org-distill-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (org-distill-export-to-html t s v b)
		(org-open-file (org-distill-export-to-html nil s v b)))))))

  :options-alist '((:distill-affiliation "DISTILL_AFFILIATION" nil nil newline)
		   (:distill-affiliation-url "DISTILL_AFFILIATION_URL" nil nil newline)
		   (:distill-bibfile "DISTILL_BIBFILE" nil nil newline)
		   (:distill-appendix "DISTILL_APPENDIX" nil nil newline)
		   (:html-toplevel-hlevel "HTML_TOPLEVEL_HLEVEL" nil org-html-toplevel-hlevel)
		   )
  
  :translate-alist '(
		     (template . org-distill-template)
		     )
  )

;;;###autoload
(defun org-distill-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Distill-HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'distill "*Org Distill-HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-distill-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat
		     (when (> (length org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'distill file
      async subtreep visible-only body-only ext-plist)))


(defun org-distill-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (org-html-doctype info)
   "\n"
   (concat 
    (let ((link-up (org-trim (plist-get info :html-link-up)))
 	  (link-home (org-trim (plist-get info :html-link-home))))
      (unless (and (string= link-up "") (string= link-home ""))
	(format (plist-get info :html-home/up-format)
 		(or link-up link-home)
 		(or link-home link-up))))
    ;; Preamble.
    (org-distill--byline info)
    )
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div) ))
   ;; Document title and subtitle.
   (let ((title (and (plist-get info :with-title)
		     (plist-get info :title)))
	 (subtitle (plist-get info :subtitle))
	 )
     (concat
      (format
       "<h1>%s</h1>\n"
       (org-export-data title info))
      (format
       "<h2>%s</h2>\n"
       (org-export-data subtitle info))
      "<dt-byline></dt-byline>"
      ))
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Appendix
   (let ((appendix (org-export-data (plist-get info :distill-appendix) info)))
     (if (not (string= "" appendix))
	 (format "<dt-appendix>\n%s\n</dt-appendix>\n"
		 (with-temp-buffer
		   (insert-file-contents appendix)
		   (set-mark (point-min))
		   (goto-char (point-max))
		   (org-html-convert-region-to-html)
		   (buffer-string))
		 )
       "<dt-appendix></dt-appendix>\n"
       )
     )
   
   ;; Bibliography
   (let ((bibfile (org-export-data (plist-get info :distill-bibfile) info)))
     (if (not (string= "" bibfile))
	 (format "<script type=\"text/bibliography\">\n%s\n</script>\n"
		 (with-temp-buffer
		   (insert-file-contents bibfile)
		   (buffer-string))
		 )))

   ;; Postamble.
   ;; (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
 	     "</script><script src=\""
 	     org-html-klipse-js
 	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
 	     org-html-klipse-css "\"/>"))
   ;; Closing document.
   "\n")
  )



(provide 'ox-distill)

;;; ox-distill.el ends here
