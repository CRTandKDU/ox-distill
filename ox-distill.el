;;; ox-distill.el --- Distill-HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-
;;; Commentary:

;; This library implements a Distill-styled HTML back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

;;; Dependencies
(require 'cl-lib)
(require 'format-spec)
(require 'ol)
(require 'ox)

;; Support for linking citations, footnotes
(org-link-set-parameters "dtcite"
                         :follow	#'org-distill--cite-open
                         :export	#'org-distill--cite-export
                         :store		#'org-distill--cite-store-link)

(defun org-distill--cite-open (path _)
  "Visit a Distill citation link"
  )

(defun org-distill--cite-export (link description format _)
  (let ((desc (or description link)))
    (pcase format
      (`distill (format "<dt-cite key=\"%s\"></dt-cite>" desc))
      (t (format "%s" desc))))
  )

(defun org-distill--cite-store-link ()
  "Store a link to a Distill citation."
  )

(org-link-set-parameters "dtfn"
                         :follow	#'org-distill--fn-open
                         :export	#'org-distill--fn-export
                         :store		#'org-distill--fn-store-link)

(defun org-distill--fn-open (path _)
  "Visit a Distill citation link"
  )

(defun org-distill--fn-export (link description format _)
  (pcase format
    (`distill (format "<dt-fn>%s</dt-fn>" description))
    (t (format "%s" desc)))
  )


(defun org-distill--fn-store-link ()
  "Store a link to a Distill citation."
  )

;; Distill-style byline
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


;; Overwrites 'html custom variables (?)
;; (custom-set-variables
;;  '(org-html-head-include-default-style nil)
;;  '(org-html-head "")
;;  '(org-html-divs
;;    '((preamble "div" "preamble")
;;     (content "dt-article" "content")
;;     (postamble "div" "postamble")))
;;  '(org-html-postamble nil)
;;  '(org-html-preamble nil)
;;  )

;; Derive backend
(org-export-define-derived-backend 'distill 'html
  :menu-entry
  '(?d "Export to Distill-HTML"
       ((?D "As Distill-HTML buffer" org-distill-export-as-html)
	(?d "As Distill-HTML file" org-distill-export-to-html)
	(?o "As Distill-HTML file and open"
	    (lambda (a s v b)
	      (if a (org-distill-export-to-html t s v b)
		(org-open-file (org-distill-export-to-html nil s v b)))))))

  :options-alist '((:distill-affiliation "DISTILL_AFFILIATION" nil nil newline)
		   (:distill-affiliation-url "DISTILL_AFFILIATION_URL" nil nil newline)
		   (:distill-bibfile "DISTILL_BIBFILE" nil nil newline)
		   (:distill-appendix "DISTILL_APPENDIX" nil nil newline)
		   (:html-toplevel-hlevel "HTML_TOPLEVEL_HLEVEL" nil org-html-toplevel-hlevel)
		   )
  
  :translate-alist '((link . org-distill-link)
		     (template . org-distill-template)
		     )
  )

;; Overwrites 'html backend link exporting
(defun org-distill-link (link desc info)
  "Transcode a LINK object from Org to HTML, checks for Distill-typed links.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((html-ext (plist-get info :html-extension))
	 (dot (when (> (length html-ext) 0) "."))
	 (link-org-files-as-html-maybe
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
	    ;; needed.  See `org-html-link-org-files-as-html'.
	    (cond
	     ((and (plist-get info :html-link-org-files-as-html)
		   (string= ".org"
			    (downcase (file-name-extension raw-path "."))))
	      (concat (file-name-sans-extension raw-path) dot html-ext))
	     (t raw-path))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (org-string-nw-p desc))
	 (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto" "news"))
	    (url-encode-url (concat type ":" raw-path)))
	   ((string= "file" type)
	    ;; During publishing, turn absolute file names belonging
	    ;; to base directory into relative file names.  Otherwise,
	    ;; append "file" protocol to absolute file name.
	    (setq raw-path
		  (org-export-file-uri
		   (org-publish-file-relative-name raw-path info)))
	    ;; Possibly append `:html-link-home' to relative file
	    ;; name.
	    (let ((home (and (plist-get info :html-link-home)
			     (org-trim (plist-get info :html-link-home)))))
	      (when (and home
			 (plist-get info :html-link-use-abs-url)
			 (file-name-absolute-p raw-path))
		(setq raw-path (concat (file-name-as-directory home) raw-path))))
	    ;; Maybe turn ".org" into ".html".
	    (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
	    ;; Add search option, if any.  A search option can be
	    ;; relative to a custom-id, a headline title, a name or
	    ;; a target.
	    (let ((option (org-element-property :search-option link)))
	      (if (not option) raw-path
		(let ((path (org-element-property :path link)))
		  (concat raw-path
			  "#"
			  (org-publish-resolve-external-link option path t))))))
	   (t raw-path)))
	 (attributes-plist
	  (org-combine-plists
	   ;; Extract attributes from parent's paragraph.  HACK: Only
	   ;; do this for the first link in parent (inner image link
	   ;; for inline images).  This is needed as long as
	   ;; attributes cannot be set on a per link basis.
	   (let* ((parent (org-export-get-parent-element link))
		  (link (let ((container (org-export-get-parent link)))
			  (if (and (eq 'link (org-element-type container))
				   (org-html-inline-image-p link info))
			      container
			    link))))
	     (and (eq link (org-element-map parent 'link #'identity info t))
		  (org-export-read-attribute :attr_html parent)))
	   ;; Also add attributes from link itself.  Currently, those
	   ;; need to be added programmatically before `org-html-link'
	   ;; is invoked, for example, by backends building upon HTML
	   ;; export.
	   (org-export-read-attribute :attr_html link)))
	 (attributes
	  (let ((attr (org-html--make-attribute-string attributes-plist)))
	    (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ;; jmc - Check first for the Distill-type links, before resorting to HTML Monday, April 18, 2022
     ((org-export-custom-protocol-maybe link desc 'distill info))
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
	   (org-export-inline-image-p
	    link (plist-get info :html-inline-image-rules)))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (format "<a href=\"#%s\"%s>%s</a>"
		  (org-export-get-reference destination info)
		  attributes
		  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  ;; ID link points to an external file.
	  (`plain-text
	   (let ((fragment (concat "ID-" path))
		 ;; Treat links to ".org" files as ".html", if needed.
		 (path (funcall link-org-files-as-html-maybe
				destination info)))
	     (format "<a href=\"%s#%s\"%s>%s</a>"
		     path fragment attributes (or desc destination))))
	  ;; Fuzzy link points nowhere.
	  (`nil
	   (format "<i>%s</i>"
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Link points to a headline.
	  (`headline
	   (let ((href (org-html--reference destination info))
		 ;; What description to use?
		 (desc
		  ;; Case 1: Headline is numbered and LINK has no
		  ;; description.  Display section number.
		  (if (and (org-export-numbered-headline-p destination info)
			   (not desc))
		      (mapconcat #'number-to-string
				 (org-export-get-headline-number
				  destination info) ".")
		    ;; Case 2: Either the headline is un-numbered or
		    ;; LINK has a custom description.  Display LINK's
		    ;; description or headline's title.
		    (or desc
			(org-export-data
			 (org-element-property :title destination) info)))))
	     (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
	  ;; Fuzzy link points to a target or an element.
	  (_
           (if (and destination
                    (memq (plist-get info :with-latex) '(mathjax t))
                    (eq 'latex-environment (org-element-type destination))
                    (eq 'math (org-latex--environment-type destination)))
               ;; Caption and labels are introduced within LaTeX
	       ;; environment.  Use "ref" or "eqref" macro, depending on user
               ;; preference to refer to those in the document.
               (format (plist-get info :html-equation-reference-format)
                       (org-html--reference destination info))
             (let* ((ref (org-html--reference destination info))
                    (org-html-standalone-image-predicate
                     #'org-html--has-caption-p)
                    (counter-predicate
                     (if (eq 'latex-environment (org-element-type destination))
                         #'org-html--math-environment-p
                       #'org-html--has-caption-p))
                    (number
		     (cond
		      (desc nil)
		      ((org-html-standalone-image-p destination info)
		       (org-export-get-ordinal
			(org-element-map destination 'link #'identity info t)
			info 'link 'org-html-standalone-image-p))
		      (t (org-export-get-ordinal
			  destination info nil counter-predicate))))
                    (desc
		     (cond (desc)
			   ((not number) "No description for this link")
			   ((numberp number) (number-to-string number))
			   (t (mapconcat #'number-to-string number ".")))))
               (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" (org-html-encode-plain-text path))))
	(format "<a href=\"#%s\" %s%s>%s</a>"
		fragment
		(format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
			fragment fragment)
		attributes
		(format (org-export-get-coderef-format path desc)
			(org-export-resolve-coderef path info)))))
     ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\"%s>%s</a>"
	      (org-html-encode-plain-text path)
	      attributes
	      desc))
     ;; External link without a description part.
     (path
      (let ((path (org-html-encode-plain-text path)))
	(format "<a href=\"%s\"%s>%s</a>" path attributes path)))
     ;; No path, only description.  Try to do something useful.
     (t
      (format "<i>%s</i>" desc)))))

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
  (let ((org-html-head-include-default-style nil)
	(org-html-head "")
	(org-html-divs
	 '((preamble "div" "preamble")
	   (content "dt-article" "content")
	   (postamble "div" "postamble")))
	(org-html-postamble nil)
	(org-html-preamble nil))
    (org-export-to-buffer 'distill "*Org Distill-HTML Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (set-auto-mode t)))
    )
  )

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
  (let* ((org-html-head-include-default-style nil)
	(org-html-head "")
	(org-html-divs
	 '((preamble "div" "preamble")
	   (content "dt-article" "content")
	   (postamble "div" "postamble")))
	(org-html-postamble nil)
	(org-html-preamble nil)
	(extension (concat
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
