#|
Oppgave: Webcrawler
Dato: 02.12.2009

Utviklet i CLISP

OBS!
Denne webcrawleren bruker pakken Drakma til HTTP-requests, drakma henter ut html i en streng
For å installere drakma (med asdf-install):
(require 'asdf)
(require 'asdf-install)
(asdf-install:install "drakma")
Du vil faa noen feilmeldinger, disse pga at vi ikke har alt som trengs for aa behandle https,
disse kan ignoreres (SKIP-GPG-CHECK, ACCEPT)

OBS!
Feil kan forekomme, siden ikke alle foelger samme syntaks i sin HTML-kode
|#

; ---- Ulike typer soek: ----

; crawler fra domene og lagrer lenker i en logfil
; brukes slik: (link-crawl "start" "logfil")
(defun link-crawl (domain logfile)
  (crawler (list domain) (list nil)
	      :next (lambda (html visited) (getURL html visited))
	      :test (lambda (node visited) (not (list-member node visited)))
	      :test2 (lambda (x) t)
	      :log (lambda (inURL toURL) (writeToFile logfile (format nil "(~A ~A)" inURL toURL)))
	      :combine (lambda (list next) (append list next))))

; crawler kun lenker innen samme domene
; brukes slik: (domain-limited-link-crawl "domene" "logfil")
(defparameter *base-domain* "")
(defun domain-limited-link-crawl (domain logfile)
  (defparameter *base-domain* (third (string-split "/" domain)))
  (crawler (list domain) (list nil)
	      :next (lambda (html visited) (getURL html visited))
	      :test (lambda (node visited) (and (not (list-member node visited)) (search *base-domain* (third (string-split "/" node)))))
	      :test2 (lambda (x) t)
	      :log (lambda (inURL toURL) (writeToFile logfile (format nil "(~A ~A)" inURL toURL)))
	      :combine (lambda (list next) (append list next))))

; ser etter linker til sider utenfor domenet vi søker i
; hjelpefunksjon som brukes i domain-limited-external-search
(defun checkforForeignURL (lst url)
	  (cond ((null lst) nil)
		((not (search url (first lst)))
		 (cons (first lst) (checkforForeignURL (rest lst) url)))
		(t (checkforForeignURL (rest lst) url))))
		 

; soker etter linker til eksterne domener
; bruk: (domain-limited-external-search "domene" "logfil")
(defun domain-limited-external-search (domain logfile)
  (defparameter *base-domain* (third (string-split "/" domain)))
  (crawler (list domain) (list nil)
	      :next (lambda (html visited) (getURL html visited))
	      :test (lambda (node visited) (and (not (list-member node visited)) (search *base-domain* (third (string-split "/" node)))))
	      :test2 (lambda (x) t)
	      :log (lambda (inURL toURL) (cond ((checkforForeignURL toURL *base-domain*)(print "fant ekstern")(writeToFile logfile  (format nil "(~A ~A)" inURL (checkforForeignURL toURL *base-domain*))))))
	      :combine (lambda (list next) (append list next))))

; soker etter ord i sider i et domene
; bruk (domain-limited-word-search "domene" "logfil" "ord som det skal soekes etter")
(defun domain-limited-word-search (domain logfile word)
  (defparameter *base-domain* (third (string-split "/" domain)))
  (crawler (list domain) (list nil)
	      :next (lambda (html visited) (getURL html visited))
	      :test (lambda (node visited) (and (not (list-member node visited)) (search *base-domain* (third (string-split "/" node)))))
	      :test2 (lambda (x) (cond ((search word x) t)))
	      :log (lambda (inURL toURL) (writeToFile logfile (format nil "(~A)" inURL)))
	      :combine (lambda (list next) (append list next))))

; soker etter ord i urler
; bruk: (word-in-url-search "domene" "logfil" "ord")
(defun word-in-url-search (domain logfile word)
  (defparameter *base-domain* (third (string-split "/" domain)))
  (crawler (list domain) (list nil)
	      :next (lambda (html visited) (getURL html visited))
	      :test (lambda (node visited) (and (not (list-member node visited)) (search *base-domain* (third (string-split "/" node)))))
	      :test2 (lambda (x) (cond ((search word x) t)))
	      :log (lambda (inURL toURL) (cond ((search word inURL) (writeToFile logfile (format nil "(~A)" inURL)))))
	      :combine (lambda (list next) (append list next))))

; ---- Hjelpefunksjoner: ----

; lager en liste over stringer ut fra en string (nytt element bestemmes av hva som gies som split-string (e.g whitespace))
(defun string-split (split-string string)
  (loop with l = (length split-string)
     for n = 0 then (+ pos l)
     for pos = (search split-string string :start2 n)
     if pos collect (subseq string n pos)
     else collect (subseq string n)
     while pos))

; ser om str er medlem i liste lst
(defun list-member (str lst)
  (cond ((null lst) nil)
	((equal str (first lst)))
	(t
	 (list-member str (rest lst)))))


; sjekker om det er en lovlig filendelse
; for a unnga a crawle bildefiler, css-stylesheets, javascript o.l
(defun doCrawl (str)
  (cond
    ((equal (linktype str) "html")
     t)
    ))

; sjekker filtype
(defun linktype (str)
  (cond
    ; stilsett
    ((search ".css" str) "stylesheet")

    ; javascript
    ((search ".js" str) "javascript")

    ; bilder
    ((search ".png" str) "image")
    ((search ".jpg" str) "image")
    ((search ".gif" str) "image")
    ((search ".bmp" str) "image")
    ((search ".psd" str) "image")
    ((search ".jpeg" str) "image")
    
    ; xml / rss / atom
    ; alle returnerer xml, siden rss og atom er xml
    ((search ".xml" str) "xml")
    ((search ".atom" str) "xml")
    ((search ".rss" str) "xml")

    ; vanlig innhold / html
    ((search ".php" str) "html")
    ((search ".htm" str) "html")
    ((search ".html" str) "html")
    ((search ".ece" str) "html")
    
    ; innhold med ukjent filendelse / uten filendelse (f.eks db.no, vg.no)
    (t "unsure")
    ))

; Feltvariabel for getURL
(defparameter *htmllist* '(""))
(defparameter *htmlstr* "")

; Splitter opp en string til lister, forsoeker aa splitte etter hvilken syntaks som blir brukt i HTMLen
; Bytter ut ' med " i listen som sendes videre
(defun getURL (string visited)
  ; erstatt ' med "
  (defparameter *htmlstr* (substitute #\" #\' string))
  ; split href=\"
  (cond ((search "href=\"" *htmlstr*)
	 (defparameter *htmllist* (string-split "href=\"" *htmlstr*)))
	; split href=
	((search "href=" *htmlstr*)
	 (defparameter *htmllist* (string-split "href=" *htmlstr*))))
  (getURLhelper *htmllist* visited)
)

; Splitter en string videre opp, og sender videre
(defun getURLhelper (lst visited)
    (cond ((null lst) nil)
	((doCrawl (first lst))
	 (cond
	   ; split streng inn i lenker med \"
	   ((search "\"" (first lst))
	    (append (getURLhelperhelper (string-split "\"" (first lst)) visited) (getURLhelper (rest lst) visited)))
	   ; split streng inn i lenker med >
	   ((search ">" (first lst))
	    (append (getURLhelperhelper (string-split ">" (first lst)) visited) (getURLhelper (rest lst) visited)))
	   ))
	(t (getURLhelper (rest lst) visited))))

; Ser etter urler i en liste
(defparameter *lasturl* "undefined")

(defun getURLhelperhelper (lst visited)
  (cond ((null lst) nil)
	; se om det er en gyldig lenke (grisete pga mange forbehold)
	((and (not (search "</a>" (first lst))) (and (and (and (doCrawl (first lst)) (search "http://" (first lst))) (not (list-member (first lst) visited))) (not (search "#" (first lst)))))
	 (defparameter *lasturl* (first lst))
	 (cons (first lst) (getURLhelperhelper (rest lst) visited)))

	; lag lenker til drakma ut fra relative lenker (type <a href="noe.html">)
	((and (and (not (search "http://" (first lst))) (not (search "#" (first lst)))) (and (doCrawl (first lst)) (not (search "</a>" (first lst)))))

	 ; er siste element i *lasturl* en referanse til et element (type http://noe.com/noe.html)
	 (cond ((search "." (subseq (reverse *lasturl*) 0 6))
		; finn f.eks "http://noe.com/noe/" ut fra "http://noe.com/noe/noe.html"
		(cons (format nil "~A~A" (subseq *lasturl* 0 (+ 1 (search "/" *lasturl* :from-end 5))) (first lst)) (getURLhelperhelper (rest lst) visited)))

	       ; hvis vi allerede har *lasturl* paa formen "http://noe.com/noe/, bruk denne
	       (t (cons (format nil "~A~A" *lasturl* (first lst)) (getURLhelperhelper (rest lst) visited)))))
	(t (getURLhelperhelper (rest lst) visited))))


; Bruker Drakma til aa hente ned HTML gjennom et HTTP-request
; Feilhaandteringen bestaar i aa ignorere feil, siden feilene foraarsakes av uriktige lenker eller feil paa serveren som spoerres
(defun getHTML (input)
  (cond ((equal (subseq input 0 7) "http://")
	 ; feilhaandtering
	 (handler-case 
	     ; Drakma-kall (HTTP-REQUEST)
	     (drakma:http-request input)
	   ; catcher diverse feilmeldinger
	   (DRAKMA::DRAKMA-SIMPLE-ERROR (se) (format t "-> Could not crawl (Error: ~A)~%" se)) 
	   (USOCKET:TIMEOUT-ERROR (se) (format t "-> Could not crawl (Error: ~A)~%" se))
	   (PURI:URI-PARSE-ERROR (se) (format t "-> Could not crawl (Error: ~A)~%" se))
	   (SB-INT:SIMPLE-STREAM-ERROR (se) (format t "-> Could not crawl (Error: ~A)~%" se))
	   (SB-KERNEL::UNDEFINED-ALIEN-FUNCTION-ERROR (se) (format t "-> Could not crawl (Error: ~A)~%" se))
	   (FLEXI-STREAMS:EXTERNAL-FORMAT-ENCODING-ERROR (se) (format t "-> Could not crawl (Error: ~A)~%" se))))
	(t "")))

; utskrift
; file: filen som skal skrives til
; str: tekststreng som skal skrives
(defun writeToFile (file str)
  (with-open-file (stream  file
			   :direction :output
			   :if-exists :append
			   :if-does-not-exist :create )
    (format stream "~A ~%" str))
  )

; --- feltvariabel ---
; jeg bruker en defparameter for aa mellomlagre HTML, slik at jeg unngar flere http-requests/filaapninger
(defparameter *HTML* "")

; --- Crawleren ---
; utformet som et standard uinformert soek (bredde-foerst/dybde-foerst)
; tar folgende argumenter:
; fringe: usokte urler
; visited: liste over besokte urler

; keys:
; take: hva skal crawleren returnere når den er ferdig (hvis den ikke defineres eksplisitt, så leverer crawleren en liste over sider den har crawlet)
; test: ligger siden i visited
; test2: test for skriving, hva skal skrives ut
; log: hvordan skal ting undersokes
; next: hva skal legges til i fringe
; combine: hvordan skal ting legges til fringe (breadth-first, depth-first)
(defun crawler (fringe visited &key test log next combine (take #'identity) test2)

  (cond ((null fringe) (print "Crawl finished"))

	((funcall test (first fringe) visited)
	 (format t "Crawling: ~A~%" (first fringe))
	 (defparameter *HTML* (getHTML (first fringe)))
	 
	 (if (funcall test2 *HTML*)
	     (funcall log (first fringe) (funcall next *HTML* (list nil)))
	 )

	 (funcall take (cons (first fringe)
			     (crawler (funcall combine (rest fringe) (funcall next *HTML* visited)) 
				      (cons (first fringe) visited)
				      :test test
				      :test2 test2
				      :next next
				      :combine combine
				      :log log
				      :take take))))

	(t 
	 (crawler (rest fringe) visited
		    :test test
		    :test2 test2
		    :next next
		    :combine combine
		    :log log
		    :take take)
	)))
