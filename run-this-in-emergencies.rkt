#! /usr/bin/env racket

#lang racket
(require net/url
         racket/match)

; k so like this is the html page. I just put it all in text. A big string.
(define toldpage
  (port->string
   (get-pure-port (string->url "http://www.reddit.com/r/YouGotTold/"))))

; this is the part where I find the Toldler's List and itemize it
(define alltoldanddone
  (string-split
   (first (string-split
           (second (string-split toldpage "Fifty Shades of Told</strong>"))
           "Please address any"))
   "</p>\n\n<p>"))

; the last item has HTML tags that end the list starting with </p>,
; so I get rid of those tags
(define (remove-last-par lst)
  (cond
    [(empty? (rest lst))
     (cons (first (string-split (first lst) "</p>"))
           empty)]
    [else
     (cons (first lst) (remove-last-par (rest lst)))]))

(define just-told-no-tags
  (remove-last-par alltoldanddone))

; thanks regexes. Here we collapse <a href="url">url</a> into url
; we also get rid of the checkboxes in front of them
(define just-told-no-tags-no-urls-no-boxes
  (map (lambda (lst)
         (regexp-replace #rx"\\[.*\\] "
                         (regexp-replace #rx"\">.*"
                                         (regexp-replace #rx"<a href=\"" lst "")
                                         "")
                         ""))
       just-told-no-tags))

; randomly select an element, return it, and we're done bye thanks for watching
; see you next time it was fun having you
(define did-you-get-told
  (list-ref just-told-no-tags-no-urls-no-boxes
            (random (length just-told-no-tags-no-urls-no-boxes))))

(cond
  [(equal? did-you-get-told
           "Not Told")
   "CONGRATULATIONS YOU DIDN'T GET TOLD GO BUY YOURSELF AN ICE CREAM"]
  [else
      did-you-get-told])
