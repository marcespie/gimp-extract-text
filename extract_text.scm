; ex:sw=2
; Copyright (c) 2023 Marc Espie <espie@openbsd.org>
; 
; Permission to use, copy, modify, and distribute this software for any
; purpose with or without fee is hereby granted, provided that the above
; copyright notice and this permission notice appear in all copies.
; 
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

; Helpers for a decent interface for gimp functions
(define (get f id)
  (car (f id))
)

(define (to-bool f p)
  (= (get f p) TRUE)
)

(define (item-visible? id)
  (to-bool gimp-item-get-visible id)
)

(define (item-text-layer? id)
  (to-bool gimp-item-is-text-layer id)
)

(define (item-group? id)
  (to-bool gimp-item-is-group id)
)

(define (to-list f id)
  (vector->list (cadr (f id)))
)

(define (item-get-children id)
  (to-list gimp-item-get-children id)
)

(define (image-get-layers id)
  (to-list gimp-image-get-layers id)
)

; Helper for only grabbing visible text layers, optionally
(define (want-text-info? id visible)
  (if (= visible 1)
    (if (item-visible? id) #t #f)
    #t))

; Extract all text data we can from a layer AND its children if applicable
(define (recurse-extract-text id stream visible extra)
  (cond 
    ((item-text-layer? id)       ; is it a text layer?
      (when (want-text-info? id visible)
	(begin
	  (when (= extra 1)
	    (let (
	      (coords (gimp-drawable-offsets id))
	      )
	      (display (string-append
	        "font="
		(car (gimp-text-layer-get-font id))
		" visible="
		(if (item-visible? id) "yes" "no")
		" x="
		(number->string (car coords))
		" y="
		(number->string (cadr coords))
		": ") stream)
	    )
	  )
; this is tricky: either text layers are unchanged and they yield non-empty text
; or they have markup, and they yield markup text
; (we don't even try to clean that up)
	  (display (car (gimp-text-layer-get-text id)) stream)
	  (display (car (gimp-text-layer-get-markup id)) stream)
	  (newline stream)
	)
      )
    )
    ((item-group? id)
      (for-each (lambda (id)
	  (recurse-extract-text id stream visible extra)
	)
	(item-get-children id)
      )
    )
  )
)

; Extract text from an Image
(define (extract-text-from-image image stream visible extra)
  ; output the file name
  (when (= extra 1)
    (display "file=" stream)
  )
  (display (car (gimp-image-get-filename image)) stream)
  (newline stream)
  (for-each (lambda (id)
      (recurse-extract-text id stream visible extra)
    )
    (image-get-layers image)
  )
  (newline stream)
)


; Extract text from a single file
(define (extract-text-from-file filename stream visible extra)
  (let (
      ; load the file  
      (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
    )
    (extract-text-from-image image stream visible extra)
    (gimp-image-delete image)
  )
)

; helper to define the actual output-stream
(define (with-file p filename)
  (let (
      (stream (open-output-file filename))
    )
    (if (output-port? stream)
      (begin
	(p stream)
	(close-output-port stream) 
	#t
      )
      (begin
	(gimp-message (string-append _"Error opening " filename))
	#f
      )
    )
  )
)

(define (extract-text image-filename text-filename visible extra)
  (with-file (lambda (stream)
      (extract-text-from-file image-filename stream visible extra)
    )
    text-filename
  )
)

(define (compose dir pattern)
  (if (= (string-length dir) 0)
     pattern
     (string-append dir "/" pattern)
  )
)

(define (extract-text-batch dir pattern text-filename visible extra)
  (when
    (with-file (lambda (stream)
	    (for-each (lambda (image-filename)
		(extract-text-from-file image-filename stream visible extra)
	      )
	      (cadr (file-glob (compose dir pattern) 1))
	    )
	  )
      text-filename
    )
    (gimp-message _"Finished")
  )
)

(define (xcf-to-txt s) 
  (string-append (substring s 0 (- (string-length s) 4)) ".txt")
)

(define (extract-text-current-image image text-filename visible extra)
  (with-file (lambda (stream)
	  (extract-text-from-image image stream visible extra)
       )
      (if (= (string-length text-filename) 0)
	 (xcf-to-txt (car (gimp-image-get-filename image)))
	 text-filename
      )
  )
)

(script-fu-register 
  "extract-text"
  _"Extract Text from file..."
  "Extract all text information from text layers \
  of a given image file"
  "Marc Espie"
  "BSD License"
  "2023 January 1st"
  ""
  SF-FILENAME	_"Image file name" ""
  SF-FILENAME	_"Output file name" ""
  SF-TOGGLE	_"Only visible layers" TRUE
  SF-TOGGLE	_"Extra annotations" FALSE
)

(script-fu-register 
  "extract-text-batch"
  _"Extract Text from files..."
  "Extract all text information from text layers \
  of a bunch of image files"
  "Marc Espie"
  "BSD License"
  "2023 January 1st"
  ""
  SF-DIRNAME	_"Image directory" ""
  SF-STRING	_"File pattern" "*.xcf"
  SF-FILENAME	_"Output file name" ""
  SF-TOGGLE	_"Only visible layers" TRUE
  SF-TOGGLE	_"Extra annotations" FALSE
)

(script-fu-register 
  "extract-text-current-image"
  _"Extract Text from current image..."
  "Extract all text information from the current active image"
  "Marc Espie"
  "BSD License"
  "2023 January 1st"
  ""
  SF-IMAGE	_"The image" 0
  SF-FILENAME	_"Output file name" ""
  SF-TOGGLE	_"Only visible layers" TRUE
  SF-TOGGLE	_"Extra annotations" FALSE
)

(script-fu-menu-register "extract-text"
	_"<Image>/Tools")

(script-fu-menu-register "extract-text-batch"
	_"<Image>/Tools")

(script-fu-menu-register "extract-text-current-image"
	_"<Image>/Tools")
