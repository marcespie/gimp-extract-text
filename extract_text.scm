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

(define (to_bool f p)
  (= (get f p) TRUE)
)

(define (item-is-visible id)
  (to_bool gimp-item-get-visible id)
)

(define (item-is-text-layer id)
  (to_bool gimp-item-is-text-layer id)
)

(define (item-is-group id)
  (to_bool gimp-item-is-group id)
)

(define (to_list f id)
  (vector->list (cadr (f id)))
)

(define (item-get-children id)
  (to_list gimp-item-get-children id)
)

(define (image-get-layers id)
  (to_list gimp-image-get-layers id)
)

; Helper for only grabbing visible text layers, optionally
(define (want_text_info id visible)
  (if (= visible 1)
    (if (item-is-visible id)
      #t
      #f
    )
    #t
  )
)

; Extract all text data we can from a layer AND its children if applicable
(define (recurse_extract_text id stream visible extra)
  (cond 
    ((item-is-text-layer id)       ; is it a text layer?
      (if (want_text_info id visible)
	(begin
; this is tricky: either text layers are unchanged and they yield non-empty text
; or they have markup, and they yield markup text
; (we don't even try to clean that up)
	  (if (= extra 1)
	    (let (
	      (coords (gimp-drawable-offsets id))
	      )
	      (display (string-append
		"visible="
		(if (item-is-visible id) "yes" "no")
		" x="
		(number->string (car coords))
		" y="
		(number->string (cadr coords))
		": ") stream)
	    )
	  )
	  (display (car (gimp-text-layer-get-text id)) stream)
	  (display (car (gimp-text-layer-get-markup id)) stream)
	  (newline stream)
	)
      )
    )
    ((item-is-group id)
      (for-each (lambda (id)
	  (recurse_extract_text id stream visible extra)
	)
	(item-get-children id)
      )
    )
  )
)

; Extract text from an Image
(define (extract_text_from_image image stream visible extra)
  ; output the file name
  (if (= extra 1)
    (display "file=" stream)
  )
  (display (car (gimp-image-get-filename image)) stream)
  (newline stream)
  (for-each (lambda (id)
      (recurse_extract_text id stream visible extra)
    )
    (image-get-layers image)
  )
  (newline stream)
)


; Extract text from a single file
(define (extract_text_from_file filename stream visible extra)
  (let (
      ; load the file  
      (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
    )
    (extract_text_from_image image stream visible extra)
    (gimp-image-delete image)
  )
)

; helper to define the actual output-stream
(define (with_file p filename)
  ; XXX we need let so we can close the stream
  (let (
      (stream 0)
    )
    (set! stream (open-output-file filename))
    (p stream)
    (close-output-port stream) 
  )
)

(define (extract_text image_filename text_filename visible extra)
  (with_file (lambda (stream)
      (extract_text_from_file image_filename stream visible extra)
    )
    text_filename
  )
)

(define (compose dir pattern)
  (if (= (string-length dir) 0)
     pattern
     (string-append dir "/" pattern)
  )
)

(define (extract_text_batch dir pattern text_filename visible extra)
  (with_file (lambda (stream)
	  (for-each (lambda (image_filename)
	      (extract_text_from_file image_filename stream visible extra)
	    )
	    (cadr (file-glob (compose dir pattern) 1))
	  )
	)
    text_filename
  )
  (gimp-message "Finished")
)

(define (extract_text_current_image image text_filename visible extra)
  (with_file (lambda (stream)
	  (extract_text_from_image image stream visible extra)
       )
     text_filename
  )
)

(script-fu-register 
  "extract_text"
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
  "extract_text_batch"
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
  "extract_text_current_image"
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

(script-fu-menu-register "extract_text"
	_"<Image>/Tools")

(script-fu-menu-register "extract_text_batch"
	_"<Image>/Tools")

(script-fu-menu-register "extract_text_current_image"
	_"<Image>/Tools")
