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

; Helper for only grabbing visible text layers, optionally
(define (want_text_info id visible)
  (if (= visible 1)
    (if (= (car (gimp-item-get-visible id)) TRUE)
      #t
      #f
    )
    #t
  )
)

; Extract all text data we can from a layer AND its children if applicable
(define (recurse_extract_text id stream visible)
  (cond 
    ((= (car (gimp-item-is-text-layer id)) TRUE)       ; is it a text layer?
      (if (want_text_info id visible)
	(begin
; this is tricky: either text layers are unchanged and they yield non-empty text
; or they have markup, and they yield markup text
; (we don't even try to clean that up)
	  (display (car (gimp-text-layer-get-text id)) stream)
	  (display (car (gimp-text-layer-get-markup id)) stream)
	  (newline stream)
	)
      )
    )
    ((= (car (gimp-item-is-group id)) TRUE)
      (for-each (lambda (id)
	  (recurse_extract_text id stream visible)
	)
	(vector->list (cadr (gimp-item-get-children id)))
      )
    )
  )
)

; Extract text from an Image
(define (extract_text_from_image image stream visible)
  ; output the file name
  (display (car (gimp-image-get-filename image)) stream)
  (newline stream)
  (for-each (lambda (id)
      (recurse_extract_text id stream visible)
    )
    (vector->list (cadr (gimp-image-get-layers image)))
  )
  (newline stream)
)


; Extract text from a single file
(define (extract_text_from_file filename stream visible)
  (let (
      ; load the file  
      (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
    )
    (extract_text_from_image image stream visible)
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

(define (extract_text image_filename text_filename visible)
  (with_file (lambda (stream)
      (extract_text_from_file image_filename stream visible)
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

(define (extract_text_batch dir pattern text_filename visible)
  (with_file (lambda (stream)
	  (for-each (lambda (image_filename)
	      (extract_text_from_file image_filename stream visible)
	    )
	    (cadr (file-glob (compose dir pattern) 1))
	  )
	)
    text_filename
  )
  (gimp-message "Finished")
)

(define (extract_text_current_image image text_filename visible)
  (with_file (lambda (stream)
	  (extract_text_from_image image stream visible)
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
)

(script-fu-menu-register "extract_text"
	_"<Image>/Tools")

(script-fu-menu-register "extract_text_batch"
	_"<Image>/Tools")

(script-fu-menu-register "extract_text_current_image"
	_"<Image>/Tools")
