# AVCapture wraper for Chicken Scheme

This wraps AVCapture and exposes it to Chicken.

## Taking a picture:

```scheme
(use avcapture)

; Helper function to read bytes from a port to a string
(define (read-all-bytes port)
  (with-output-to-string (lambda ()
    (let loop ((x (read-byte port)))
      (if (not (eof-object? x))
        (begin
          (write-byte x)
          (loop (read-byte port)))
        (close-input-port port))))))

; Find a device that supports video
(define device (find device-has-video? (devices)))

; Take a photo and print it out
(print (capture-photo-from-device (lambda (capture-photo)
                             (read-all-bytes (capture-photo)))
                           (find device-has-video? (devices))))
```

## Caveats

Taking a photo is an asynchronous operation.  You *must* read all bytes from the
port returned by the `capture-photo` procedure before returning from the thunk,
otherwise the photo session will be closed and your photo will be lost.
