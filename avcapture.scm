;;;; av_capture.scm
;;;; Bindings to AVCapture

(module avcapture
  (devices
   device-name
   avmedia-type-video)

(import scheme chicken foreign)

(define-record-type avcapture-device
  (wrap-device device)
  device?
  (device unwrap-device))

(define (device-name device) (get-device-name (unwrap-device device)))

(define (devices)
  (let* ((devs (get-devs))
         (devlen (dev-len devs)))
    (let loop ((idx 0)
               (seed '()))
      (if (= idx devlen)
          seed
          (loop (+ idx 1) (cons (wrap-device (obj-at-idx devs idx)) seed))))))

(define avmedia-type-video (get-avmedia-type-video))

;; Native bits

(foreign-declare "#import <AVFoundation/AVFoundation.h>")

(define-foreign-type AVCaptureDevice (c-pointer "AVCaptureDevice"))
(define-foreign-type NSArray (c-pointer "NSArray"))

(define get-device-name (foreign-lambda* c-string
  ((AVCaptureDevice dev))
  "C_return([[dev localizedName] UTF8String]);"))

(define get-devs (foreign-safe-lambda* c-pointer ()
                                       "C_return([AVCaptureDevice devices]);"))

(define dev-len (foreign-safe-lambda* int ((c-pointer ary))
                                       "C_return([ary count]);"))

(define obj-at-idx (foreign-safe-lambda* AVCaptureDevice
                                         ((c-pointer ary)
                                          (int idx))
                                       "C_return([ary objectAtIndex: idx]);"))

(define get-avmedia-type-video (foreign-safe-lambda* c-string ()
                                "C_return([AVMediaTypeVideo UTF8String]);"))
)
