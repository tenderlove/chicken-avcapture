;;;; av_capture.scm
;;;; Bindings to AVCapture

(module avcapture
  (devices
   device-name)

(import scheme chicken foreign)

(define-record-type avcapture-device
  (wrap-device device)
  device?
  (device unwrap-device))

(define (device-name device) (get-device-name (unwrap-device device)))

(define (devices) (map wrap-device (get_devices '())))

;; Native bits

(foreign-declare "#import <AVFoundation/AVFoundation.h>")

(define-foreign-type AVCaptureDevice (c-pointer "AVCaptureDevice"))
(define-foreign-type NSArray (c-pointer "NSArray"))

(define get-device-name (foreign-lambda* c-string
  ((AVCaptureDevice dev))
  "C_return([[dev localizedName] UTF8String]);"))

(define get_devices (foreign-safe-lambda* scheme-object
                                          ((scheme-object seed))
"
NSArray * list = [AVCaptureDevice devices];
for (id object in list) {
  C_word * _pair = C_alloc(C_SIZEOF_PAIR);
  C_word * a = C_alloc(C_SIZEOF_POINTER);
  C_word ptr = C_mpointer(&a, object);
  seed = C_pair(&_pair, ptr, seed);
}
C_return(seed);
"))

)
