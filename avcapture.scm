;;;; av_capture.scm
;;;; Bindings to AVCapture

(module avcapture
  (devices
   device-name
   device-has-media-type?
   device-has-video?
   make-device-input
   make-stillimage-output
   make-session
   session-add-input!
   session-add-output!
   session-start-running!
   session-stop-running!
   video-connection?
   video-connection
   capture-photo-from-device
   capture-photo
   avmedia-type-video)

(import scheme chicken foreign posix)
(use posix)

(define-record-type avcapture-session
  (wrap-session session)
  session?
  (session unwrap-session))

(define-record-type avcapture-videoconn
  (wrap-video-connection conn)
  video-connection?
  (conn unwrap-video-connection))

(define-record-type avcapture-device
  (wrap-device device)
  device?
  (device unwrap-device))

(define-record-type avcapture-device-input
  (wrap-device-input input)
  device-input?
  (input unwrap-device-input))

(define-record-type avcapture-stillimage-output
  (wrap-stillimage-output output)
  stillimage-output?
  (output unwrap-stillimage-output))

(define (device-has-video? dev) (device-has-media-type? dev avmedia-type-video))
(define (device-has-media-type? device media-type)
  (_device-has-media-type? (unwrap-device device) media-type))

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

(define (make-device-input device)
  (wrap-device-input (_make-device-input (unwrap-device device))))

(define (make-stillimage-output)
  (wrap-stillimage-output (_make-stillimage-output)))

(define (make-session) (wrap-session (_make-session)))

(define (session-add-input! input session)
  (_session-add-input! (unwrap-device-input input)
                       (unwrap-session session))
  session)

(define (session-add-output! output session)
  (_session-add-output! (unwrap-stillimage-output output)
                       (unwrap-session session))
  session)

(define (session-start-running! session)
  (_session-start-running! (unwrap-session session))
  session)

(define (session-stop-running! session)
  (_session-stop-running! (unwrap-session session))
  session)

(define (video-connection output) (connect-output avmedia-type-video output))

(define (connect-output media-type output)
  (let ((conn (_connect-output media-type (unwrap-stillimage-output output))))
    (if conn (wrap-video-connection conn) #f)))

(define (capture-photo connection output)
  (let ((conn (unwrap-video-connection connection))
        (out (unwrap-stillimage-output output)))
    (let-values (((in-fd out-fd) (create-pipe)))
      (_capture-photo conn out out-fd)
      (open-input-file* in-fd))))

(define (capture-photo-from-device thunk device)
  (let ((session (make-session))
        (output (make-stillimage-output)))
    (session-add-input! (make-device-input device) session)
    (session-add-output! output session)
    (session-start-running! session)
    (let ((conn (video-connection output)))
      (thunk (lambda () (capture-photo conn output))))))

;; Native bits

(foreign-declare "#import <AVFoundation/AVFoundation.h>")

(define-foreign-type AVCaptureStillImageOutput (c-pointer "AVCaptureStillImageOutput"))
(define-foreign-type AVCaptureDevice (c-pointer "AVCaptureDevice"))
(define-foreign-type AVCaptureDeviceInput (c-pointer "AVCaptureDeviceInput"))
(define-foreign-type AVCaptureSession (c-pointer "AVCaptureSession"))
(define-foreign-type AVCaptureConnection (c-pointer "AVCaptureConnection"))
(define-foreign-type NSArray (c-pointer "NSArray"))

(define _capture-photo (foreign-lambda* int
                                        ((AVCaptureConnection connection)
                                         (AVCaptureStillImageOutput output)
                                         (int wrt))
"
  [output captureStillImageAsynchronouslyFromConnection: connection
          completionHandler:^(CMSampleBufferRef imageDataSampleBuffer, NSError *error) {
            if (imageDataSampleBuffer != NULL) {
              NSData *imageData = [AVCaptureStillImageOutput jpegStillImageNSDataRepresentation:imageDataSampleBuffer];
              write(wrt, [imageData bytes], [imageData length]);
              close(wrt);
            } else {
            }
          }];
  C_return(wrt);
"))
(define _connect-output (foreign-lambda* AVCaptureConnection
                                         ((c-string media_type)
                                          (AVCaptureConnection capture))
"
  NSString * mt = [NSString stringWithCString: media_type
                            encoding: NSUTF8StringEncoding];

  AVCaptureConnection * conn = [capture connectionWithMediaType:mt];

  if (conn) {
    C_return(conn);
  } else {
    C_return(NULL);
  }
"))
(define _make-session (foreign-safe-lambda* AVCaptureSession ()
"
  C_return([[AVCaptureSession alloc] init]);
"))

(define _session-add-input! (foreign-lambda* AVCaptureSession
                                             ((AVCaptureDeviceInput input)
                                              (AVCaptureSession session))
"
[session addInput:input];
C_return(session);
"))

(define _session-add-output! (foreign-lambda* AVCaptureSession
                                             ((AVCaptureStillImageOutput output)
                                              (AVCaptureSession session))
"
[session addOutput:output];
C_return(session);
"))

(define _session-start-running! (foreign-lambda* AVCaptureSession
                                             ((AVCaptureSession session))
"
[session startRunning];
C_return(session);
"))

(define _session-stop-running! (foreign-lambda* AVCaptureSession
                                             ((AVCaptureSession session))
"
[session stopRunning];
C_return(session);
"))

(define _make-stillimage-output (foreign-safe-lambda* AVCaptureStillImageOutput
                                                      ()
"
  AVCaptureStillImageOutput *output = [[AVCaptureStillImageOutput alloc] init];
  NSDictionary *outputSettings = [[NSDictionary alloc] initWithObjectsAndKeys:
                                    AVVideoCodecJPEG, AVVideoCodecKey,
                                    nil];
  [output setOutputSettings:outputSettings];
  [outputSettings release];
  C_return(output);
"))

(define _make-device-input (foreign-safe-lambda* AVCaptureDeviceInput
                                                 ((AVCaptureDevice dev))
"
AVCaptureDeviceInput *input = [AVCaptureDeviceInput alloc];
[input initWithDevice:dev error: nil];
C_return(input);
"))

(define _device-has-media-type? (foreign-safe-lambda* scheme-object
                                                      ((AVCaptureDevice dev)
                                                       (c-string str))
"
NSString * mt = [NSString stringWithCString: str
                          encoding: NSUTF8StringEncoding];

if ([dev hasMediaType: mt]) {
  C_return(C_SCHEME_TRUE);
} else {
  C_return(C_SCHEME_FALSE);
}
"))

(define get-device-name (foreign-lambda* c-string
  ((AVCaptureDevice dev))
  "C_return([[dev localizedName] UTF8String]);"))

(define get-devs (foreign-safe-lambda* c-pointer ()
                                       "C_return([AVCaptureDevice devices]);"))

(define dev-len (foreign-safe-lambda* int ((c-pointer ary))
                                       "C_return([((id)ary) count]);"))

(define obj-at-idx (foreign-safe-lambda* AVCaptureDevice
                                         ((c-pointer ary)
                                          (int idx))
                                       "C_return([((id)ary) objectAtIndex: idx]);"))

(define get-avmedia-type-video (foreign-safe-lambda* c-string ()
                                "C_return([AVMediaTypeVideo UTF8String]);"))
)
