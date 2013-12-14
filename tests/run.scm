(use avcapture test srfi-1)

(define (read-photo port)
  (with-output-to-string (lambda ()
    (let loop ((x (read-byte port)))
      (if (not (eof-object? x))
        (begin
          (write-byte x)
          (loop (read-byte port)))
        (close-input-port port))))))

(test-begin "avcapture")

(test-group "devices"
  (test-assert avmedia-type-video)
  (let ((devs (devices)))
    (test-assert (< 0 (length devs)))
    (test-assert (map device-name devs))
    (test-assert (< 0 (length (filter device-has-video? devs)))))
)

(test-group "device-input"
  (let ((devs (devices)))
    (test-assert (make-device-input (find device-has-video? devs)))))

(test-group "device-output"
  (test-assert (eq? #f (video-connection (make-stillimage-output))))
  (test-assert (make-stillimage-output)))

(test-group "session"
  (test-assert (make-session))
  (let ((session (make-session))
        (dev (find device-has-video? (devices)))
        (output (make-stillimage-output)))
    (session-add-input! (make-device-input dev) session)
    (session-add-output! output session)
    (test-assert (video-connection? (video-connection output))))

  (test-group "running"
    (let ((session (make-session))
          (dev (find device-has-video? (devices)))
          (output (make-stillimage-output)))
      (session-add-input! (make-device-input dev) session)
      (session-add-output! output session)
      (test-assert (session-start-running! session))
      (test-assert (session-stop-running! session))))

  (test-group "take a photo"
    (let ((session (make-session))
          (dev (find device-has-video? (devices)))
          (output (make-stillimage-output)))
      (session-add-input! (make-device-input dev) session)
      (session-add-output! output session)
      (test-assert (session-start-running! session))
      (test-assert (capture-photo (video-connection output) output))
      (test-assert (session-stop-running! session))))

  (test-group "high level"
    (let ((photo-bytes (capture-photo-from-device (lambda (capture-photo)
                                                    (read-photo (capture-photo)))
                                                  (find device-has-video? (devices)))))

      (test-assert photo-bytes))))

(test-end)
(test-exit)
