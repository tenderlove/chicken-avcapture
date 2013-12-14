(use avcapture test srfi-1)

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
    (test-assert (video-connection? (video-connection output)))))

(test-end)
(test-exit)
