(use avcapture test)

(test-begin "avcapture")

(test-group "devices"
  (test-assert avmedia-type-video)
  (let ((devs (devices)))
    (test-assert (< 0 (length devs)))
    (test-assert (map device-name devs)))
)

(test-end)
(test-exit)
