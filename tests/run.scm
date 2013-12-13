(use avcapture test)

(test-begin "avcapture")

(test-group "devices"
  (let ((devs (devices)))
    (test-assert (< 0 (length devs)))
    (test-assert (map device-name devs)))
)

(test-end)
(test-exit)
