(load "./test/l-test-helpers.el")
(load-test-file "l-string")

(test-suit "#join-path"
  (context "path ends in /"
    (it-should "should join without a bar between path and filename"
      (expect (join-path "/path/to/" "file.lau")
              :to-equal "/path/to/file.lau")))
  (context "path ends in other thing than a /"
    (it-should "add a bar between path and filename"
      (expect (join-path "/path/to" "file.lau")
              :to-equal "/path/to/file.lau")))
  (context "path or filename is nil"
    (it-should "throw error"
      (expect (join-path nil "file.lau")
              :to-throw)
      (expect (join-path "path/to" nil)
              :to-throw)
      (expect (join-path nil nil)
              :to-throw))))

(test-suit "#file-extension"
  (context "filename or extension is nil"
    (it-should "throw error"
      (expect (file-extension nil "lau")
              :to-throw)
      (expect (file-extension "file" nil)
              :to-throw)
      (expect (file-extension nil nil)
              :to-throw)))
  (context "filename and extension exists"
    (it-should "concatenate filename with extension"
      (expect (file-extension "file" "lau") :to-equal "file.lau"))))
