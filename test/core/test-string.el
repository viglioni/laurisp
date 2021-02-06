;; -*- lexical-binding: t -*-
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

(test-suit "#regex-matches"
  (context "there are several matches in string"
    (it-should "return all of them in the correct order"
      (let ((string "match1 asdasdasda match2 adasdas match3 asd")
            (regexp "match[0-9]"))
        (expect (regex-matches regexp string)
                :to-equal '("match1" "match2" "match3")))))
  (context "there are no matchs"
    (it-should "return nil"
      (expect (regex-matches "nomatch" "asdasdasdas")
              :to-be nil)))
  (context "one of the params is nil"
    (it-should "throw error"
      (expect (regex-matches "regexp" nil) :to-throw)
      (expect (regex-matches nil "string") :to-throw)
      (expect (regex-matches nil nil) :to-throw))))

(test-suit "#get-string-from-file"
  (context "param is nil or do not exist"
    (it-should "throw error"
      (expect (get-string-from-file nil) :to-throw)
      (expect (get-string-from-file "invalid filepath") :to-throw)))
  (context "param is a valid filepath"
    (before-each
      (spy-on 'file-exists-p :and-return-value t)
      (spy-on 'insert-file-contents
              :and-call-fake (lambda (path) (insert "string"))))
    (it-should "return file's content"
      (expect (get-string-from-file "filepath")
              :to-equal "string"))))

(test-suit "#remove-suffix"
  (context "param is nil"
    (it-should "throw error"
      (expect (remove-suffix) :to-throw )))
  (context "param has suffix"
    (it-should "remove suffix"
      (expect (remove-suffix "file.lau") :to-equal "file")))
  (context "param has no suffix"
    (it-should "return param"
      (expect (remove-suffix "file") :to-equal "file"))))

(test-suit "#insert-on-fst-empty-line"
  (context "any param is nil"
    (it-should "throw error"
      (expect (insert-on-fst-empty-line) :to-throw )
      (expect (insert-on-fst-empty-line "text" nil) :to-throw )
      (expect (insert-on-fst-empty-line nil 0) :to-throw )))
  (context "parameters are valid"
    :var* ((buffer-text "first line\n\nthird line")
           (text "i will be there")
           (point-after-empty-line 15)
           (point-before-empty-line 2)
           (expected-text "first line\ni will be there\nthird line")
           (expected-after-point (+ point-after-empty-line (length text)))
           test-on-temp-buffer)
    (before-each
      (fset 'test-on-temp-buffer (lambda (initial-point)
                                   (with-temp-buffer
                                     (insert buffer-text)
                                     (goto-char initial-point)
                                     (insert-on-fst-empty-line text (point))
                                     (list (point) (buffer-string))))))
    (context "cursor is after the empty line"
      (it-should "insert text and put the cursor in the correct place"
        (expect (test-on-temp-buffer point-after-empty-line)
                :to-equal (list expected-after-point expected-text))))
    (context "cursor is before the empty line"
      (it-should "insert text without changing the cursor point"
        (expect (test-on-temp-buffer point-before-empty-line)
                :to-equal (list point-before-empty-line expected-text))))))




