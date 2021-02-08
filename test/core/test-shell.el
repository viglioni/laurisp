;; -*- lexical-binding: t -*-
(load "./test/l-test-helpers.el")
(load-test-file "l-shell")

(test-suite "#lpwd"
  (before-each (spy-on 'expand-file-name))
  (context "dir is passed"
    (it-should "return its path"
      (lpwd "some dir")
      (expect 'expand-file-name :to-have-been-called-with "some dir")))
  (context "dir is not passed"
    (it-should "return . path"
      (lpwd)
      (expect 'expand-file-name :to-have-been-called-with "."))))


(test-suite "#ls"
  (before-each
    (spy-on 'directory-files))
  (context "a valid dir is passed"
    (before-each (spy-on 'file-directory-p :and-return-value t))
    (it-should "return its path"
      (ls "some dir")
      (expect 'directory-files :to-have-been-called-with "some dir")))
  (context "an invalid dir is passed"
    (before-each (spy-on 'file-directory-p :and-return-value nil))
    (it-should "return its path"
      (expect (ls "some dir") :to-throw)))
  (context "dir is not passed"
    (it-should "return . path"
      (ls)
      (expect 'directory-files :to-have-been-called-with "."))))

(test-suite "#touch"
  (context "filename is nil"
    (it-should "throw error"
      (expect (touch) :to-throw)))
  (context "file exists"
    (before-each
      (spy-on 'print)
      (spy-on 'file-exists-p :and-return-value t))
    (it-should "print message and return nil"
      (expect (touch "existent-file") :to-be nil)
      (expect 'print :to-have-been-called-with "file already exists")))
  (context "file does not exist"
    :var ((expected "path/file"))
    (before-each
      (spy-on 'lpwd :and-return-value "path/")
      (spy-on 'write-region)
      (spy-on 'file-exists-p :and-return-value nil))
    (it-should "create file and return filepath"
      (expect (touch "file") :to-equal expected)
      (expect 'write-region :to-have-been-called-with "" "" expected))))

(test-suite "#echo-into"
  (context "filename or text is nil"
    (it-should "throw error"
      (expect (filename) :to-throw)
      (expect (filename nil "text") :to-throw)
      (expect (filename "file" nil) :to-throw)))
(context "file does not exist"
  (before-each (spy-on 'file-exists-p :and-return-value nil))
  (it-should "throw"
    (expect (echo-into) :to-throw )))
(context "file exists"
  :var ((filename "filename")
        (text "text"))
  (before-each (spy-on 'file-exists-p :and-return-value t)
               (spy-on 'write-region))
  (it-should "write on file and return t"
    (expect (echo-into filename text) :to-be t )
    (expect 'write-region :to-have-been-called-with text "" filename))))

(test-suite "#count-non-empty-lines"
  (context "file is nil"
    (it-should "throw error"
      (expect (count-non-empty-lines) :to-throw )))
  (context "file exists"
    (before-each
      (spy-on 'get-string-from-file
              :and-return-value "line1\n\nline2\nline3\n    \nline4\n\n\n\n"))
    (it-should "return the correct number of non-empty lines"
      (expect (count-non-empty-lines "file") :to-be 4))))

(test-suite "#count-all-laurisp-lines"
  :var ((files-regexp (rx (| (and line-start
                                  (| "l" "test")
                                  (+ (any "-" letter))
                                  ".el"
                                  line-end)
                             (and line-start
                                  (+ (any "-" letter))
                                  ".snippet"
                                  line-end)))))
  (before-each  (spy-on 'directory-files-recursively :and-return-value '(1 2 3 4))
                (spy-on 'count-non-empty-lines :and-call-fake 'identity))
  (it-should "count the number of non-empty line of this project"
    (expect (count-all-laurisp-lines) :to-be 10)
    (expect 'directory-files-recursively
            :to-have-been-called-with "~/laurisp" files-regexp t)))
