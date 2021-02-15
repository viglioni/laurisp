;; -*- lexical-binding: t -*-
(load "./test/l-test-helpers.el")
(load-test-file "l-tide-funcs")

;;
;; Import JS libs from package.json
;;

(test-suite "#laurascript--get-project-libs"
  (context "there is no package.json"
    (before-each (spy-on 'file-exists-p :and-return-value nil))
    (it-should "throw error"
      (expect (laurascript--get-project-libs) :to-throw )))
  (context "there is a package.json"
    :var ((pjson-content "{
                            \"author\": \"Laura Viglioni\",
                            \"license\": \"GPL-3.0\",
                            \"dependencies\": {
                               \"ramda\": \"^0.27.1\",
                              \"immutable\": \"^4.0.0-rc.12\"
                             },
                            \"devDependencies\": {
                              \"@types/axios\": \"^0.14.0\"
                            },
                            \"scripts\": {
                              \"test\": \"jest\"
                            },
                            \"_moduleAliases\": {
                              \"logic\": \"src/logic\"
                            }
                          }")
          (expected '("immutable" "ramda")))
    (before-each
      (spy-on 'file-exists-p :and-return-value t)
      (spy-on 'get-string-from-file :and-return-value pjson-content))
    (it-should "return a list with the sorted lib names in depedencies"
      (expect (laurascript--get-project-libs) :to-equal expected))))

(test-suite "#laurascript--insert-import"
  :var ((buffer-text "first line\n\nthird line\n\n\n")
        (expected "first line\nimport something from \"somelib\"\n\nthird line\n\n\n")
        test-on-buffer)
  (before-each
    (fset 'test-on-buffer (lambda (lib-name import-name)
                            (with-temp-buffer
                              (insert buffer-text)
                              (laurascript--insert-import lib-name import-name)
                              (buffer-string)))))
  (it-should "insert correct import on the first empty line"
    (expect (test-on-buffer "somelib"  "something") :to-equal expected)))


;; (test-suite "#laurascript--helm-import-lib"
;;   (it-should ""
;;     (with-temp-buffer
;;       (laurascript--helm-import-lib "lib-name")
;;       (switch-to-buffer "*helm buffer source*")
;;       (insert "import-name\n"))
;;     (expect t :to-be t)))
