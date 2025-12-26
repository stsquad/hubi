;;; hubi-ert.el --- Tests for hubi -*- lexical-binding: t -*-

(require 'ert)
(require 'hubi)

(ert-deftest hubi-test-string-contains-p ()
  (should (hubi--string-contains-p "make" "make -j8"))
  (should (hubi--string-contains-p "meson" "meson compile"))
  (should (hubi--string-contains-p "compile" "meson compile"))
  (should (hubi--string-contains-p "test" "meson test --suite unit"))
  (should (not (hubi--string-contains-p "make" "ninja"))))

(ert-deftest hubi-test-format-compile-make ()
  (let ((hubi-make-args "-j4 "))
    (should (string-match-p "make -j4  -C /tmp all "
                          (hubi--format-compile "make" "/tmp" "all" nil)))
    (should (string-match-p "make -j4  -C /tmp test "
                          (hubi--format-compile "make" "/tmp" "test" nil)))
    (should (string-match-p "make -j4  -C /tmp all FOO=BAR"
                          (hubi--format-compile "make" "/tmp" "all" '("FOO=BAR"))))))

(ert-deftest hubi-test-format-compile-ninja ()
  (should (equal "ninja -C /tmp all"
                 (hubi--format-compile "ninja" "/tmp" "all" nil))))

(ert-deftest hubi-test-format-compile-meson ()
  (should (equal "cd /tmp && meson compile target"
                 (hubi--format-compile "meson compile" "/tmp" "target" nil)))
  (should (equal "cd /tmp && meson test --suite unit"
                 (hubi--format-compile "meson test" "/tmp" "unit" nil))))

(ert-deftest hubi-test-format-compile-cargo ()
  (should (equal "cargo build"
                 (hubi--format-compile "cargo build" "/tmp" "" nil)))
  (should (equal "cargo build -p mypkg"
                 (hubi--format-compile "cargo build" "/tmp" "mypkg" nil)))
  (should (equal "env FOO=BAR cargo build -p mypkg"
                 (hubi--format-compile "cargo build" "/tmp" "mypkg" '("FOO=BAR")))))

(defmacro hubi-with-temp-dir (dir-var &rest body)
  "Create a temporary directory, bind it to DIR-VAR, and run BODY.
The directory is deleted after BODY finishes."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,dir-var (make-temp-file "hubi-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir-var t))))

(ert-deftest hubi-test-find-build-subdir ()
  (hubi-with-temp-dir temp-dir
    (let ((build-dir (expand-file-name "build" temp-dir)))
      (make-directory build-dir)
      (should (equal build-dir (hubi--find-build-subdir temp-dir))))))

(ert-deftest hubi-test-get-build-subdirs ()
  (hubi-with-temp-dir temp-dir
    (let ((b1 (expand-file-name "b1" temp-dir))
          (b2 (expand-file-name "b2" temp-dir)))
      (make-directory b1)
      (make-directory b2)
      ;; We can't easily control mtime here without sleep or mocking,
      ;; but we can at least check if both are returned.
      (let ((subdirs (hubi--get-build-subdirs temp-dir)))
        (should (member b1 subdirs))
        (should (member b2 subdirs))
        (should (= 2 (length subdirs)))))))

(ert-deftest hubi-test-get-build-directories ()
  (hubi-with-temp-dir temp-dir
    (let ((build-dir (expand-file-name "build" temp-dir)))
      (make-directory build-dir)
      (let ((b1 (expand-file-name "b1" build-dir)))
        (make-directory b1)
        ;; mock hubi--compile-root to return our temp-dir
        (cl-letf (((symbol-function 'hubi--compile-root)
                   (lambda () temp-dir)))
          (let ((dirs (hubi-get-build-directories)))
            (should (member b1 dirs))
            (should (= 1 (length dirs)))))))))

(provide 'hubi-ert)
;;; hubi-ert.el ends here
