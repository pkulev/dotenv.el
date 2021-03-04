(require 'ert)

(ert-deftest test-dotenv--assignment? ()
  "Tests for `dotenv--assignment?'."
  (should (equal (dotenv--assignment? "") nil))
  (should (equal (dotenv--assignment? "#") nil))
  (should (equal (dotenv--assignment? "# this is the comment = not assignment") nil))
  (should (equal (dotenv--assignment? "= seems like assigmnent") t))
  (should (equal (dotenv--assignment? "definetely = assigmnent") t)))

(ert-deftest test-dotenv-parse-line ()
  "Test for `dotenv-parse-line'."
  ;; Empty line is nil
  (should (equal (dotenv-parse-line "") nil))
  (should (equal (dotenv-parse-line "=WAT") nil))
  ;; Comments are ignored
  (should (equal (dotenv-parse-line "#") nil))
  (should (equal (dotenv-parse-line "# this is comment") nil))
  (should (equal (dotenv-parse-line "# KEY=VAL") nil))
  ;; Empty values are allowed
  (should (equal (dotenv-parse-line "KEY=") '("KEY" "")))
  ;; FIXME: quote escaping
  ;; (should (equal (dotenv-parse-line "KEY=\"\"") '("KEY" "")))
  (should (equal (dotenv-parse-line "KEY=VAL") '("KEY" "VAL"))))
  ;; FIXME: quote escaping
  ;; (should (equal (dotenv-parse-line "KEY=\"VAL\"") '("KEY" "VAL"))))

(ert-deftest test-dotenv-parse-file ()
  "Tests for `dotenv-parse-file'."
  (should (equal (dotenv-parse-file "test.env")
                 '(("VAR3" "test-val-3")
                   ("VAR4" "test-val-4")
                   ("VAR5" "test-val-5")
                   ("VAR6" "")))))
