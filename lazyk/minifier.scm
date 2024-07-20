(define minify
  (let ((lib-path (or (getenv "LK_MINIFIER_DYLIB_PATH")
                      "./minifier/target/release/liblazyk_minifier_chez_dylib.dylib")))
    (case lib-path
      (("")
        (display "\x1b;[93mlazier stdlib\x1b;[m: skipping loading the minifier library\n" (current-error-port))
        (lambda (s) s))
      (else
        (load-shared-object lib-path)
        (foreign-procedure "lazyk_minify" (string) string)))))

