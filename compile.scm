#!/usr/bin/env -S scheme --script

(define is-atleast-10
  (let-values (((major minor point) (scheme-version-number)))
    (<= 10 major)))

(if (not is-atleast-10)
  (eval `(define (path-build a b)
           (string-append a "/" b))))

(define (list-split sep l)
  (define (go split remaining)
    (cond
      [(null? remaining) (list (reverse split))]
      [(equal? sep (car remaining)) (cons (reverse split) (go '() (cdr remaining)))]
      [else (go (cons (car remaining) split) (cdr remaining))]))
  (go '() l))

(define (string-split sep s)
  (map list->string (list-split sep (string->list s))))

(define (or-default default thunk)
  (guard (ex [(error? ex) (default)]) (thunk)))

(define (whichever-works . thunks)
  (if (null? thunks)
    (error 'whichever-works "Expected at least one thunk"))
  (if (null? (cdr thunks))
        ((car thunks)))
  (or-default
    (lambda ()
      (apply whichever-works (cdr thunks)))
    (car thunks)))

(define (whatever-file-exists . files)
  (if (null? files)
    (error 'whatever-file-exists "expected at least on file path"))
  (call/cc (lambda (return)
             (for-each (lambda (file)
                         (if (file-exists? file)
                           (return file)))
                       files)
             (error 'whatever-file-exists
                    (with-output-to-string (lambda ()
                                             (display "None of these files found: ")
                                             (display files)))))))
(define cc
  (let ((cc (getenv "CC")))
    (if (not cc) "gcc" cc)))

(define foreign-symbols
  (let ((foreign-symbols (getenv "FOREIGN_SYMBOLS")))
    (filter (lambda (s) (< 0 (string-length s)))
            (string-split #\, (if (not foreign-symbols) "" foreign-symbols)))))

(define scheme-dirs
  (let ((scheme-dirs (getenv "SCHEME_DIRS")))
    (if (not scheme-dirs)
      (begin
          (display "SCHEME_DIRS environment variable missing\n") (current-error-port)
          (exit)))
    (string-split #\: scheme-dirs)))

(define (lookup-in-scheme-dirs file)
  (apply whatever-file-exists
         (map
           (lambda (dir) (path-build dir file))
           scheme-dirs)))

(define clean (member "--clean" (command-line-arguments)))
(command-line-arguments
  (filter
    (lambda (arg) (not (equal? "--clean" arg)))
    (command-line-arguments)))

(if (< (length (command-line-arguments)) 1)
  (begin
    (display "Missing arguments, expected:\n")
    (display "  ./compile.ss <source-file>")
    (exit 1)))

(define source-file (car (command-line-arguments)))

(define source-file-root (path-root source-file))

(define scheme-header-file (lookup-in-scheme-dirs "scheme.h"))

(define (run cmd)
  (let-values (((stdin stdout stderr pid) (open-process-ports
                                            cmd
                                            (buffer-mode block)
                                            (make-transcoder (utf-8-codec)))))
    (get-string-all stdout)))

(define (run-and-log cmd)
  (display "+ ") (display cmd) (newline)
  (system cmd))

(define (temp-directory)
  (list->string (filter (lambda (c) (not (char-whitespace? c))) (string->list (run "mktemp -d")))))

(define (with-temp-directories body)
  (let* ((tempdirectories '())
        (new (lambda ()
               (let ((path (temp-directory)))
                 (set! tempdirectories (cons path tempdirectories))
                 path))))
    (dynamic-wind
      void
      (lambda () (body new))
      (lambda ()
        (for-each
          (lambda (d)
            (for-each (lambda (f) (delete-file (path-build d f) #t))
                      (directory-list d))
            (delete-directory d #t))
          tempdirectories)))))

(define name-of-embedded-code "chezbootfile")

(define embedding-code
  (apply string-append
    (map (lambda (s) (string-append s "\n"))
         (list
"#include <assert.h>"
"#include <fcntl.h>"
"#include <errno.h>"
"#include <stdio.h>"
"#include <stdlib.h>"
"#include <string.h>"
"#include <unistd.h>"

"#include <scheme.h>"

(string-append "extern const char " name-of-embedded-code ";")
(string-append "extern const unsigned " name-of-embedded-code "_size;")
"extern const char scheme_program;"
"extern const unsigned scheme_program_size;"
"extern void register_symbols();"

(if is-atleast-10
  ""
  "char bootfilename[] = \"/tmp/bootfileXXXXXX\";\nconst char *cleanup_bootfile = 0;")
"char schemefilename[] = \"/tmp/schemeprogramXXXXXX\";"
"const char *cleanup_schemefile = 0;"

"void cleanup(void) {"
  (if is-atleast-10
    ""
    "if (cleanup_bootfile) unlink(bootfilename);")
  "if (cleanup_schemefile) unlink(schemefilename);"
"}"

"int maketempfile(char *template, const char *contents, size_t size) {"
  "int fd;"
  "fd = mkstemp(template);"
  "assert(fd >= 0);"

  "assert(write(fd, contents, size) == size);"
  "assert(lseek(fd, 0, SEEK_SET) == 0);"
  "return fd;"
"}"

"static const char *argv0;"

"const char *program_name(void) {"
  "return argv0;"
"}"

"void custom_init(void) {"
  "Sregister_symbol(\"program_name\", (void*)program_name);"
  "register_symbols();"
"}"

"int run_program(int argc, const char **argv) {"
  "argv0 = argv[0];"
  "Sscheme_init(0);"
  (if is-atleast-10
    (string-append "Sregister_boot_file_bytes(argv[0], (void*)&" name-of-embedded-code ", " name-of-embedded-code "_size);")
    "Sregister_boot_file(bootfilename);")
  "Sbuild_heap(0, custom_init);"
  "return Sscheme_program(schemefilename, argc, argv);"
"}"

"int main(int argc, const char **argv) {"
  "int schemefd;"
  (if is-atleast-10 "" "int bootfd;")
  "int ret;"

  "atexit(cleanup);"

  (if is-atleast-10
    ""
    (string-append "bootfd = maketempfile(bootfilename, &" name-of-embedded-code ", " name-of-embedded-code "_size);"))
  "schemefd = maketempfile(schemefilename, &scheme_program, scheme_program_size);"
  "cleanup_schemefile = schemefilename;"

  "ret = run_program(argc, argv);"

  (if is-atleast-10 "" "close(bootfd);")
  "close(schemefd);"

  "return ret;"
"}"


"int setupterm(char *term, int fd, int *errret) {"
  "return 0;"
"}"

"int tputs(const char *str, int affcnt, int (*putc)(int)) {"
  "return 0;"
"}"

"void *cur_term;"
))))

(with-temp-directories
  (lambda (create-tempdir)
    (define tempdir (create-tempdir))
    (define wrapped-program-cfile (path-build tempdir "program.generated.c"))
    (define full-chez-a "/tmp/selfcontained-chez/full_chez.a")
    (define program-wpo  (string-append source-file-root ".wpo"))
    (define program-so   (string-append source-file-root ".so"))
    (define program-chez (path-build tempdir "program.chez"))
    (define produced-object-files '())

    (define (write-c-datafile array-name source-path)
      (let ([data (bytevector->u8-list (get-bytevector-all (open-file-input-port source-path)))])
        (format #t "#include <stdint.h>~n")
        (format #t "const uint8_t ~a[] = {~{0x~x,~}};~n" array-name data)
        (format #t "const unsigned int ~a_size = sizeof(~a);~n" array-name array-name)))

    (let ([scheme-header-dir (path-parent scheme-header-file)]
          [custom-entry-file (path-build tempdir "custom-boot.ss")]
          [custom-boot-file (path-build tempdir "custom.boot")]
          [custom-boot-c (path-build tempdir "custom_boot.c")]
          [custom-boot-o (path-build tempdir "custom_boot.o")]
          [scheme-boot-file (lookup-in-scheme-dirs "/scheme.boot")]
          [petite-boot-file (lookup-in-scheme-dirs "/petite.boot")]
          [embedding-c (path-build tempdir "embedding.c")]
          [embedding-o (path-build tempdir "embedding.o")])
      (unless (file-exists? full-chez-a)
        (unless (file-exists? "/tmp/selfcontained-chez")
           (mkdir "/tmp/selfcontained-chez"))

        (with-output-to-file
          custom-entry-file
          (lambda ()
            (write
              '(let ([program-name
                     (foreign-procedure "program_name" () string)])
                (scheme-program
                  (lambda (fn . fns)
                    (command-line (cons (program-name) fns))
                    (command-line-arguments fns)
                    (load-program fn))))))
          '(replace))

        (apply make-boot-file
          custom-boot-file
          '()
          (list petite-boot-file scheme-boot-file custom-entry-file))

        (fasl-compressed #t) ; true is the default
        (if is-atleast-10
          (vfasl-convert-file custom-boot-file custom-boot-file '()))

        (with-output-to-file embedding-c (lambda () (display embedding-code)) '(replace))

        (with-output-to-file custom-boot-c
                             (lambda () (write-c-datafile name-of-embedded-code custom-boot-file))
                             '(replace))
        (run-and-log (string-append "gcc -c -o " custom-boot-o " " custom-boot-c " -I" scheme-header-dir))
        (run-and-log (string-append "gcc -c -o " embedding-o " -x c " embedding-c " -I" scheme-header-dir))
        (run-and-log (string-append "ar rcs " full-chez-a " " custom-boot-o " " embedding-o))))

    (compile-library-handler
      (lambda (source-file object-file)
        (set! produced-object-files (cons object-file produced-object-files))
        (display (string-append "library-handler " source-file " -> " object-file "\n"))
        (compile-library source-file object-file)))

    (compile-file-message #t)
    (compile-imported-libraries #t)
    (generate-wpo-files #t)
    (compile-program source-file)
    (compile-whole-program program-wpo program-chez #t)

    (set! produced-object-files (cons program-so produced-object-files))

    (if clean
      (for-each
        (lambda (path)
          (delete-file path #t)
          (let ((wpo-file (string-append (path-root path) ".wpo")))
            (if (file-exists? wpo-file)
              (delete-file wpo-file #t))))
        produced-object-files))

    (with-output-to-file wrapped-program-cfile
      (lambda ()
        (let ([data (bytevector->u8-list (get-bytevector-all (open-file-input-port program-chez)))]
              [symbol-name "scheme_program"])
          (format #t "#include <stdint.h>~n")
          (format #t "const uint8_t ~a[] = {~{0x~x,~}};~n" symbol-name data)
          (format #t "const unsigned int ~a_size = sizeof(~a);~n" symbol-name symbol-name)
          (display "extern void Sregister_symbol(char*, void*);\n")
          (for-each
            (lambda (symbol)
              (format #t "extern void ~a();\n" symbol))
            foreign-symbols)
          (display "void register_symbols() {\n")
          (for-each
            (lambda (symbol)
              (format #t "Sregister_symbol(\"~a\", ~a);\n" symbol symbol))
            foreign-symbols)
          (display "}")))
      '(replace))

    (run-and-log (apply string-append (map (lambda (s) (string-append s " ")) (list
              cc "-o" source-file-root
              full-chez-a
              (whichever-works
                (lambda () (lookup-in-scheme-dirs "libkernel.a"))
                (lambda () (string-append
                 (lookup-in-scheme-dirs "kernel.o")
                 " -luuid")))
              (whichever-works
                (lambda () (lookup-in-scheme-dirs "libz.a"))
                (lambda () "-lz"))
              (whichever-works
                (lambda () (lookup-in-scheme-dirs "liblz4.a"))
                (lambda () "-llz4"))
              (apply string-append (map (lambda (s) (string-append s " "))
                                        (cdr (command-line-arguments))))
              wrapped-program-cfile
              "-fno-lto" "-m64" "-ldl" "-lm" "-lpthread"))))
    ))

