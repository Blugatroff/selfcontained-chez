#!/usr/bin/env -S scheme --script

(let-values (((major minor point) (scheme-version-number)))
  (if (> 10 major)
    (eval `(define (path-build a b)
             (string-append a "/" b)))))

(define (whatever-file-exists . files)
  (if (null? files)
    (error 'whatever-file-exists "expected at least on file path"))
  (call/cc (lambda (return)
             (for-each (lambda (file)
                         (if (file-exists? file)
                           (return file)))
                       files)
             (return (car files)))))

(define scheme-dir (cadr (command-line-arguments)))

(if (< (length (command-line-arguments)) 3)
  (begin
    (display "Missing arguments, expected:\n")
    (display "  ./compile.ss <c-compiler> <scheme-dir> <source-file>")))

(define c-compiler  (car   (command-line-arguments)))
(define scheme-dir  (cadr  (command-line-arguments)))
(define source-file (caddr (command-line-arguments)))

(define source-file-root (path-root source-file))

(if (eq? #f scheme-dir)
  (begin
    (display "SCHEME_DIR environment variable missing\n" (current-error-port))
    (exit 1)))

(display scheme-dir) (newline)

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

"char bootfilename[] = \"/tmp/chezschemebootXXXXXX\";"
"char schemefilename[] = \"/tmp/schemeprogramXXXXXX\";"
"const char *cleanup_bootfile = 0;"
"const char *cleanup_schemefile = 0;"

"void cleanup(void) {"
  "if (cleanup_bootfile) unlink(bootfilename);"
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
"}"

"int run_program(int argc, const char **argv, const char *bootfilename, const char *schemefilename) {"
  "argv0 = argv[0];"
  "Sscheme_init(0);"
  "Sregister_boot_file(bootfilename);"
  "Sbuild_heap(0, custom_init);"
  "return Sscheme_program(schemefilename, argc, argv);"
"}"



"int main(int argc, const char **argv) {"
  "int bootfd;"
  "int schemefd;"
  "int ret;"

  "atexit(cleanup);"

  (string-append "bootfd = maketempfile(bootfilename, &" name-of-embedded-code ", " name-of-embedded-code "_size);")
  "cleanup_bootfile = bootfilename;"
  "schemefd = maketempfile(schemefilename, &scheme_program, scheme_program_size);"
  "cleanup_schemefile = schemefilename;"

  "ret = run_program(argc, argv, bootfilename, schemefilename);"

  "close(bootfd);"
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
    (define custom-boot-file (path-build tempdir "custom-boot.ss"))
    (define wrapped-program-cfile (path-build tempdir "program.generated.c"))
    (define embedding-code-file (path-build tempdir "embedding.c"))
    (define embedding-o (path-build tempdir "embedding.o"))
    (define petite-boot-file (string-append scheme-dir "/petite.boot"))
    (define scheme-boot-file (string-append scheme-dir "/scheme.boot"))
    (define petite-custom-boot-file (path-build tempdir "petite.boot"))
    (define scheme-custom-boot-file (path-build tempdir "scheme.boot"))
    (define petite-boot-c (path-build tempdir "petite_boot.c"))
    (define scheme-boot-c (path-build tempdir "scheme_boot.c"))
    (define petite-boot-o (path-build tempdir "petite_boot.o"))
    (define scheme-boot-o (path-build tempdir "scheme_boot.o"))
    (define petite-a    (path-build tempdir "petite.a"))
    (define full-chez-a (path-build tempdir "full-chez.a"))
    (define program-wpo  (string-append source-file-root ".wpo"))
    (define program-so   (string-append source-file-root ".so"))
    (define program-chez (path-build tempdir "program.chez"))
    (define produced-object-files '())

    (define (write-c-datafile array-name source-path)
      (let ([data (bytevector->u8-list (get-bytevector-all (open-file-input-port source-path)))])
        (format #t "#include <stdint.h>~n")
        (format #t "const uint8_t ~a[] = {~{0x~x,~}};~n" array-name data)
        (format #t "const unsigned int ~a_size = sizeof(~a);~n" array-name array-name)))

    (with-output-to-file
      custom-boot-file
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

    ; (display "making custom petite boot file\n")
    ; (apply make-boot-file
    ;   petite-custom-boot-file
    ;   '()
    ;   (list petite-boot-file custom-boot-file))

    (display "making custom full-chez boot file\n")
    (apply make-boot-file
      scheme-custom-boot-file
      '()
      (list petite-boot-file scheme-boot-file custom-boot-file))

    ; (with-output-to-file petite-boot-c
    ;                      (lambda () (write-c-datafile name-of-embedded-code petite-custom-boot-file))
    ;                      '(replace))
    (with-output-to-file scheme-boot-c
                         (lambda () (write-c-datafile name-of-embedded-code scheme-custom-boot-file))
                         '(replace))

    (with-output-to-file embedding-code-file (lambda () (display embedding-code)) '(replace))

    ; (run-and-log (string-append "gcc -c -o " petite-boot-o " " petite-boot-c " -I" scheme-dir))
    (run-and-log (string-append "gcc -c -o " scheme-boot-o " " scheme-boot-c " -I" scheme-dir))
    (run-and-log (string-append "gcc -c -o " embedding-o " -x c " embedding-code-file " -I" scheme-dir))

    ; (run-and-log (string-append "ar rcs " petite-a    " " petite-boot-o " " embedding-o))
    (run-and-log (string-append "ar rcs " full-chez-a " " scheme-boot-o " " embedding-o))

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

    (for-each
      (lambda (path)
        (delete-file path #t)
        (let ((wpo-file (string-append (path-root path) ".wpo")))
          (if (file-exists? wpo-file)
            (delete-file wpo-file #t))))
      produced-object-files)

    (with-output-to-file wrapped-program-cfile
      (lambda ()
        (let ([data (bytevector->u8-list (get-bytevector-all (open-file-input-port program-chez)))]
              [symbol-name "scheme_program"])
          (format #t "#include <stdint.h>~n")
          (format #t "const uint8_t ~a[] = {~{0x~x,~}};~n" symbol-name data)
          (format #t "const unsigned int ~a_size = sizeof(~a);~n" symbol-name symbol-name)))
      '(replace))

    (run-and-log (apply string-append (map (lambda (s) (string-append s " ")) (list
              c-compiler "-o" source-file-root
              full-chez-a
              (whatever-file-exists (string-append scheme-dir "/libkernel.a") (string-append scheme-dir "/kernel.o"))
              ; (let ((file (string-append scheme-dir "/liblz4.a"))) (if (file-exists? file) file ""))
              ; (let ((file (string-append scheme-dir "/libz.a"))) (if (file-exists? file) file ""))
              wrapped-program-cfile
              "-m64" "-ldl" "-lm" "-lpthread" "-lz" "-llz4" "-luuid"))))
    ))

