(module i-wonder (i-wonder)

(import scheme chicken)
(use awful awful-sql-de-lite regex srfi-1 srfi-13 data-structures files)

(define (i-wonder base-path db-file #!key (awful-settings (lambda (_) (_))))

  (unless (file-exists? db-file)
    (print "Could not find the database file (" db-file "). Aborting.")
    (exit 1))

  (add-request-handler-hook!
   'i-wonder
   (lambda (path handler)
     (when (string-prefix? base-path path)
       (parameterize ((enable-sxml #t)
                      (app-root-path base-path)
                      (db-credentials db-file)
                      (page-css "/css/i-wonder.css"))
         (switch-to-sql-de-lite-database)
         (awful-settings handler)))))

  (parameterize ((enable-sxml #t))

    (define (awful-badge)
      `(div (@ (id "awful"))
            (a (@ (href "http://wiki.call-cc.org/egg/awful"))
               (img (@ (border 0) (src "/img/thats-awful.png"))))))

    (define (permalink id)
      (link (make-pathname base-path (->string id)) "permalink"
            class: "permalink"))

    (define (report-abuse id)
      (link "#" "Report abuse" id: (conc "abuse-" id) class: "report-abuse"))

    (define (wondering-links id)
      `(div (@ (class "wbar"))
            ,(report-abuse id)
            " "
            ,(permalink id)))

    (define (render-wondering wondering id)
      `(div (@ (class "random-wondering"))
            "I wonder " ,wondering
            ,(wondering-links id)))

    (define (render-single-wondering wondering id)
      `(div (@ (id "single-wondering"))
            ,wondering
            ,(wondering-links id)))

    (define (ajax/report-abuse)
      (ajax "report-abuse" ".report-abuse" 'click
            (lambda ()
              (set-abusive! ($ 'id)))
            live: #t
            prelude: "elt=$(this);"
            arguments: '((id . "$(this).attr('id').replace(/abuse-/, '')"))
            success: "elt.html('Thanks!')"))

    ;;;
    ;;; DB stuff
    ;;;
    (define (get-new-captcha)
      (car ($db "select hash, question from captchas order by random() limit 1")))

    (define (get-captcha-for-hash hash)
      (caar ($db "select answer from captchas where hash=?"
                 values: (list hash))))

    (define (insert-wondering wondering)
      ;; Insert the given wondering and retur its id in the DB
      ($db "insert into wonderings (wondering) values (?)"
           values: (list wondering))
      (caar ($db "select last_insert_rowid() from wonderings")))

    (define (get-wondering id)
      ($db "select wondering from wonderings where rowid=?"
           values: (list id)))

    (define (set-abusive! id)
      ($db "update wonderings set abusive=((select abusive from wonderings where rowid=?) + 1) where rowid=?"
           values: (list id id)))

    (define (get-random-wonderings)
      ($db "select rowid, wondering from wonderings order by random() limit 25"))


    ;;;
    ;;; Pages
    ;;;
    (define-page base-path
      (lambda ()

        (add-javascript
         "$(document).ajaxStart(function(){$('#ajax-busy').show();}).ajaxStop(function(){$('#ajax-busy').hide();});"
         "$('#captcha-question').simpletip({content:'Just wondering if you are human.', fixed: false});"
         "$('#echo-window').hide();"
         "$('#close-echo').click(function(){$('#echo-window').fadeOut();});"
         "$('#wondering').focus();")

        (ajax "save-wondering" 'wondering-submit 'click
              (lambda ()
                (with-request-variables (wondering captcha-answer captcha-hash captcha-question)
                  (let ((captcha-right-answer (get-captcha-for-hash captcha-hash))
                        (new-captcha (get-new-captcha)))
                    (debug captcha-answer " " captcha-right-answer " " captcha-hash " " new-captcha)
                    (if (equal? captcha-right-answer captcha-answer)
                        `((new-wondering
                           . ,(if (and wondering (not (equal? wondering "")))
                                  (render-wondering wondering (insert-wondering wondering))
                                  ""))
                          (echo-message . "")
                          (captcha-hash . ,(car new-captcha))
                          (captcha-question . ,(cadr new-captcha)))
                        ;; Wrong captcha answer
                        `((new-wondering . "")
                          (echo-message . "Wrong captcha answer, dude (or dudette)...")
                          (captcha-hash . ,(string-append "'" captcha-hash "'"))
                          (captcha-question . ,(string-append "'" captcha-question "'")))))))
              arguments: '((wondering . "$('#wondering').val()")
                           (captcha-answer . "$('#captcha-answer').val()")
                           (captcha-question . "$('#captcha-question').text()")
                           (captcha-hash . "$('#captcha-hash').val()"))
              success: (string-append
                        "$('#new-wondering').html(response['new-wondering']);"
                        "var msg = response['echo-message'];"
                        "if (msg == '') { "
                        "  $('#wondering').val('');"
                        "  $('#captcha-answer').val('');"
                        "  $('#captcha-question').html(response['captcha-question']);"
                        "  $('#captcha-hash').val(response['captcha-hash']);"
                        "} else {"
                        "  $('#echo-message').html(msg); $('#echo-window').show();"
                        "}"
                        "$('#wondering').focus();")
              update-targets: #t)

        (ajax/report-abuse)

        (let* ((captcha (get-new-captcha))
               (captcha-hash (car captcha))
               (captcha-question (cadr captcha)))
          `((div (@ (id "echo-window"))
                 (img (@ (src "/img/close.png") (id "close-echo")))
                 (div (@ (id "echo-message"))))
            (div (@ (id "i-wonder")) "I wonder ...")
            (input (@ (type "text") (id "wondering") (size 70) (maxlen 100)))
            (div (@ (id "submit-container"))
                 (span (@ (id "captcha-question")) ,captcha-question)
                 (input (@ (type "text") (id "captcha-answer") (size 8)))
                 (input (@ (type "hidden") (id "captcha-hash") (value ,captcha-hash)))
                 ,(link "#" "Submit" id: "wondering-submit" class: "link-button"))
            (div (@ (id "wonderings"))
                 (div (@ (id "new-wondering")))
                 ,@(map (lambda (id/wondering)
                          (let ((id (car id/wondering))
                                (wondering (cadr id/wondering)))
                            (render-wondering wondering id)))
                        (get-random-wonderings)))
            ,(awful-badge))))
      title: "I wonder..."
      headers: (include-javascript "/js/jquery.simpletip-1.3.1.pack.js")
      use-ajax: #t)


    (define-page (regexp (make-pathname base-path "/[0-9]+"))
      (lambda (path)
        (ajax/report-abuse)
        (let* ((id (last (string-split path "/")))
               (wondering (get-wondering id)))
          `((div (@ (id "i-wonder")) "I wonder ")
            ,(render-single-wondering
              (if (null? wondering)
                  "if you wonder that this wondering exists."
                  (caar wondering))
              id)
            ,(awful-badge))))
      title: "I wonder..."
      use-ajax: #t)

    )))
