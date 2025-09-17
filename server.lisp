(declaim (optimize (speed 3) (debug 0) (safety 0) (space 0)))
(ql:quickload '(:hunchentoot :drakma))

(defparameter *proxy-routes*
  '(("/liarsdice" . "http://localhost:3000")
    ("/fitzer"  . "http://localhost:3001")))
(defparameter *portnum* 8080)

(defun find-proxy-route (path)
  "Find matching proxy route for given path"
  (find-if (lambda (route)
             (let ((prefix (car route)))
               (and (>= (length path) (length prefix))
                    (string= prefix path :end2 (length prefix)))))
           *proxy-routes*))


(defun proxy-handler ()
  "Handle proxy requests"
  (let* ((path (hunchentoot:script-name*))
         (route (find-proxy-route path)))
;;    (format t "Path: ~A | route: ~A%" path route)
    (when route
      (let* ((prefix (car route))
             (target (cdr route))
             (upstream-path (subseq path (length prefix)))
             (query (hunchentoot:query-string*))
             (upstream-url (concatenate 'string target upstream-path))
             (full-url (if query
                           (concatenate 'string upstream-url "?" query)
                           upstream-url)))
        (format t "Proxying ~A to ~A~%" path full-url)
        (handler-case
            (multiple-value-bind (body status headers)

                (drakma:http-request full-url
                     :method (hunchentoot:request-method*)
                     :content (hunchentoot:raw-post-data :want-stream nil)
                     :content-type (hunchentoot:header-in* "content-type")
                     :want-stream nil)

              (setf (hunchentoot:return-code*) status)
              (let ((content-type (cdr (assoc :content-type headers))))
                (when content-type
                  (setf (hunchentoot:content-type*) content-type)))
              body)
          (error (e)
            (format t "Proxy error: ~A~%" e)
            (setf (hunchentoot:return-code*) 502)
            "Proxy Error"))))))

(defun proxy-dispatcher (request)
  "Dispatch function for proxy routes"
  (let ((path (hunchentoot:script-name request)))
    (when (find-proxy-route path)
      #'proxy-handler)))

;; Static handler for root
(hunchentoot:define-easy-handler (home :uri "/") ()
  (hunchentoot:handle-static-file #P"./public/index.html"))

;; Set up dispatch table
(setf hunchentoot:*dispatch-table*
      (list #'proxy-dispatcher
            'hunchentoot:dispatch-easy-handlers))

;; Start server
(format t "Starting server @ port: ~A" *portnum*)
(defparameter *server* (make-instance 'hunchentoot:easy-acceptor :port *portnum*))
(hunchentoot:start *server*)
