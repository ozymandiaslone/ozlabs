;;;; proxy-server.lisp

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(ql:quickload '("hunchentoot" "dexador"))

(defparameter *static-dir*
  (merge-pathnames "public/" 
    (make-pathname :directory 
      (pathname-directory *load-pathname*))))

;; Proxy helper: forward request to backend
(defun proxy-request (backend-url request)
  "Forward the Hunchentoot REQUEST to BACKEND-URL and return the body."
  (let* ((uri (hunchentoot:request-uri request))
         (full-url (concatenate 'string backend-url uri)))
    (dexador:get full-url)))  ;; Could also forward headers, POST data, etc.

;; Example routes
(hunchentoot:define-easy-handler (home :uri "/") ()
  (hunchentoot:handle-static-file (merge-pathnames "index.html" *static-dir*)))

(hunchentoot:define-easy-handler (static :uri "/static/") ()
  (hunchentoot:handle-static-file (merge-pathnames "index.html" *static-dir*))) ;; adjust as needed

;; Proxy route to another service running on localhost:5000
(hunchentoot:define-easy-handler (app1 :uri "/app1/") (request)
  (proxy-request "http://localhost:5000" request))

;; Proxy route to another service running on localhost:6000
(hunchentoot:define-easy-handler (app2 :uri "/app2/") (request)
  (proxy-request "http://localhost:6000" request))

(defun main ()
  (format t "~&Starting Lisp reverse proxy on 8080...~%")
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080)))

(main)
