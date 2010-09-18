(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; Static References

(defun css (path)
  (with-html (:link :rel "stylesheet" :type "text/css" :href (url path))))

(defun js (path)
  (with-html (:script :type "application/javascript" :src (url  path))))

(defun img (path)
  (with-html
    (:img :src (url "img/" path))))

(defun lib (path)
  (with-html (:script :type "application/javascript"
                      :src (url "lib" path))))



;;; HTML Head

(defun head-js-std ()
  (mapc #'js '("lib/jquery/jquery-1.4.2.min.js"
               "lib/jquery-ui/js/jquery-ui-1.8.2.custom.min.js" 
               "js/main.js")))


(defun head-css-std ()
  (mapc #'css '("css/reset.css"
                "css/main.css"
                "lib/jquery-ui/css/smoothness/jquery-ui-1.8.2.custom.css")))

(defun head-config () 
  (head-css-std)
  (css '"css/table.css")
  (head-js-std))



;;; User interface elements

(defun ok-button ()
  (with-html
    (:button :type "submit" (img "tick.png"))))

(defun cancel-button (href)
  (with-html
    (:a :href href (img "cancel.png"))))

(defun logo ()
  (with-html
    (:h1 "Scrooge")))

(defun footer ()
  (with-html
    (:div :id "footer" "Powered by lisp")))




