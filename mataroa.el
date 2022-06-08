;;; mataroa.el --- Emacs interface to 'mataroa.blog'  -*- lexical-binding: t; -*-
;; Copyright (C) 2022 yote

;; Author: yote <me@yote.cl>
;; Package-Requires: request
;; Version: 1.0
;; URL: https://git.sr.ht/~coyotes/mataroa.el

;;; Commentary:

;;; Code:
(require 'request)
(require 'iso8601)
(require 'cl-lib)

(defgroup mataroa nil
  "An Emacs interface for the blogging platform 'mataroa.blog'."
  :group 'tools)

(defcustom mataroa-api-key ""
  "The API key for your blog."
  :type 'string
  :group 'mataroa)

(defun mataroa--request (endpoint type &optional data)
  "Make a request to endpoint ENDPOINT in the 'mataroa.blog' API, of HTTP type TYPE and with DATA (if provided)."
  (when (equal mataroa-api-key "")
    (error "API key must be set to use this library"))
  (let* ((req (if (or (eq type 'patch) (eq type 'post))
                  (request (concat "https://mataroa.blog/api/" endpoint "/")
                    :type (upcase (symbol-name type))
                    :headers (list (cons "Content-Type"  "application/json")
                                   (cons "Authorization" (concat "Bearer " mataroa-api-key)))
                    :parser #'json-read
                    :data (json-serialize data)
                    :sync t)
                (request (concat "https://mataroa.blog/api/" endpoint)
                  :type (upcase (symbol-name type))
                  :headers (list (cons "Accept" "application/json")
                                 (cons "Authorization" (concat "Bearer " mataroa-api-key)))
                  :parser #'json-read
                  :sync t)))
         (resp (request-response-data req)))
    (if (eq (alist-get 'ok resp) t)
        resp
      (error (concat "API returned an error: \"" (or (alist-get 'message resp)
                                                     (alist-get 'error resp))
                     "\"")))))

(cl-defstruct mataroa-post title slug body published-at url)

(defun mataroa--parse-post (alist)
  "Parse ALIST into a MATAROA-POST value."
  (let-alist alist
    (make-mataroa-post :title .title
                       :slug .slug
                       :body .body
                       :published-at (if .published_at (iso8601-parse .published_at) nil)
                       :url .url)))

(defun mataroa--make-params (post)
  "Parse POST into an alist for submitting to the API."
  (let ((title (mataroa-post-title post))
        (body (mataroa-post-body post))
        (slug (mataroa-post-slug post))
        (published-at (mataroa-post-published-at post))
        (params nil))
    (if title        (setq params (append params (list (cons 'title title)))))
    (if body         (setq params (append params (list (cons 'body body)))))
    (if slug         (setq params (append params (list (cons 'slug slug)))))
    (if published-at (setq params (append params (list (cons 'published_at
                                                             (format-time-string "%Y-%m-%d" published-at))))))
    params))

(defun mataroa-get-posts ()
  "List all blogposts from user's blog."
  (cl-map 'vector
          #'mataroa--parse-post
          (cdr (assq 'post_list (mataroa--request "posts" 'get)))))

(defun mataroa-get-post (slug)
  "Get blogpost with SLUG from user's blog."
  (mataroa--parse-post (mataroa--request (concat "posts/" slug) 'get)))

(defun mataroa-create-post (post)
  "Create a blogpost according to parameters in POST."
  (let-alist (mataroa--request "posts" 'post (mataroa--make-params post))
    (make-mataroa-post :title (mataroa-post-title post)
                       :slug .slug
                       :body (mataroa-post-body post)
                       :published-at (mataroa-post-published-at post)
                       :url .url)))

(defun mataroa-delete-post (slug)
  "Delete blogpost with SLUG from user's blog."
  (mataroa--request (concat "posts/" slug) 'delete))

(defun mataroa-update-post (slug post &optional return)
  "Update blogpost with SLUG using the parameters in POST, and return new post if RETURN is T."
  (let ((resp (mataroa--request (concat "posts/" slug) 'patch (mataroa--make-params post))))
    (if return
        (mataroa-get-post (or (alist-get 'slug resp) slug))
      t)))

;; The UI part
(defun --mataroa-get-org-kw ()
  "Get keywords from current Org file as an alist."
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword)))))

(provide 'mataroa)
;;; mataroa.el ends here
