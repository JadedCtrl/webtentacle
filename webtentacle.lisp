;;; Copyright 2023, Jaidyn Ann <jadedctrl@posteo.at>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage #:webtentacle
  (:use #:cl)
  (:export :server :start-server :clack-response))

(in-package #:webtentacle)


(defun user-json (&key user host aliases properties links)
  "Given the USER’s information, return the applicable Webfinger JSON.
Details of the values of USER, HOST, ALIASES, PROPERTIES, and LINKS can be found
in the docstring of SERVER."
  (yason:with-output-to-string* ()
    (yason:with-object ()
      (yason:encode-object-element
       "subject"
       (concatenate 'string "acct:" user "@" host))
      (when (and aliases (listp aliases))
        (yason:encode-object-element "aliases" aliases))
      (when (and properties (listp properties))
        (yason:encode-object-element
         "properties"
         (alexandria:plist-hash-table properties)))
      (when (and links (listp links))
        (yason:encode-object-element
         "links"
         (mapcar #'alexandria:plist-hash-table links))))))


(defun fake-info-func (user host)
  "A testing function. This is a USER-INFO-FUNC function that outputs garbage."
  (let ((profile (str:concat "https://example.example/users/" user)))
    (list
     :user user
     :host host
     :aliases (list profile "https://example.example/users/old-user")
     :links
     `((href profile
        rel  "http://webfinger.net/rel/profile-page"
        type "text/html"
             properties ,(alexandria:plist-hash-table '(:apple 3 :bear 4)))
       (href ,profile
        rel  "self"
        type "application/activity+json")))))


(defun resource-user-host (resource)
  "Given a queried RESOURCE, return a list of its contained user and host."
  (let* ((sans-acct (if (str:starts-with-p "acct:" resource)
                        (subseq resource 5)
                        resource)))
    (str:split #\@ sans-acct)))


(defun filter-link-rels (rels link-plists)
  "Given a list of link property-lists, filter out links whose rel properties
aren’t a member of the RELS list.
If RELS is nil, nothing is filtered out.
If RELS is a list of strings, only links with rel properties matching a member
in RELS will remain."
  (if rels
      (remove-if-not
       (lambda (plist)
         (member (getf plist 'rel) rels :test #'equal))
       link-plists)
      link-plists))


(defun filter-user-info-rels (rels user-info)
  "Filter the :LINKS property-list’s properties from a USER-INFO property-list,
by their relations.
If RELS is nil, nothing is filtered out.
If RELS is a list of strings, only links with rel properties matching a member
in RELS will remain."
  (setf (getf user-info :links) (filter-link-rels rels (getf user-info :links)))
  user-info)


(defun clack-response (user-info-func resource &rest rels)
  "Given a USER-INFO-FUNC (as per the specification of SERVER’s docstring), and
the RESOURCE and RELS parameters from a Webfinger HTTP request, return the
response JSON in Clack’s format.
This can be used if you don’t want to wrap your server with SERVER, and would
rather handle the Webfinger path yourself."
  (list 200
        '(:content-type "text/plain")
        (list (format nil "~A~%"
                      (apply #'user-json
                             (filter-user-info-rels
                              rels
                              (apply user-info-func
                                     (resource-user-host resource))))))))


(defun server (env user-info-func &optional (clack-app nil))
  "Start handling Webfinger requests, wrapping around the given CLACK-APP body
function.

USER-INFO-FUNC should be a function that will return user information to be
served by Webfinger.
USER-INFO-FUNC should take two parameters: Username and host, both strings.
It should return a property-list with some of the following properties:
  * :USER
  * :HOST
  * :ALIASES
  * :PROPERTIES
  * :LINKS
You need at minimum :USER and :HOST, all else is optional.

:ALIASES is a simple list of URLs.
:PROPERTIES is a simple property-list of whatever you want.

:LINKS is a list of property-lists, each with some of (or all) of the keys:
  * rel
  * types
  * href
  * titles
  * properties
… all of which are strings, except for the plists “titles” & “properties.”

“properties” should be a property-list containing whatever you want.
“titles” should contain a property for each language-code, with its
value being the corresponding title; for example,
  '(en “Birds & Planes”
    eo “Birdoj k Aviadiloj”
    es “No habla español :-(”)
"
  (let* ((uri (quri:uri (getf env :request-uri)))
         (params (quri:uri-query-params uri)))
    (if (string= (quri:uri-path uri) "/.well-known/webfinger")
        ;; We only want to handle the *exact* webfinger path
        (apply #'clack-response
               (append (list user-info-func
                             (cdr (assoc "resource" params :test #'string=)))
                       ;; We want all “rel” parameters, not just the first one
                       (mapcar
                        #'cdr
                        (remove-if-not
                          (lambda (pair)
                            (string= (car pair) "rel"))
                          params))))
        ;; At any other path, give control back over to the user’s server
        (or (and clack-app (funcall clack-app env))
            '(512 (:content-type "text/plain") ("HECK"))))))


(defun start-server (user-info-func)
  "Run a Webfinger HTTP server, given a USER-INFO-FUNC (see SERVER’s docstring).
This is useful if you want to delegate Webfinger-handling to this library with a
reverse-proxy.
It is also useful for debugging this library."
  (clack:clackup
   (lambda (env)
     (funcall #'server env user-info-func))))
