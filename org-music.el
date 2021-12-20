;;; org-music.el --- Links to music within Org -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 TEC
;;
;; Author: TEC <http://github/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: January 06, 2021
;; Modified: January 06, 2021
;; Version: 0.0.1
;; Keywords: multimedia
;; Homepage: https://github.com/tec/org-music
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  links to music within Org
;;
;;; Code:

(require 'rng-uri)
(require 'ox)
(require 'dbus)
(require 'dash)

;;; Variables

(defgroup org-music nil
  "Org music: links"
  :group 'org-link
  :prefix "org-music-")
(defcustom org-music-player 'mpris
  "Music player type. Curretly values of `mpris' and `mpd' are supported."
  :type '(choice (const mpris) (const mpd))
  :group 'org-music)
(defvar org-music-mpris-player nil
  "Name of the mpris player, used in the form org.mpris.MediaPlayer2.PLAYER.
If nil, this is guesed using `org-music-guess-mpris-player'.")
(defvar org-music-track-search-method 'file
  "Method to find the track file from the link.
Can be beets or file.")
(defvar org-music-beets-db nil
  "Location of the beets DB, for when using beets as the `org-music-track-search-method'")
(defvar org-music-folder "~/Music/"
  "Location of your music folder, for when using file as the `org-music-track-search-method'")
(defvar org-music-recognised-extensions '("flac" "mp4" "m4a" "aiff" "wav" "ogg" "aiff")
  "When searching for files in `org-music-track-search-method', recognise these extensions as audio files.")
(defvar org-music-album-art-inline nil
  "Include a base 64 encoded version of the album art.
Try to find a URL for the art otherwise.")
(defvar org-music-youtube-api-key
  (when-let ((secret (plist-get (car (auth-source-search :host "googleapis.com")) :secret)))
    (if (functionp secret)
        (funcall secret) secret))
  "Youtube API key.
Automatically fetched from the authinfo file on initialisation,
by looking for the password of an entry for googleapis.com.")

;;; Utility functions

(defun org-music-get-link (full &optional include-time display-mode)
  "Generate link string for currently playing track, optionally including a time-stamp"
  (pcase org-music-player ;; NOTE this could do with better generalisation
    ('mpris (let* ((track-metadata
                    (org-music-mpris-get-property "Metadata"))
                   (album-artist (caar (cadr (assoc "xesam:albumArtist" track-metadata))))
                   (artist (if (or (equal album-artist "")
                                   (null album-artist)
                                   (s-contains-p "various" album-artist t))
                               (caar (cadr (assoc "xesam:artist" track-metadata)))
                             album-artist))
                   (track (car (cadr (assoc "xesam:title" track-metadata))))
                   (start-time (when include-time
                                 (/ (org-music-mpris-get-property "Position") 1000000))))
              (if full
                  (format "[[%susic:%s][%s by %s]]"
                          (if display-mode "M" "m")
                          (org-music-format-link artist track start-time) track artist)
                (org-music-format-link artist track start-time))))
    ('mpd (org-music-get-link-mpd full include-time display-mode))
    (_ (user-error! "The specified music player: %s is not supported" org-music-player))))

(defun org-music-get-link-mpd (full &optional include-time display-mode)
  (with-temp-buffer
    (call-process "mpc" nil t nil "-f"
                  "%artist%\n%title%")
    (goto-char (point-min))
    ;; parse process output with great pain
    (unless
        (looking-at
         (rx (group (zero-or-more not-newline)) ; artist
             "\n"
             (group (zero-or-more not-newline)) ; track
             "\n"
             ;; skip song info line until e.g 3:45/5:07
             (or "[paused]" "[playing]")
             (one-or-more whitespace)
             "#" (one-or-more (any "0-9")) "/" (one-or-more (any "0-9"))
             (one-or-more whitespace)
             (group (one-or-more (any "0-9"))) ; minutes
             ":"
             (group (one-or-more (any "0-9"))) ; seconds
             "/"))
      (error "couldn't parse mpc -f output"))
    (let ((artist (match-string 1))
          (track (match-string 2))
          (start-time (when include-time
                        (+ (* 60 (string-to-number (match-string 3)))
                           (string-to-number (match-string 4))))))
      (if full
          (format "[[%susic:%s][%s by %s]]"
                  (if display-mode "M" "m")
                  (org-music-format-link artist track start-time) track artist)
        (org-music-format-link artist track start-time)))))

(defun org-music-format-link (artist track &optional start-time end-time)
  (let ((artist (replace-regexp-in-string ":" "\\\\:" artist))
        (track (replace-regexp-in-string ":" "\\\\:" track)))
    (concat artist ":" track
            (cond ((and start-time end-time)
                   (format "::%s-%s"
                           (org-music-seconds-to-time start-time)
                           (org-music-seconds-to-time end-time)))
                  (start-time
                   (format "::%s"
                           (org-music-seconds-to-time start-time)))))))

(defun org-music-parse-link (link)
  (let* ((link-dc (->> link
                       (replace-regexp-in-string "\\([^\\\\]\\)\\\\:" "\\1#COLON#")
                       (replace-regexp-in-string "\\(::[a-z0-9]*[0-9]\\)\\'" "\\1s")))
         (link-components (mapcar (lambda (lc) (replace-regexp-in-string "#COLON#" ":" lc))
                                  (s-split ":" link-dc)))
         (artist (nth 0 link-components))
         (track (nth 1 link-components))
         (durations (when (and (> (length link-components) 3)
                               (equal (nth 2 link-components) ""))
                      (s-split "-" (nth 3 link-components))))
         (start-time (when durations
                       (org-music-time-to-seconds (car durations))))
         (end-time (when (cdr durations)
                     (org-music-time-to-seconds (cadr durations)))))
    (list artist track start-time end-time)))

(defun org-music-seconds-to-time (seconds)
  "Convert a number of seconds to a nice human duration, e.g. 5m21s.
This action is reversed by `org-music-time-to-seconds'."
  (if (< seconds 60)
      (format "%ss" seconds)
    (if (< seconds 3600)
        (format "%sm%ss" (/ seconds 60) (% seconds 60))
      (format "%sh%sm%ss" (/ seconds 3600) (/ (% seconds 3600) 60) (% seconds 60)))))

(defun org-music-time-to-seconds (time-str)
  "Get the number of seconds in a string produced by `org-music-seconds-to-time'."
  (let* ((time-components (reverse (s-split "[a-z]" time-str)))
         (seconds (string-to-number (nth 1 time-components)))
         (minutes (when (> (length time-components) 2)
                    (string-to-number (nth 2 time-components))))
         (hours (when (> (length time-components) 3)
                  (string-to-number (nth 3 time-components)))))
    (+ (* 3600 (or hours 0)) (* 60 (or minutes 0)) seconds)))

(defun org-music-play-track (artist title &optional start-time end-time)
  "Play the track specified by ARTIST and TITLE, optionally skipping to START-TIME in, stopping at END-TIME."
  (if-let ((file (org-music-find-track-file artist title)))
      (pcase org-music-player
        ('mpris (org-music-mpris-play file start-time end-time))
        ('mpd (org-music-mpd-play file start-time end-time))
        (_ (user-error! "The specified music player: %s is not supported" org-music-player)))
    (user-error! "Could not find the track '%s' by '%s'" title artist)))

(defun org-music-mpd-play (file &optional start-time end-time)
  (let ((local-file (string-trim-left (expand-file-name file)
                                      (regexp-quote
                                       (expand-file-name org-music-folder)))))
    (call-process "mpc" nil nil nil "prev" file) ; try not to override current
    (unless (eq 0 (call-process "mpc" nil nil nil "insert" local-file))
      (user-error "Can't `mpc insert %s'" local-file))
    (call-process "mpc" nil nil nil "next")
    (when start-time (call-process "mpc" nil nil nil "seek"
                                   (number-to-string start-time)))
    (ignore end-time))) ; yep
(defun org-music-mpris-play (file &optional start-time end-time)
  (let ((uri (url-encode-url (rng-file-name-uri file))))
    (org-music-mpris-call-method "OpenUri" uri)
    (sleep-for 0.1) ; give things a moment to ge going
    (let ((track-id (caadr (assoc "mpris:trackid"
                                  (org-music-mpris-get-property "Metadata")))))
      (when start-time
        (org-music-mpris-call-method "SetPosition" :object-path track-id
                                     :int64 (round (* start-time 1000000))))
      (when end-time
        (org-music-mpris-stop-at-time uri end-time)))))

(defun org-music-mpris-stop-at-time (url end-time)
  "Check that url is playing, and if it is stop it at END-TIME."
  (when (equal url (caadr (assoc "xesam:url" (org-music-mpris-get-property "Metadata"))))
    (let* ((time-current (/ (/ (org-music-mpris-get-property "Position") 10000) 100.0))
           (time-delta (- end-time time-current)))
      (message "%s" time-delta)
      (if (< time-delta 0)
          (org-music-mpris-call-method "Pause")
        (if (< time-delta 6)
            (run-at-time (max 0.001 (* 0.9 time-delta)) nil #'org-music-mpris-stop-at-time url end-time)
          (run-at-time 5 nil #'org-music-mpris-stop-at-time url end-time))))))

(defun org-music-mpris-get-property (property)
  "Return the value of org.mpris.MediaPlayer2.Player.PROPERTY."
  (dbus-get-property :session (concat "org.mpris.MediaPlayer2."
                                      (or org-music-mpris-player (org-music-guess-mpris-player)))
                     "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player"
                     property))

(defun org-music-mpris-call-method (property &rest args)
  "Call org.mpris.MediaPlayer2.Player.PROPERTY with ARGS, returning the result."
  (apply #'dbus-call-method :session (concat "org.mpris.MediaPlayer2."
                                             (or org-music-mpris-player (org-music-guess-mpris-player)))
         "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player"
         property args))

(defun org-music-guess-mpris-player ()
  (when-let ((players
              (-filter (lambda (interface)
                         (and (s-contains-p "org.mpris.MediaPlayer2" interface)
                              (not (string-match-p "firefox\\|chromium\\|plasma-browser-integration" interface))))
                       (dbus-call-method :session
                                         dbus-service-dbus
                                         dbus-path-dbus
                                         dbus-interface-dbus
                                         "ListNames"))))
    (replace-regexp-in-string "org\\.mpris\\.MediaPlayer2\\." "" (car players))))

(defun org-music-find-track-file (artist title)
  "Try to find the file for TRACK by ARTIST, using `org-music-track-search-method', returning nil if nothing could be found."
  (pcase org-music-track-search-method
    ('file (org-music-find-file artist title))
    ('beets (org-music-beets-find-file artist title))
    (_ (user-error! "The specified music search method: %s is not supported" org-music-track-search-method))))

(defun org-music-beets-find-file (artist title)
  "Find the file correspanding to a given artist and title."
  (let* ((artist-escaped (replace-regexp-in-string "\"" "\\\\\"" artist))
         (title-escaped (replace-regexp-in-string "\"" "\\\\\"" title))
         (file-result (lambda (f) (if (string= "" f ) nil (string-trim-right f)))))
    (or
     (funcall file-result
              (org-music-beets-sql-query
               (format
                "SELECT path FROM items WHERE albumartist IS \"%s\" AND title IS \"%s\" LIMIT 1 COLLATE NOCASE"
                artist-escaped title-escaped)))
     (funcall file-result
              (org-music-beets-sql-query
               (format
                "SELECT path FROM items WHERE artist IS \"%s\" AND title IS \"%s\" LIMIT 1 COLLATE NOCASE"
                artist-escaped title-escaped))))))

(defun org-music-beets-sql-query (query)
  "Submit a QUERY to the beets db."
  (with-temp-buffer
    (call-process
     "sqlite3"
     nil t nil
     (expand-file-name org-music-beets-db)
     query)
    (when (> (point) 1) ; remove trailing newline
      (delete-char -1))
    (buffer-string)))

(defun org-music-beets-all-artist-title ()
  "Return a list of (artist title) elements for the entire db."
  (mapcar
   (lambda (artist-title)
     (split-string artist-title "|"))
   (split-string
    (org-music-beets-sql-query
     "SELECT artist, title, album, genre FROM items") "\n")))

(defvar org-music--beets-all-artist-title-fontified-cache nil)
(defun org-music-beets-all-artist-title-fontified ()
  "Fontified version of `org-music-beets-all-artist-title'."
  (or (cdr (assq (frame-width) org-music--beets-all-artist-title-fontified-cache))
      (cdar (push (cons (frame-width)
                        (mapcar (lambda (result)
                                  (let ((artist (car result))
                                        (track (cadr result))
                                        (album (caddr result))
                                        (genre (cadddr result))
                                        full-width)
                                    (setq full-width (+ (length artist) (length track) (length album) (length genre)))
                                    (concat (propertize artist 'face 'font-lock-keyword-face)
                                            (propertize ":​" 'face 'font-lock-comment-delimiter-face)
                                            (propertize track 'face 'font-lock-variable-name-face)
                                            " " ; no break space, for spliting
                                            (make-string (max 0 (- (frame-width) full-width 3)) ? )
                                            (propertize album 'face 'font-lock-string-face)
                                            " "
                                            (propertize genre 'face 'font-lock-doc-face))))
                                (org-music-beets-all-artist-title)))
                  org-music--beets-all-artist-title-fontified-cache))))

(defun org-music-beets-search ()
  "Interactive search."
  (if (not (eq org-music-track-search-method 'beets))
      (user-error "Beets file location must be used for track searching.")
    (let ((result
           (completing-read "Song: " (org-music-beets-all-artist-title-fontified))))
      (split-string (car (split-string result " ")) ":​"))))

(defun org-music-beets-search-m ()
  "Return a music:search result string, from an interactive search."
  (concat "music:" (org-music-beets-search)))
(defun org-music-beets-search-M ()
  "Return a Music:search result string, from an interactive search."
  (concat "Music:" (org-music-beets-search)))

(defun org-music-search-and-play ()
  "Search for a track, and play it immediately."
  (interactive)
  (apply #'org-music-play-track (org-music-beets-search)))

(defun org-music-search-and-insert ()
  "Search for a track, and insert it as a link."
  (interactive)
  (cl-destructuring-bind (artist track) (org-music-beets-search)
    (insert "[[music:" (org-music-format-link artist track)
            "][" track " by " artist "]]")))

(defun org-music-find-file (artist title)
  "Try to find a file in `org-music-folder' which contains TITLE, looking first in ./ARTIST if possible."
  (when-let* ((music-folder (expand-file-name org-music-folder))
              (search-folders (or
                               (-filter ; look for folders which contain ARTIST
                                (lambda (file-or-folder)
                                  (and
                                   (s-contains-p artist (file-name-base file-or-folder) t)
                                   (file-directory-p file-or-folder)))
                                (directory-files music-folder t))
                               (list music-folder)))
              (extension-regex (format "\\.\\(?:%s\\)\\'" (s-join "\\|" org-music-recognised-extensions)))
              (tracks (-filter
                       (lambda (file)
                         (s-contains-p title (file-name-base file) t))
                       (-flatten (mapcar (lambda (dir)
                                           (directory-files-recursively dir extension-regex))
                                         search-folders)))))
    (when (> (length tracks) 1)
      (message "Warning: multiple matches for %s by %s found" title artist))
    (car tracks)))

(defun org-music-file-metadata (file)
  (let ((result (with-temp-buffer
                  (call-process "ffprobe" nil t nil
                                "-v" "quiet" "-print_format" "json=compact=1" "-show_format" file)
                  (goto-char (point-min))
                  (json-parse-buffer :object-type 'plist))))
    (plist-get (plist-get result :format) :tags)))

(defun org-music-file-cover-thumbnail-url (file)
  (let ((metadata (org-music-file-metadata file))
        (url-request-extra-headers '(("Accept" . "application/json"))))
    (when-let* ((album-id (or (plist-get metadata :MUSICBRAINZ_ALBUMID)
                              (plist-get metadata :MusicBrainz\ Album\ Id)))
                (album-art (with-current-buffer
                               (url-retrieve-synchronously
                                (format "https://coverartarchive.org/release/%s" album-id))
                             (goto-char url-http-end-of-headers)
                             (json-parse-buffer :object-type 'plist :array-type 'list)))
                (cover (car (-filter
                             (lambda (i)
                               (member "Front" (plist-get i :types)))
                             (plist-get album-art :images))))
                (thumbnail (plist-get (plist-get cover :thumbnails) :small)))
      thumbnail)))

;;; Integration with Org

(org-link-set-parameters "music"
                         :follow #'org-music-open-fn
                         :export #'org-music-export-text
                         :complete #'org-music-beets-search-m)

(org-link-set-parameters "Music" ;; like music, but visually fancier
                         ;; FIXME this should work as far as I can tell
                         ;; :image-data-fun #'org-music-image-fn
                         :follow #'org-music-open-fn
                         :export #'org-music-fancy-export
                         :complete #'org-music-beets-search-M)

(defun org-music-open-fn (link)
  (apply #'org-music-play-track (org-music-parse-link link)))

(defun org-music-insert-current-track (&optional include-time)
  "Insert link to currest track, including a timestamp when the universal argument is supplied."
  (interactive "P")
  (pp include-time)
  (insert (org-music-get-link t include-time
                              (and (= (current-column) 0) ; at start of line,
                                   (save-excursion        ; and line above blank
                                     (forward-line -1)
                                     (end-of-line)
                                     (= (current-column) 0))))))

(defun org-music-export-text (path desc backend &optional _info newline album-p)
  (let* ((track-info (org-music-parse-link path))
         (artist (nth 0 track-info))
         (track (nth 1 track-info))
         (start-time (nth 2 track-info))
         (end-time (nth 3 track-info))
         (youtube (org-music-youtube-track artist track))
         (url (or (org-music-youtube-track-url youtube start-time)
                  (org-music-default-url artist track)))
         (link (lambda (body)
                 (if (not url) body
                   (cond ((org-export-derived-backend-p backend 'html)
                          (concat "<a target=\"_blank\" title=\"" (replace-regexp-in-string "\"" "\\\\\"" (plist-get youtube :title)) "\" href=\"" url "\">" body "</a>"))
                         ((org-export-derived-backend-p backend 'latex)
                          (concat "\\href{" url "}{" body "}"))
                         (t body)))))
         (emphasise (cond ((org-export-derived-backend-p backend 'html)
                           (lambda (s) (format "<span style=\"font-style: italic\">%s</span>" s)))
                          ((org-export-derived-backend-p backend 'latex)
                           (lambda (s) (format "\\emph{%s}" s)))
                          (t (lambda (s) s))))
         (diminish (cond ((org-export-derived-backend-p backend 'html)
                          (lambda (s) (format "<small>%s</small>" s)))
                         ((org-export-derived-backend-p backend 'latex)
                          (lambda (s) (format "{\\footnotesize %s}" s)))
                         (t (lambda (s) (format "(%s)" s))))))
    (funcall link
             (or desc
                 (concat
                  (cond ((and start-time end-time)
                         (format "%s to %s seconds of%s" start-time end-time (or newline " ")))
                        (start-time
                         (format "%s seconds into%s" start-time (or newline " "))))
                  (funcall emphasise track)
                  (when album-p
                    (when-let ((album (org-music-kid3-get "Album" (org-music-find-track-file artist track))))
                      (concat (or newline " ") (funcall diminish album))))
                  (or newline " ")
                  "by "
                  artist)))))

(defvar org-music--youtube-cache
  (make-hash-table :test 'equal))

(defun org-music-youtube-track (artist track)
  (let ((stored-value (gethash (concat artist " " track) org-music--youtube-cache)))
    (unless stored-value
      (setq stored-value
            (let* ((file (org-music-find-track-file artist track))
                   (file-youtube-id (when file (org-music-kid3-cmd "get \"YOUTUBE_ID\"" file))))
              (puthash (concat artist " " track)
                       (if (and file-youtube-id (not (string= "" file-youtube-id)))
                           (list :id file-youtube-id
                                 :title (org-music-kid3-cmd "get \"YOUTUBE_TITLE\"" file))
                         (when org-music-youtube-api-key
                           (let ((data (if-let* ((result
                                                  (with-current-buffer
                                                      (url-retrieve-synchronously
                                                       (format "https://youtube.googleapis.com/youtube/v3/search?part=snippet&q=%s&type=video&key=%s"
                                                               (url-hexify-string
                                                                (concat artist " " track))
                                                               org-music-youtube-api-key))
                                                    (json-parse-buffer :object-type 'plist)))
                                                 (videos (plist-get
                                                          result
                                                          :items))
                                                 (vid1 (if (= (length videos) 0) nil
                                                         (aref videos 0))))
                                           (list
                                            :id (plist-get (plist-get vid1 :id) :videoId)
                                            :title (replace-regexp-in-string "\\\\ " " " (plist-get (plist-get vid1 :snippet) :title)))
                                         'none)))
                             (when file
                               (org-music-kid3-set "YOUTUBE_ID" (plist-get data :id) file)
                               (org-music-kid3-set "YOUTUBE_TITLE" (plist-get data :title) file))
                             data)))
                       org-music--youtube-cache))))
    (if (equal 'none stored-value) nil stored-value)))

(defun org-music-youtube-track-url (youtube-entry &optional start-time)
  (when youtube-entry
    (format "https://www.youtube.com/watch?v=%s%s"
            (plist-get youtube-entry :id)
            (when (and start-time (/= 0 start-time))
              (format "&t=%d" start-time)))))

(defun org-music-default-url (artist track)
  (concat "https://www.youtube.com/results?search_query="
          (url-hexify-string
           (concat artist " - " track))))

(defun org-music-kid3-cmd (cmd file)
  (with-temp-buffer
    (call-process
     "kid3-cli"
     nil t nil
     "-c" cmd file)
    (when (> (point) 1) ; remove trailing newline
      (delete-char -1))
    (buffer-string)))

(defun org-music-kid3-get (param file)
  (org-music-kid3-cmd (format "get \"%s\"" param) file))

(defun org-music-kid3-set (param value file)
  (org-music-kid3-cmd (format "set \"%s\" \"%s\"" param (shell-quote-argument value)) file)
  value)

(defun org-music-cover-image (track-file)
  "Try to find a cover image for the track in the given location"
  (car (-filter (lambda (file)
                  (-contains-p '("png" "jpg" "jpeg") (file-name-extension file)))
                (directory-files (file-name-directory track-file) t "cover"))))

(defun org-music-image-fn (_protocol link _description)
  (when-let* ((track-data (org-music-parse-link link))
              (cover-file (org-music-cover-image
                           (org-music-find-track-file
                            (nth 0 track-data) (nth 1 track-data)))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq buffer-file-coding-system 'binary)
      (insert-file-contents cover-file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-music-fancy-export (path desc backend _info)
  (let* ((track-data (org-music-parse-link path))
         (file (org-music-find-track-file
                (nth 0 track-data) (nth 1 track-data)))
         (url (or (org-music-youtube-track-url (org-music-youtube-track (nth 0 track-data)
                                                                        (nth 1 track-data))
                                               (nth 2 track-data))
                  (org-music-default-url (nth 0 track-data) (nth 1 track-data))))
         (cover-img (if (org-export-derived-backend-p backend 'html)
                        (if org-music-album-art-inline
                            (concat
                             "data:image/png;base64,"
                             (base64-encode-string
                              (with-temp-buffer
                                (let* ((img-file (org-music-cover-image file))
                                       (tmp-img-file (make-temp-file "albumart-" nil (file-name-extension img-file t))))
                                  (when (executable-find "convert")
                                    (call-process "convert" nil nil nil
                                                  img-file "-resize" "250x250" tmp-img-file)
                                    (setq img-file tmp-img-file))
                                  (when (and (executable-find "pngquant")
                                             (string= "png" (file-name-extension img-file)))
                                    (call-process "pngquant" nil nil nil "-f" "-o" img-file img-file))
                                  (insert-file-contents img-file))
                                (buffer-string))))
                          (or (org-music-file-cover-thumbnail-url file)
                              (org-music-cover-image file)))
                      (org-music-cover-image file)))
         (newline-str (cond ((org-export-derived-backend-p backend 'html) "<br>")
                            ((org-export-derived-backend-p backend 'latex) "\\newline ")
                            (t " ")))
         (text (org-music-export-text path nil backend nil newline-str t)))
    (cond ((org-export-derived-backend-p backend 'html)
           (format "<div class='music-track'>\n    %s<img src='%s'>%s <span>%s</span>\n</div>"
                   (if url
                       (concat "<a target=\"_blank\" href=\"" url "\">")
                     "")
                   cover-img
                   (if url "</a>\n" "")
                   text))
          ((org-export-derived-backend-p backend 'latex)
           (format "\\begin{tabular}{@{\\hspace{0.3\\columnwidth}}r@{\\hspace{0.1\\columnwidth}}p{0.4\\columnwidth}}
  \\includegraphics[height=6em]{%s} & \\vspace{-0.12\\columnwidth}%s
\\end{tabular}" cover-img text))
          (t text))))

(provide 'org-music)
;;; org-music.el ends here
