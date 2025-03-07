
;; Function to list all available devices
(defun ffmpeg-list-devices ()
  "List all available audio and video devices."
  (interactive)
  (with-current-buffer (get-buffer-create "*ffmpeg-devices*")
    (erase-buffer)
    (call-process "ffmpeg" nil t nil "-f" "avfoundation" "-list_devices" "true" "-i" "")
    (display-buffer (current-buffer))))

;; Function to test video capture with verbose logging
(defun ffmpeg-test-video-capture-verbose ()
  "Test video capture with verbose logging."
  (interactive)
  (let* ((output-file (expand-file-name
                       (concat "test-recording-"
                               (format-time-string "%Y%m%d-%H%M%S")
                               ".mp4")
                       "~/Videos/"))
         (test-buffer "*ffmpeg-verbose-test*")
         (test-process nil))

    ;; Create and display buffer
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (display-buffer (current-buffer)))

    ;; Set up video-only recording
    (setq test-process
          (start-process "ffmpeg-test" test-buffer
                        "ffmpeg" "-y"
                        "-v" "verbose"  ;; Verbose logging
                        "-f" "avfoundation"
                        "-framerate" "30"
                        "-pixel_format" "uyvy422"
                        "-i" "0:none"   ;; Video only, no audio
                        "-t" "5"        ;; 5 second duration
                        "-c:v" "libx264"
                        "-preset" "ultrafast"  ;; Faster encoding
                        "-pix_fmt" "yuv420p"
                        output-file))

    (message "Started test recording to %s" output-file)

    ;; Set up process sentinel
    (set-process-sentinel
     test-process
     (lambda (process event)
       (message "Test process %s: %s" process (string-trim event))))))

;; Function to test with sync option
(defun ffmpeg-test-video-with-sync ()
  "Test video capture with vsync option."
  (interactive)
  (let* ((output-file (expand-file-name
                       (concat "test-sync-"
                               (format-time-string "%Y%m%d-%H%M%S")
                               ".mp4")
                       "~/Videos/"))
         (test-buffer "*ffmpeg-sync-test*")
         (test-process nil))

    ;; Create and display buffer
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (display-buffer (current-buffer)))

    ;; Try with different sync options
    (setq test-process
          (start-process "ffmpeg-sync-test" test-buffer
                        "ffmpeg" "-y"
                        "-f" "avfoundation"
                        "-framerate" "30"
                        "-pixel_format" "uyvy422"
                        "-i" "0:none"
                        "-t" "5"
                        "-vsync" "2"     ;; Try different sync mode
                        "-c:v" "libx264"
                        "-preset" "ultrafast"
                        "-pix_fmt" "yuv420p"
                        output-file))

    (message "Started sync test recording to %s" output-file)

    ;; Set up process sentinel
    (set-process-sentinel
     test-process
     (lambda (process event)
       (message "Sync test process %s: %s" process (string-trim event))))))

;; Function to test with alternative video format
(defun ffmpeg-test-video-alternative-format ()
  "Test video capture with an alternative format that might complete more reliably."
  (interactive)
  (let* ((output-file (expand-file-name
                       (concat "test-format-"
                               (format-time-string "%Y%m%d-%H%M%S")
                               ".mp4")
                       "~/Videos/"))
         (test-buffer "*ffmpeg-format-test*")
         (test-process nil))

    ;; Create and display buffer
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (display-buffer (current-buffer)))

    ;; Use a simpler format
    (setq test-process
          (start-process "ffmpeg-format-test" test-buffer
                        "ffmpeg" "-y"
                        "-f" "avfoundation"
                        "-framerate" "30"
                        "-pixel_format" "uyvy422"
                        "-i" "0:none"
                        "-t" "5"
                        "-vcodec" "h264_videotoolbox"  ;; Use hardware encoding
                        "-b:v" "2000k"                 ;; Set bitrate
                        output-file))

    (message "Started format test recording to %s" output-file)

    ;; Set up process sentinel
    (set-process-sentinel
     test-process
     (lambda (process event)
       (message "Format test process %s: %s" process (string-trim event))))))

;; Function to test with direct device access
(defun ffmpeg-test-video-direct-device ()
  "Test video capture with direct device specification."
  (interactive)
  (let* ((output-file (expand-file-name
                       (concat "test-direct-"
                               (format-time-string "%Y%m%d-%H%M%S")
                               ".mp4")
                       "~/Videos/"))
         (test-buffer "*ffmpeg-direct-test*")
         (test-process nil))

    ;; Create and display buffer
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (display-buffer (current-buffer)))

    ;; Use the most minimal command possible
    (setq test-process
          (start-process "ffmpeg-direct-test" test-buffer
                        "ffmpeg" "-y"
                        "-f" "avfoundation"
                        "-i" "0"
                        "-t" "3"
                        "-c:v" "copy"  ;; Just copy the stream without re-encoding
                        output-file))

    (message "Started direct test recording to %s" output-file)

    ;; Set up process sentinel
    (set-process-sentinel
     test-process
     (lambda (process event)
       (message "Direct test process %s: %s" process (string-trim event))))))

;; Test with explicitly non-interactive mode
(defun ffmpeg-test-noninteractive ()
  "Test video capture with explicitly non-interactive settings."
  (interactive)
  (let* ((output-file (expand-file-name
                      (concat "test-noninteractive-"
                              (format-time-string "%Y%m%d-%H%M%S")
                              ".mp4")
                      "~/Videos/"))
        (test-buffer "*ffmpeg-noninteractive*")
        (process-connection-type nil)  ;; Use a pipe instead of a pty
        test-process)

    ;; Create and display buffer
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (display-buffer (current-buffer)))

    ;; Add non-interactive flags
    (message "Starting non-interactive test...")
    (setq test-process
          (start-process "ffmpeg-noninteractive" test-buffer
                        "ffmpeg" "-y"           ;; Overwrite without asking
                        "-nostdin"              ;; Disable interaction
                        "-f" "avfoundation"
                        "-framerate" "30"
                        "-pixel_format" "uyvy422"
                        "-i" "0:none"
                        "-t" "5"
                        "-c:v" "libx264"
                        "-preset" "ultrafast"
                        "-pix_fmt" "yuv420p"
                        output-file))

    (message "Process started: %s" test-process)

    ;; Set up process sentinel
    (set-process-sentinel
     test-process
     (lambda (process event)
       (message "Non-interactive test process: %s" (string-trim event))))))

;; Try hardware acceleration
(defun ffmpeg-test-hardware-encoding ()
  "Test video capture with hardware acceleration."
  (interactive)
  (let* ((output-file (expand-file-name
                      (concat "test-hardware-"
                              (format-time-string "%Y%m%d-%H%M%S")
                              ".mp4")
                      "~/Videos/"))
        (test-buffer "*ffmpeg-hardware*")
        (process-connection-type nil)  ;; Use a pipe instead of a pty
        test-process)

    ;; Create and display buffer
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (display-buffer (current-buffer)))

    ;; Use hardware encoding
    (message "Starting hardware encoding test...")
    (setq test-process
          (start-process "ffmpeg-hardware" test-buffer
                        "ffmpeg" "-y"
                        "-nostdin"
                        "-f" "avfoundation"
                        "-framerate" "30"
                        "-pixel_format" "uyvy422"
                        "-i" "0:none"
                        "-t" "5"
                        "-c:v" "h264_videotoolbox"  ;; Hardware encoding
                        output-file))

    (message "Process started: %s" test-process)

    ;; Set up process sentinel
    (set-process-sentinel
     test-process
     (lambda (process event)
       (message "Hardware test process: %s" (string-trim event))))))

;; Ultimate minimal test
(defun ffmpeg-test-minimal ()
  "Test with minimal options that should definitely work."
  (interactive)
  (let* ((output-file (expand-file-name
                      (concat "test-minimal-"
                              (format-time-string "%Y%m%d-%H%M%S")
                              ".mp4")
                      "~/Videos/"))
        (test-buffer "*ffmpeg-minimal*")
        (process-connection-type nil)  ;; Use a pipe instead of a pty
        test-process)

    ;; Create and display buffer
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (display-buffer (current-buffer)))

    ;; Absolute minimal command
    (message "Starting minimal test...")
    (setq test-process
          (start-process "ffmpeg-minimal" test-buffer
                        "ffmpeg" "-y"
                        "-nostdin"
                        "-f" "avfoundation"
                        "-i" "0"
                        "-t" "3"
                        output-file))

    (message "Process started: %s" test-process)

    ;; Set up process sentinel
    (set-process-sentinel
     test-process
     (lambda (process event)
       (message "Minimal test process: %s" (string-trim event))))))
