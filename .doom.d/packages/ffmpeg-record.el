;;; ffmpeg-record.el --- Video and audio recording functions using FFmpeg

;;; Commentary:
;; This package provides functions to record video and audio using the FFmpeg CLI tool.
;; It allows for recording with predefined durations or custom intervals.
;; Requires the exec-path-from-shell package to ensure proper PATH inheritance.

;;; Code:

(require 'exec-path-from-shell)

;; Initialize exec-path-from-shell to inherit PATH from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(defgroup ffmpeg-record nil
  "Settings for FFmpeg recording functions."
  :group 'external)

(defcustom ffmpeg-record-video-directory "~/Videos/"
  "Directory to save video recordings."
  :type 'directory
  :group 'ffmpeg-record)

(defcustom ffmpeg-record-audio-directory "~/Audio/"
  "Directory to save audio recordings."
  :type 'directory
  :group 'ffmpeg-record)

(defcustom ffmpeg-record-video-fps 30
  "Frames per second for video recording."
  :type 'integer
  :group 'ffmpeg-record)

(defcustom ffmpeg-record-video-format "mp4"
  "Default format for video recordings."
  :type 'string
  :group 'ffmpeg-record)

(defcustom ffmpeg-record-audio-format "m4a"
  "Default format for audio recordings."
  :type 'string
  :group 'ffmpeg-record)

(defcustom ffmpeg-record-audio-codec "aac"
  "Audio codec to use for recordings."
  :type 'string
  :group 'ffmpeg-record)

(defcustom ffmpeg-record-audio-device-index "0"
  "Index of the audio device to use for recording."
  :type 'string
  :group 'ffmpeg-record)

(defvar ffmpeg-record--process nil
  "Current FFmpeg recording process.")

(defun ffmpeg-record--ensure-directory (dir)
  "Ensure that DIR exists."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun ffmpeg-record--generate-filename (directory format)
  "Generate a filename in DIRECTORY with FORMAT extension."
  (let ((timestamp (format-time-string "%Y%m%d-%H%M%S")))
    (expand-file-name (concat "recording-" timestamp "." format) directory)))

(defun ffmpeg-record--list-audio-devices ()
  "List available audio devices using ffmpeg."
  (with-temp-buffer
    (if (eq system-type 'darwin)
        (call-process "ffmpeg" nil t nil "-f" "avfoundation" "-list_devices" "true" "-i" "")
      (call-process "ffmpeg" nil t nil "-list_input_devices"))
    (buffer-string)))

(defun ffmpeg-record-select-audio-device ()
  "List available audio devices and let user select one."
  (interactive)
  (let ((devices-output (ffmpeg-record--list-audio-devices))
        (device-list nil)
        (choice nil))
    (with-temp-buffer
      (insert devices-output)
      (goto-char (point-min))
      (while (re-search-forward "\\[AVFoundation indev.*\\] \\[\\([0-9]+\\)\\] \\(.*\\)" nil t)
        (push (cons (match-string 2) (match-string 1)) device-list))
      (setq device-list (nreverse device-list))
      (setq choice (completing-read "Select audio device: " device-list))
      (let ((device-index (cdr (assoc choice device-list))))
        (message "Selected device %s (index: %s)" choice device-index)
        (customize-set-variable 'ffmpeg-record-audio-device-index device-index)))))

(defun ffmpeg-record--detect-video-input ()
  "Detect the appropriate video input device based on OS."
  (cond
   ((eq system-type 'darwin)
    "0") ; macOS video device index
   ((eq system-type 'gnu/linux)
    "/dev/video0")
   ((eq system-type 'windows-nt)
    "video=\"Integrated Camera\"")
   (t (error "Video input not configured for this OS"))))

;; Video recording function with improved process handling
(defun ffmpeg-record-video (duration)
  "Record video for DURATION seconds using FFmpeg."
  (interactive
   (list (read-number "Duration in seconds: " 10)))

  (when (< duration 1)
    (setq duration 10))

  (ffmpeg-record--ensure-directory ffmpeg-record-video-directory)
  (let ((output-file (ffmpeg-record--generate-filename
                      ffmpeg-record-video-directory
                      ffmpeg-record-video-format))
        (video-source (ffmpeg-record--detect-video-input))
        cmdlist)

    ;; Use process arguments as a list rather than a shell command string
    (setq cmdlist
          (cond ((eq system-type 'darwin)
                 (list "ffmpeg" "-y" "-f" "avfoundation" "-framerate"
                       (number-to-string ffmpeg-record-video-fps)
                       "-t" (number-to-string duration)
                       "-i" video-source
                       "-c:v" "libx264" "-preset" "fast" "-pix_fmt" "yuv420p"
                       output-file))
                ((eq system-type 'gnu/linux)
                 (list "ffmpeg" "-y" "-f" "v4l2" "-framerate"
                       (number-to-string ffmpeg-record-video-fps)
                       "-t" (number-to-string duration)
                       "-i" video-source
                       "-c:v" "libx264" "-preset" "fast" "-pix_fmt" "yuv420p"
                       output-file))
                ((eq system-type 'windows-nt)
                 (list "ffmpeg" "-y" "-f" "dshow" "-framerate"
                       (number-to-string ffmpeg-record-video-fps)
                       "-t" (number-to-string duration)
                       "-i" video-source
                       "-c:v" "libx264" "-preset" "fast" "-pix_fmt" "yuv420p"
                       output-file))))

    (message "%s" cmdlist)
    (message "Starting video recording for %d seconds to %s..." duration output-file)

    ;; Use start-process with explicit argument list instead of shell-command
    (let ((ffmpeg-record--process
           (apply 'start-process "ffmpeg-video" "*ffmpeg-recording*" cmdlist)))

      ;; Enhanced process sentinel with better event handling
      (set-process-sentinel
       ffmpeg-record--process
       (lambda (process event)
         (let ((event-str (string-trim event)))
           (cond
            ((string= event-str "finished")
             (message "Video recording completed: %s" output-file))
            ((string= event-str "exited abnormally with code 255")
             (message "FFmpeg process was interrupted"))
            ((string-prefix-p "exited abnormally" event-str)
             (with-current-buffer "*ffmpeg-recording*"
               (message "FFmpeg error: %s. Check *ffmpeg-recording* buffer for details."
                        event-str))))))))))

;; Audio recording function with improved process handling
(defun ffmpeg-record-audio (duration)
  "Record audio for DURATION seconds using FFmpeg."
  (interactive
   (list (read-number "Duration in seconds: " 10)))

  (when (< duration 1)
    (setq duration 10))

  (ffmpeg-record--ensure-directory ffmpeg-record-audio-directory)
  (let ((output-file (ffmpeg-record--generate-filename
                      ffmpeg-record-audio-directory
                      ffmpeg-record-audio-format))
        audio-source
        cmdlist)

    ;; Build argument list based on OS
    (cond ((eq system-type 'darwin)
           (setq audio-source (format ":%s" ffmpeg-record-audio-device-index))
           (setq cmdlist (list "ffmpeg" "-y" "-f" "avfoundation"
                               "-t" (number-to-string duration)
                               "-i" audio-source
                               "-c:a" ffmpeg-record-audio-codec
                               output-file)))
          ((eq system-type 'gnu/linux)
           (setq audio-source "default")
           (setq cmdlist (list "ffmpeg" "-y" "-f" "pulse"
                               "-t" (number-to-string duration)
                               "-i" audio-source
                               "-c:a" ffmpeg-record-audio-codec
                               output-file)))
          ((eq system-type 'windows-nt)
           (setq audio-source "audio=\"Microphone (Realtek High Definition Audio)\"")
           (setq cmdlist (list "ffmpeg" "-y" "-f" "dshow"
                               "-t" (number-to-string duration)
                               "-i" audio-source
                               "-c:a" ffmpeg-record-audio-codec
                               output-file)))
          (t (error "Unsupported system type")))

    ;; (message "%s" cmdlist)
    (message "Starting audio recording for %d seconds to %s..." duration output-file)

    ;; Start process with explicit argument list
    (setq ffmpeg-record--process
          (apply 'start-process "ffmpeg-audio" "*ffmpeg-recording*" cmdlist))

    ;; Enhanced process sentinel
    (set-process-sentinel
     ffmpeg-record--process
     (lambda (process event)
       (let ((event-str (string-trim event)))
         (cond
          ((string= event-str "finished")
           (message "Audio recording completed: %s" output-file))
          ((string= event-str "exited abnormally with code 255")
           (message "FFmpeg process was interrupted"))
          ((string-prefix-p "exited abnormally" event-str)
           (with-current-buffer "*ffmpeg-recording*"
             (message "FFmpeg error: %s. Check *ffmpeg-recording* buffer for details."
                      event-str)))))))))

;; Enhanced process stop function
(defun ffmpeg-record-stop ()
  "Stop the current FFmpeg recording process."
  (interactive)
  (when (and ffmpeg-record--process
             (process-live-p ffmpeg-record--process))
    ;; Try graceful termination first
    (if (eq system-type 'windows-nt)
        ;; Windows doesn't support SIGTERM the same way
        (interrupt-process ffmpeg-record--process)
      (signal-process (process-id ffmpeg-record--process) 'SIGTERM))

    ;; Force kill after a short delay if still running
    (run-with-timer 1.0 nil
                   (lambda ()
                     (when (and ffmpeg-record--process
                                (process-live-p ffmpeg-record--process))
                       (delete-process ffmpeg-record--process)
                       (message "Recording forcibly terminated."))))

    (message "Stopping recording...")
    (setq ffmpeg-record--process nil)))

;; Optional: function to show process details for debugging
(defun ffmpeg-record-debug-process ()
  "Show details about the current ffmpeg process."
  (interactive)
  (if (and ffmpeg-record--process (process-live-p ffmpeg-record--process))
      (message "Process: %s, PID: %s, Status: %s"
               (process-name ffmpeg-record--process)
               (process-id ffmpeg-record--process)
               (process-status ffmpeg-record--process))
    (message "No active ffmpeg process")))

(defun ffmpeg-record-video-5s ()
  "Record video for 5 seconds."
  (interactive)
  (ffmpeg-record-video 5))

(defun ffmpeg-record-video-10s ()
  "Record video for 10 seconds."
  (interactive)
  (ffmpeg-record-video 10))

(defun ffmpeg-record-audio-5s ()
  "Record audio for 5 seconds."
  (interactive)
  (ffmpeg-record-audio 5))

(defun ffmpeg-record-audio-10s ()
  "Record audio for 10 seconds."
  (interactive)
  (ffmpeg-record-audio 10))

(defun ffmpeg-record-stop ()
  "Stop the current FFmpeg recording process."
  (interactive)
  (when (and ffmpeg-record--process
             (process-live-p ffmpeg-record--process))
    (interrupt-process ffmpeg-record--process)
    (message "Recording stopped.")
    (setq ffmpeg-record--process nil)))

(provide 'ffmpeg-record)

;;; ffmpeg-record.el ends here


;; ;; Add Homebrew binaries to Emacs exec-path
;; (add-to-list 'exec-path "/opt/homebrew/bin")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; ;; Record video, prompting for duration
;; M-x ffmpeg-record-video
;;
;; ;; Quick recordings with fixed durations
;; M-x ffmpeg-record-video-5s
;; M-x ffmpeg-record-video-10s
;; M-x ffmpeg-record-audio-5s
;; M-x ffmpeg-record-audio-10s
;;
;; ;; Stop any ongoing recording
;; M-x ffmpeg-record-stop


;; (ffmpeg-record-video-5s)
;; (ffmpeg-record-video 5)
;; (ffmpeg-record-audio-5s)
;; (ffmpeg-record-audio 5)
;; (ffmpeg-record-stop)
;; (ffmpeg-record-debug-process)


;; NOTE
;; The idea of this package is to have Emacs record, audio, and video. This is useful can be used by an LLM or is useful all on its own.
;; These commands work in the shell.
;; However on MacOS, there is an unsolved problem of Emacs interacting with a shell process.


;; NOTE ffmpeg command line
;; ffmpeg -y -f avfoundation -framerate 30 -t 5 -i 0 -c:v libx264 -preset fast -pix_fmt yuv420p /Users/timothyw/Videos/recording-20250302-181453.mp4
;; ffmpeg -y -f avfoundation -t 5 -i ":0" -c:a aac /Users/timothyw/Audio/recording-20250302-181844.m4a


;; NOTE Failing elisp code blocks
;;
;; ;; Audio
;; (setq ffmpeg-record--process
;;       (apply 'start-process "ffmpeg-audio" "*ffmpeg-recording*"
;;              (list "ffmpeg" "-y" "-f" "avfoundation" "-i" ":0" "-t" "5" "-c:a" "aac" "/Users/timothyw/Audio/recording-20250302-181844.m4a")))
;; (set-process-sentinel
;;  ffmpeg-record--process
;;  (lambda (process event)
;;    (message "%s , %s" process event)))
;;
;;
;; ;; Video
;; (setq process-connection-type nil)
;; (setq ffmpeg-record--process
;;       (apply 'start-process "ffmpeg-video" "*ffmpeg-recording*"
;;              (list "ffmpeg" "-y" "-f" "avfoundation" "-framerate" "30" "-pixel_format" "uyvy422" "-i" "0:none" "-t" "5" "-c:v"
;;                    "libx264" "-preset" "fast" "-pix_fmt" "yuv420p" "/Users/timothyw/Videos/recording-20250302-212245.mp4")))
;; (set-process-sentinel
;;  ffmpeg-record--process
;;  (lambda (process event)
;;    (message "%s , %s" process event)))


;; NOTE Stackoverflow Question
;; https://stackoverflow.com/questions/79480200/ffmpeg-video-recording-hangs-in-emacs


;; NOTE Troubleshooting with LLMs

;; ;; Claude
;; https://claude.ai/chat/33dfdcf4-d375-48b5-af96-fda835e09a01
;; https://claude.ai/chat/f608a47c-7786-4d88-9c9b-f0cf62860c7f (main)
;;
;;
;; ;; ChatGPT
;;
;; ;; ffmpeg not recording Audio (but process completing)
;; https://chatgpt.com/c/67ca39c8-f9a8-8012-92e4-f5cead079f09
;;
;; ;; ffmpeg not recording Video (not process not completing)
;; https://chatgpt.com/c/67ca3d6a-b5ac-8012-acd1-c1dbd39e8839
;;
;;
;; ;; Gemini
;; https://gemini.google.com/app/78ecf581027f3d12
;;
;; (setq process-connection-type nil)
;;
;; (setq ffmpeg-record--process
;;       (apply 'start-process "ffmpeg-video" "*ffmpeg-recording*"
;;              (list "ffmpeg" "-y" "-f" "avfoundation" "-framerate" "30" "-t" "5" "-i" "0" "-c:v"
;;                    "libx264" "-preset" "fast" "-pix_fmt" "yuv420p" "/Users/timothyw/Videos/recording-20250302-212245.mp4")))
;;
;; (set-process-sentinel
;;  ffmpeg-record--process
;;  (lambda (process event)
;;    (message "%s , %s" process event)
;;    (if (string-match "finished" event)
;;        (message "ffmpeg-video , finished")
;;      (if (string-match "exit" event)
;;          (progn
;;            (message "ffmpeg-video, exited")
;;            (with-current-buffer "*ffmpeg-recording*"
;;              (message (buffer-string))))))))


;; NOTE Placing ffmpeg in a shell script - no dice

;; (setq ffmpeg-record--process
;;       (apply 'start-process "ffmpeg-audio" "*ffmpeg-recording*"
;;              (list "/Users/timothyw/Projects/dotfiles/.doom.d/packages/troubleshoot-record-audio.sh")))
;; (set-process-sentinel
;;  ffmpeg-record--process
;;  (lambda (process event)
;;    (message "%s , %s" process event)))
;;
;;
;; ;; Video
;; (setq process-connection-type nil)
;; (setq ffmpeg-record--process
;;       (apply 'start-process "ffmpeg-video" "*ffmpeg-recording*"
;;              (list "/Users/timothyw/Projects/dotfiles/.doom.d/packages/troubleshoot-record-video.sh")))
;; (set-process-sentinel
;;  ffmpeg-record--process
;;  (lambda (process event)
;;    (message "%s , %s" process event)))


;;; ffmpeg-record.el ends here
