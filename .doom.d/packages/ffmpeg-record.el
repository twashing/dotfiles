;;; ffmpeg-record.el --- Video and audio recording functions using FFmpeg

;;; Commentary:
;; This package provides functions to record video and audio using the FFmpeg CLI tool.
;; It allows for recording with predefined durations or custom intervals.
;; This version avoids any dependencies on cl-lib or related packages.

;;; Code:

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

(defcustom ffmpeg-record-audio-format "mp3"
  "Default format for audio recordings."
  :type 'string
  :group 'ffmpeg-record)

(defcustom ffmpeg-record-audio-codec "aac"
  "Audio codec to use for recordings."
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

(defun ffmpeg-record--detect-video-input ()
  "Detect the appropriate video input device based on OS."
  (cond
   ((eq system-type 'darwin)
    "avfoundation:\"FaceTime HD Camera\"")
   ((eq system-type 'gnu/linux)
    "/dev/video0")
   ((eq system-type 'windows-nt)
    "video=\"Integrated Camera\"")
   (t (error "Video input not configured for this OS"))))

(defun ffmpeg-record--detect-audio-input ()
  "Detect the appropriate audio input device based on OS."
  (cond
   ((eq system-type 'darwin)
    "avfoundation:\":0\"")
   ((eq system-type 'gnu/linux)
    "pulse")
   ((eq system-type 'windows-nt)
    "audio=\"Microphone (Realtek High Definition Audio)\"")
   (t (error "Audio input not configured for this OS"))))

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
        video-source
        cmd)

    ;; Determine appropriate command based on OS
    (setq video-source
          (cond ((eq system-type 'darwin) "0")
                ((eq system-type 'gnu/linux) "/dev/video0")
                ((eq system-type 'windows-nt) "video=\"Integrated Camera\"")
                (t (error "Unsupported system type"))))

    (setq cmd
          (cond ((eq system-type 'darwin)
                 (format "ffmpeg -y -f avfoundation -framerate %d -t %d -i %s -c:v libx264 -preset fast -pix_fmt yuv420p %s"
                         ffmpeg-record-video-fps
                         duration
                         video-source
                         output-file))
                ((eq system-type 'gnu/linux)
                 (format "ffmpeg -y -f v4l2 -framerate %d -t %d -i %s -c:v libx264 -preset fast -pix_fmt yuv420p %s"
                         ffmpeg-record-video-fps
                         duration
                         video-source
                         output-file))
                ((eq system-type 'windows-nt)
                 (format "ffmpeg -y -f dshow -framerate %d -t %d -i %s -c:v libx264 -preset fast -pix_fmt yuv420p %s"
                         ffmpeg-record-video-fps
                         duration
                         video-source
                         output-file))))

    (message "Starting video recording for %d seconds..." duration)
    (setq ffmpeg-record--process
          (start-process "ffmpeg-video" "*ffmpeg-recording*" "sh" "-c" cmd))

    (set-process-sentinel
     ffmpeg-record--process
     (lambda (process event)
       (when (string-match "finished" event)
         (message "Video recording completed: %s" output-file))))))

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
        input-format
        cmd)

    ;; Determine appropriate command based on OS
    (cond ((eq system-type 'darwin)
           (setq input-format "avfoundation")
           (setq audio-source "\":0\""))
          ((eq system-type 'gnu/linux)
           (setq input-format "pulse")
           (setq audio-source "default"))
          ((eq system-type 'windows-nt)
           (setq input-format "dshow")
           (setq audio-source "audio=\"Microphone (Realtek High Definition Audio)\""))
          (t (error "Unsupported system type")))

    (setq cmd (format "ffmpeg -y -f %s -t %d -i %s -c:a %s %s"
                      input-format
                      duration
                      audio-source
                      ffmpeg-record-audio-codec
                      output-file))

    (message "Starting audio recording for %d seconds..." duration)
    (setq ffmpeg-record--process
          (start-process "ffmpeg-audio" "*ffmpeg-recording*" "sh" "-c" cmd))

    (set-process-sentinel
     ffmpeg-record--process
     (lambda (process event)
       (when (string-match "finished" event)
         (message "Audio recording completed: %s" output-file))))))

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

(let ((output-file (ffmpeg-record--generate-filename
                    ffmpeg-record-video-directory
                    ffmpeg-record-video-format))
      video-source
      cmd)

  ;; Determine appropriate command based on OS
  (setq video-source
        (cond ((eq system-type 'darwin) "0")
              ((eq system-type 'gnu/linux) "/dev/video0")
              ((eq system-type 'windows-nt) "video=\"Integrated Camera\"")
              (t (error "Unsupported system type"))))

  (setq cmd
        (cond ((eq system-type 'darwin)
               (format "ffmpeg -y -f avfoundation -framerate %d -t %d -i %s -c:v libx264 -preset fast -pix_fmt yuv420p %s"
                       ffmpeg-record-video-fps
                       duration
                       video-source
                       output-file))
              ((eq system-type 'gnu/linux)
               (format "ffmpeg -y -f v4l2 -framerate %d -t %d -i %s -c:v libx264 -preset fast -pix_fmt yuv420p %s"
                       ffmpeg-record-video-fps
                       duration
                       video-source
                       output-file))
              ((eq system-type 'windows-nt)
               (format "ffmpeg -y -f dshow -framerate %d -t %d -i %s -c:v libx264 -preset fast -pix_fmt yuv420p %s"
                       ffmpeg-record-video-fps
                       duration
                       video-source
                       output-file))))

  ;; (message "Video recording: for %d seconds..." duration)
  (message "Video recording: for %s" cmd)
  )

;;; ffmpeg-record.el ends here
