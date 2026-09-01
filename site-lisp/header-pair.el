;;; header-pair.el --- Open C/C++ header counterparts -*- lexical-binding: t; -*-

(require 'project)

(defconst ashu-header-pair--extensions
  '("h" "hh" "hpp" "hxx")
  "Header file extensions considered by `ashu-open-header-pair'.")

(defun ashu-header-pair--project-root (file)
  "Return FILE's project root, or the nearest parent with an include directory."
  (let ((default-directory (file-name-directory file)))
    (or (when-let ((project (project-current nil)))
          (project-root project))
        (locate-dominating-file
         file
         (lambda (directory)
           (file-directory-p (expand-file-name "include" directory)))))))

(defun ashu-header-pair--find (file)
  "Return FILE's matching header below its project's include directory.
Return nil when no matching header exists."
  (when-let* ((root (ashu-header-pair--project-root file))
              (include-directory (expand-file-name "include" root))
              ((file-directory-p include-directory)))
    (let ((base-name (file-name-base file))
          (header-regexp "\\.\\(?:h\\|hh\\|hpp\\|hxx\\)\\'"))
      (catch 'header
        (dolist (candidate
                 (directory-files-recursively include-directory header-regexp))
          (when (and (string= (file-name-base candidate) base-name)
                     (not (file-equal-p candidate file)))
            (throw 'header candidate)))
        nil))))

(defun ashu-open-header-pair ()
  "Open the current file's matching header in another window.
Search the current project's include directory and report when no pair exists."
  (interactive)
  (if-let* ((file buffer-file-name)
            (header (ashu-header-pair--find file)))
      (let ((window (or (condition-case nil
                            (split-window-right)
                          (error nil))
                        (condition-case nil
                            (split-window-below)
                          (error nil)))))
        (if window
            (progn
              (set-window-buffer window (find-file-noselect header))
              (select-window window))
          (message "Cannot split a window to show the header pair")))
    (message "Header pair not found")))

(global-set-key (kbd "C-x g") #'ashu-open-header-pair)

(provide 'header-pair)
;;; header-pair.el ends here
