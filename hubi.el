;;; hubi --- Homogeneous Unified Builder Interface
;;
;; Copyright (C) 2014 Alex Bennée
;;
;; Author: Alex Bennée <alex@bennee.com>
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;;; Commentary:
;;
;; This aims to be a batteries included transient interface to
;; `compile' which allows the user to select targets based on feedback
;; from the build tools. It also enables easy selection of env
;; variables which are often used to select build types.
;;
;;; Code:

;; Require prerequisites

;; Variables

;; Code

(require 'cl-lib)
(require 'files-x)

(defvar hubi-saved-options nil
  "List of default compile options.")

;; c.f. counsel--dominating-file
(defun hubi--dominating-file (file &optional dir)
  "Look up directory hierarchy for FILE, starting in DIR.
Like `locate-dominating-file', but DIR defaults to
`default-directory' and the return value is expanded."
  (and (setq dir (locate-dominating-file (or dir default-directory) file))
       (expand-file-name dir)))

;;
;; Project root finders/helpers
;;
;; In a perfect world there would be one source of truth, however this is
;; Emacs so lets make the heuristics both deep and configurable.
;;

(defun hubi--projectile-root ()
  "Return root of current projectile project or nil on failure.
Use `projectile-project-root' to determine the root."
  (and (fboundp 'projectile-project-root)
       (projectile-project-root)))

(defun hubi--project-current ()
  "Return root of current project or nil on failure.
Use `project-current' to determine the root."
  (let ((proj (and (fboundp 'project-current)
                   (project-current))))
    (cond ((not proj) nil)
          ((fboundp 'project-root)
           (project-root proj))
          ((fboundp 'project-roots)
           (car (project-roots proj))))))

(defun hubi--configure-root ()
  "Return root of current project or nil on failure.
Use the presence of a \"configure\" file to determine the root."
  (hubi--dominating-file "configure"))

(defun hubi--git-root ()
  "Return root of current project or nil on failure.
Use the presence of a \".git\" file to determine the root."
  (hubi--dominating-file ".git"))

(defun hubi--dir-locals-root ()
  "Return root of current project or nil on failure.
Use the presence of a `dir-locals-file' to determine the root."
  (hubi--dominating-file dir-locals-file))

(defun hubi--buffer-default-directory ()
  "Return the `default-directory' of the current buffer."
  default-directory)

(defcustom hubi--root-functions
  '(hubi--projectile-root
    hubi--project-current
    hubi--configure-root
    hubi--git-root
    hubi--dir-locals-root
    hubi--buffer-default-directory)
  "Functions to determine the project root for compile commands.

Each function on this list is called in turn with no arguments.
The first function to return a directory (or a non-nil value that
`file-directory-p' considers a directory) is used as the project root.

The order of the functions determines the priority of the root-finding
methods.  The user should change the order or remove entries if they
want a particular source to take precedence."
  :type '(repeat (function))
  :group 'compile)

(defun hubi--compile-root ()
  "Return root of current project or signal an error on failure.
The root is determined by `hubi--root-functions'."
  (or (run-hook-with-args-until-success 'hubi--root-functions)
      (error "Couldn't find project root")))

;; We use .dir-locals-2.el as .dir-locals is often a version
;; controlled file in software projects.
(defun hubi--dir-local-file()
  "Return the file for the projects local settings.
We take care to put our dir locals in the project root. We rely on the
  normal .dir-locals code to load the values for other files in the project."
  (concat (hubi--compile-root) ".dir-locals-2.el"))

;;
;; Build directory helpers.
;;

(defcustom hubi-build-directories
  '("build" "builds" "bld" ".build")
  "List of potential build directory names to check by
`hubi--find-build-subdir'. The first one
found will be returned. Change the order or add entries to reflect
  your preferred naming style for build directories."
  :type '(repeat directory)
  :group 'compile)

(defun hubi--find-build-subdir (srcdir)
  "Return builds subdirectory of SRCDIR, if one exists."
  (cl-some (lambda (dir)
             (setq dir (expand-file-name dir srcdir))
             (and (file-directory-p dir) dir))
           hubi-build-directories))

(defun hubi--get-build-subdirs (blddir)
  "Return all subdirs under BLDDIR sorted by modification time."
  (let* ((files (directory-files-and-attributes
                 blddir t directory-files-no-dot-files-regexp t))
         (dirs (cl-remove-if-not #'file-directory-p (mapcar #'car files))))
    ;; Include blddir itself if it contains non-directory files.
    (unless (cl-every #'file-directory-p (mapcar #'car files))
      (push blddir dirs))
    (sort dirs (lambda (a b)
                 (file-newer-than-file-p a b))))) ; Newest first

(defun hubi-get-build-directories (&optional dir)
  "Return a list of potential build directories. If no build
directories are found just return a list with the root of the source
tree."
  (let* ((srcdir (or dir (hubi--compile-root)))
         (blddir (hubi--find-build-subdir srcdir)))
    (if blddir
        (hubi--get-build-subdirs blddir)
      (list srcdir))))

;;
;; Target helpers
;;

(defvar hubi-make-phony-pattern "^\\.PHONY:[\t ]+\\(.+\\)$"
  "Regexp for extracting phony targets from Makefiles.")

(defvar hubi-help-pattern
  "\\(?:^\\(\\*\\)?[[:space:]]+\\([^[:space:]]+\\)[[:space:]]+-\\)"
  "Regexp for extracting help targets from a make help call.")

;; This is loosely based on the Bash Make completion code which
;; relies on GNUMake having the following return codes:
;;   0 = no-rebuild, -q & 1 needs rebuild, 2 error
(defun hubi--make-targets (build-dir &optional env)
  "Return a list of Make targets for DIR.

Return a single blank target (so we invoke the default target)
if Make exits with an error.  This might happen because some sort
of configuration needs to be done first or the source tree is
pristine and being used for multiple build trees."
  (with-temp-buffer
    (let* ((res (apply '
                 call-process
                 "make"
                 nil t nil
                 "-C" build-dir "-nqp" env))
           targets)
      (if (or (not (numberp res)) (> res 1))
          (list "")
        (goto-char (point-min))
        (while (re-search-forward hubi-make-phony-pattern nil t)
          (push (split-string (match-string-no-properties 1)) targets))
        (sort (apply #'nconc targets) #'string-lessp)))))

;; (rx (one-or-more blank) (group (one-or-more (not (in blank ":")))))
(defvar hubi-ninja-target-pattern
  "[[:blank:]]+\\([^:[:blank:]]+\\)"
  "Regexp for extracting target names from Ninja query output.")

(defun hubi--ninja-targets (build-dir &optional env)
  "Return a list of Make targets for DIR.

Return a single blank target (so we invoke the default target)
if Make exits with an error.  This might happen because some sort
of configuration needs to be done first or the source tree is
pristine and being used for multiple build trees."
  (with-temp-buffer
    (let* ((res (call-process
                 "ninja" nil t nil
                 "-C" build-dir "-t" "query" "all"))
           targets)
      (if (or (not (numberp res)) (> res 1))
          (list "")
        (goto-char (point-min))
        (while (re-search-forward hubi-ninja-target-pattern nil t)
          (push (split-string (match-string-no-properties 1)) targets))
        (sort (apply #'nconc targets) #'string-lessp)))))


(defvar hubi--target-functions
  '(("make" . hubi--make-targets)
    ("ninja" . hubi--ninja-targets))
  "Alist of compiler tool names and their target helper functions.
The functions take the build directory as a single argument.")

;;
;; Environment handling helpers
;;

(defvar hubi-env nil
  "List of NAME=VAR pairs for the environment variables.")
(make-variable-buffer-local 'hubi-env)
(put 'hubi-env 'permanent-local t)

(defvar hubi-env-history nil
  "History for `hubi'")

(defvar hubi-env-pattern
  "[_[:digit:][:upper:]]+=[/[:alnum:]]*"
  "Pattern to match valid environment variables.")

(defun hubi-env-p (cand)
    "Predicate for testing `CAND' is a valid env value."
    (string-match-p hubi-env-pattern cand))

(transient-define-suffix hubi-add-env (&optional args)
  "Add environment variable to compile invocation.
ARGS used for transient arguments."
  :transient t
  (interactive (list (transient-args transient-current-command)))
  (let ((new-env
         (completing-read "Env: " nil nil
                          #'hubi-env-p nil
                          hubi-env-history)))
    (when new-env
      (push new-env hubi-env))))

(transient-define-suffix hubi-del-env (&optional args)
  "Remove environment variable to compile invocation.
ARGS used for transient arguments."
  :transient t
  (interactive (list (transient-args transient-current-command)))
  (let ((del-env
         (completing-read "Env: " hubi-env nil t)))
    (when del-env
      (setq hubi-env (delete del-env hubi-env)))))

;;
;; Format the final build string
;;
;; Given the tool and directory lets we return the final string that
;; makes up the compile command.
;;

(defcustom hubi-make-args (format "-j%d -k " (+ 1 my-core-count))
  "Additional arguments for make.
You may, for example, want to add \"-jN\" for the number of cores
N in your system."
  :type 'string
  :group 'compile)

(defun hubi--make-formatter (build-dir &optional target env)
  "Format make command with optional parallelism."
  (format "make %s -C %s %s %s"
          hubi-make-args
          (or build-dir default-directory)
          (or target "all")
          (if env
              (mapconcat #'identity env " ")
            "")))

(defvar hubi--format-functions
  '(("make" . hubi--make-formatter)
    ("ninja" . "ninja -C %s %s"))
  "Alist of compiler tool names and their format functions/strings.
The functions take the build directory as a single argument.")

(defun hubi--format-compile (cmd dir &optional target env)
  "Given strings describing `CMD' and the `DIR' return a
formatted string. Lookup the helper from
  `hubi--format-functions' which is an alist of the form
  indexed by `tool' and returning either a format string or a function
  that will return a formatted string when called with build-dir"
  (let ((formatter (cdr (assoc cmd hubi--format-functions))))
    (if (functionp formatter)
        (funcall formatter dir target env)
      (format formatter dir target env))))

;;
;; Compile command handling
;;
;; The command in this case could be a direct compiler invocation to a
;; invoking a build tool.
;;

; nb: use -invocation so variable not picked up by risky-local-variable-p
(defvar hubi-invocation "make"
  "The command we use to compile be it direct compiler or build tool.")
(make-variable-buffer-local 'hubi-invocation)
(put 'hubi-invocation 'permanent-local t)

(defvar hubi-invocation-history nil
  "Command history for `hubi'")

;; currently dumb, we can make smarter
;; we probe the current build directory for things that would prompt
;; for a particular build.
(defun hubi--get-tools ()
  "Return a list of commands and tools we could use"
  '("make" "ninja"))

(transient-define-suffix hubi-get-tool (&optional args)
  "Read the build command we are going to use.
ARGS used for transient arguments."
  :transient t
  (interactive (list (transient-args transient-current-command)))
  (let ((cmd (completing-read "Command: "
                              (hubi--get-tools)
                              nil nil
                              hubi-invocation-history)))
    (when cmd
      (setq hubi-invocation cmd))))

;;
;; Build directory handling
;;

(defvar hubi-directory nil
  "The directory we will be building in.")
(make-variable-buffer-local 'hubi-directory)
(put 'hubi-directory 'permanent-local t)

(defvar hubi-directory-history nil
  "Build directory history for `hubi'")

(transient-define-suffix hubi-get-dir (&optional args)
  "Read the directory command we are going to build in.
ARGS used for transient arguments."
  :transient t
  (interactive (list (transient-args transient-current-command)))
  (let* ((base-dir
          (or (hubi--compile-root)  default-directory))
         (build-dir
          (completing-read
           "Build Directory: "
           (hubi-get-build-directories base-dir)
           nil t base-dir hubi-directory-history)))
    (when build-dir
      (setq hubi-directory build-dir))))

;;
;; Target handling
;;
;; The list of targets will depend on the build tool we are using as
;; we need to query it to get a list of targets.
;;

(defvar hubi-target nil
  "The target we want to build.")
(make-variable-buffer-local 'hubi-target)
(put 'hubi-target 'permanent-local t)

(defvar hubi-target-history nil
  "Target history for `hubi'")

(transient-define-suffix hubi-get-target (&optional args)
  "Read the build command we are going to use.
ARGS used for transient arguments."
  :transient t
  (interactive)
  (let ((helper (cdr (assoc hubi-invocation
                       hubi--target-functions))))
    (setq hubi-target
          (if helper
              (completing-read
               "Command: "
               (funcall helper hubi-directory hubi-env)
               nil t hubi-target-history)
            "all"))))

;;
;; Save/Restore values
;;
;; We actually define buffer-local variables to hold all the details
;; we need. However really these values are "project" wide so we want
;; the same value to hold whatever file in a project we happen to be
;; in when we hit hubi.
;;
;; To do this we utilise .dir-locals stored in the project root. We
;; reply on emacs to load the values automatically for each new file
;; in the project and we update the state before we execute each
;; compile.
;;

(defvar hubi-project-variable-list
  '(hubi-invocation
    hubi-directory
    hubi-target
    hubi-env)
  "List of variables we save in the project.")

(defun hubi--save-project-vars ()
  "Save the hubi variables to the project."
  (let ((dir-local (hubi--dir-local-file)))
    (save-window-excursion
      (mapc
       (lambda (var-symbol)
         (when (symbol-value var-symbol)
           (modify-dir-local-variable
            nil
            var-symbol
            (symbol-value var-symbol)
            'add-or-replace
            dir-local)))
      hubi-project-variable-list)
      (with-current-buffer (get-file-buffer dir-local)
        (setq-local buffer-save-without-query t)))))

;;
;; These define the immediate helpers and actions for the transient
;; defined bellow. All the options will eventually form to a command
;; which will be executed as a compile.
;;
;; Depending on the build system we will either pass the build
;; directory or cd into it to do the right thing.
;;

(transient-define-suffix hubi--do-compile (&optional args)
  "Run compiler. We can command and directory from args."
  (interactive)
  ;; update our dir-locals
  (hubi--save-project-vars)

  ;; Grab the *current* buffer's effective values so we can duplicate
  ;; them in the compilation buffer for if the user re-invokes
  ;; hubi there.
  (let* ((invocation hubi-invocation)
         (directory hubi-directory)
         (target hubi-target)
         (env hubi-env)
         (hook (lambda (_comp-cmd)
                 ;; use the magic of lexical binding...
                 (setq-local hubi-invocation invocation)
                 (setq-local hubi-directory directory)
                 (setq-local hubi-target target)
                 (setq-local hubi-env env))))

  (unwind-protect
      (add-hook 'compilation-start-hook hook)
      (compile (hubi--format-compile
                hubi-invocation
                hubi-directory
                hubi-target
                hubi-env))
      (remove-hook 'compilation-start-hook hook))))

(defun hubi--show-current-compile ()
  "Return a Transient menu headline to indicate the current compile command."
  (concat (propertize "Compile command:\n  " 'face 'transient-heading)
          (propertize
           (hubi--format-compile
            (or hubi-invocation "make")
            (or hubi-directory default-directory)
            (or hubi-target "all")
            hubi-env)
           'face 'transient-value)
          "\n"))

;;
;; Finally the root transient itself
;;

;;;###autoload (autoload 'hubi "hubi" nil t)
(transient-define-prefix hubi ()
  "Compile project with a transient interface."

  ;; Add refresh-suffixes to update the display when values change
  :refresh-suffixes t

  ["Compile"
   :description hubi--show-current-compile

   ["Settings"
    ("C" "Compile Command" hubi-get-tool)
    ("D" "Build Dir" hubi-get-dir)
    ("T" "Target" hubi-get-target)]

   ["Environment"
    ("a" "Add Env Var" hubi-add-env)
    ("d" "Del Env Var" hubi-del-env)

   ]]

  [["Actions"
    ("C-c C-c" "Compile" hubi--do-compile)]])

(provide 'hubi)
;;; hubi.el ends here
