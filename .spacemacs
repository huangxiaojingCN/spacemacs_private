;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     lua
     zilongshanren-ui
     spell-checking
     yaml
     ;;ivy
     html
     javascript
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm 
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     (shell :variables
             shell-default-height 30
             shell-default-position 'bottom)
      spell-checking
      syntax-checking
        version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      git
                                      all-the-icons
                                      company
                                      org2blog
                                      ;;company-meghanada
                                      kotlin-mode
                                      company-statistics
                                      company-tern
                                      company-flx)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(ivy auto-compile)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro for Powerline"
                               :size 19 
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq-default dotspacemacs-themes '(badwolf grandshell spacemacs-dark))  
	(setq configuration-layer-elpa-archives
         '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
           ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
           ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; 显示bar
  ;;(setq cursor-type 'bar)
  (setq-default cursor-type 'bar)

  ;; 选中替换
  (delete-selection-mode t)

  ;; 导入org
  (require 'org)

  ;; 增加全局tags配置
  (setq org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w)
                        ("@home" . ?h)
                        ("@android" . ?a)
                        ("@sleep" . ?s)
                        ("@emacs" . ?e)
                        ("@study_play" . ?y)
                        ("@amusement" . ?m)
                        ("@docker" . ?d)
                        ("@kotlin" . ?k)
                        ("@fitness" . ?f)
                        ("@data_structures" . ?r)
                        ("@nodejs" . ?n)
                        ("@urgent" . ?u)
                        ("@not-urgent" . ?N)
                        (:endgroup . nil)
                        ))

  ;; org-agenda
  (setq org-agenda-files '("~/working/GTD/working.org"                         
                           "~/working/GTD/home.org"
                           "~/working/GTD/sleep.org"
                           "~/working/GTD/emacs.org"
                           "~/working/GTD/android.org"
                           "~/working/GTD/study_play.org"
                           "~/working/GTD/amusement.org"
                           "~/working/GTD/career_planning.org"
                           "~/working/GTD/data_structures_and_algorithms.org"
                           "~/working/GTD/kotlin.org"
                           "~/working/GTD/nodejs.org"
                           "~/working/GTD/docker.org"
                           "~/working/GTD/fitness.org"
                           "~/working/GTD/control_emotion.org"
                           "~/working/GTD/calculus.org"
                           ))
    
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/working/GTD/android.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))

  ;; 自定义org agenda cusotm
  (setq org-agenda-custom-commands 
        '(("du" "重要紧急" tags-todo "urgent" ;; (1) (2) (3) (4)
           ((org-agenda-files '("~/working/GTD/working.org")) ;; (5)
            (org-agenda-sorting-strategy '(priority-up effort-down))) ;; (5) cont.
            ) ;; (6)
          ;; ...other commands here
          ("dn" "重要不紧急" tags-todo "not-urgent" ;; (1) (2) (3) (4)
           ((org-agenda-files '("~/working/GTD/working.org")) ;; (5)
            (org-agenda-sorting-strategy '(priority-up effort-down))) ;; (5) cont.
           )
          ;; other conmmands here
          ))
  
  ;; neotree 树状显示文件列表
  (add-to-list 'load-path "~/personal_management/myspacemacs/emacs-neotree/neotree.el")
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)

  ;; 配置显示目录主题
  (setq neo-theme (if (display-graphic-p) 'icons 'all-the-icons))

  ;; 全屏打开emacs
  (toggle-frame-maximized) 
  (require 'helm-bookmark)

  ;; 设置注释快捷键
  (global-set-key (kbd "C-;") 'spacemacs/comment-or-uncomment-lines)

  ;; 匹配对应s表达是高亮
  (global-set-key (kbd "C-=") 'er/expand-region)
  (set-face-background 'hl-line "#3e4446")
  (set-face-foreground 'hl-line nil)
  ;; (set-face-foreground 'highlight nil)
  (set-face-attribute 'region nil :background "#bbddf2")

  ;; 退出emacs屏幕后保存当前文件
  (defun full-auto-save-all ()
    (interactive)
    (save-excursion
      ;; (message "maybe idle timer?")
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (progn
              (basic-save-buffer)
              )
          ))))
  
  (add-hook 'focus-out-hook 'full-auto-save-all)

  ;; 插入不同语言函数模块
  (defun org-insert-src-block (src-code-type)
    "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
    (interactive
     (let ((src-code-types
            '("C" "C++"
              "clojure" "css"
              "ditaa"
              "emacs-lisp"
              "haskell"
              "java"  "js"
              "lisp" "lua" "matlab"
              "octave" "org"
              "perl"
              "python"  "R"
              "ruby"  "sass"
               "sh"
              "sql" "sqlite"
              )))
       (list (ido-completing-read "Source code type: " src-code-types))))
    (progn
      (newline-and-indent)
      (insert (format "#+BEGIN_SRC %s\n" src-code-type))
      (newline-and-indent)
      (insert "#+END_SRC\n")
      (previous-line 2)
      (org-edit-src-code)))

  (add-hook 'after-init-hook 'global-company-mode)

  ;; 自动补全
  (eval-after-load 'company
    (progn
      (company-flx-mode +1)
      (global-company-mode)
      (setq company-idle-delay 0.618)

      (company-statistics-mode)
      (global-set-key (kbd "M-/") 'company-complete)
      (setq company-minimum-prefix-length 2)
      ;; (setq company-dabbrev-char-regexp "\\sw...[a-zA-Z0-9-/\]?+")
      (setq company-backends
            '((
               company-keywords       ; keywords
               company-dabbrev-code
               company-files
               company-yasnippet
               company-ispell
               )
              ))
      ;;company-meghanada
      (add-hook 'java-mode
                (lambda ()
                  (add-to-list (make-local-variable 'company-backends)
                               'company-meghanada)))
      (add-hook 'python-mode-hook
                (lambda ()
                  (add-to-list (make-local-variable 'company-backends)
                               'company-anaconda)))
      (add-hook 'emacs-lisp-mode
                (lambda ()
                  (add-to-list (make-local-variable 'company-backends)
                               'company-elisp)))
      (dolist (hook '(js-mode-hook
                      js2-mode-hook
                      js3-mode-hook
                      inferior-js-mode-hook
                      ))
        (add-hook hook
                  (lambda ()
                    (tern-mode t)

                    (add-to-list (make-local-variable 'company-backends)
                                 'company-tern)
                    )))
      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  (set (make-local-variable 'company-backends)
                       (list
                        (cons 'company-elisp
                              (car company-backends))))))
      (dolist (hook '(js-mode-hook
                      js2-mode-hook
                      js3-mode-hook
                      js2-jsx-mode-hook
                      ))
        (add-hook hook
                  (lambda ()
                    (tern-mode t)
                    (set (make-local-variable 'company-backends)
                         (list
                          (cons 'company-tern
                                (car company-backends)))))))))

  ;; 撤销undo-tree
  (add-to-list 'load-path "~/myspacemacs/undo-tree/undo-tree.el")
  (require 'undo-tree)
  (global-undo-tree-mode 1)
  
  ;;  load org2blog
  (setq load-path (cons "~/myspacemacs/org2blog" load-path))
  (require 'org2blog-autoloads)

  (setq org2blog/wp-blog-alist
        '(("wordpress"
           :url "http://39.108.213.41:8000/xmlrpc.php"
           :username "huangxiaofeng123321@hotmail.com"
           :default-title "Hello World"
           :default-categories ("org2blog" "emacs" "java" "android" "docker")
           :tags-as-categories nil)
          ("my-blog"
           :url "http://username.server.com/xmlrpc.php"
           :username "admin")))

  (setq helm-follow-mode-persistent t)
  (eval-after-load 'helm
    (progn
      ;; (message "set value after load helm ag")
      ;; (setq helm-ag--extra-options "--ignore R.* --ignore-dir app/build ")
      ;; (lambda () (helm-attrset 'follow 1 helm-do-grep-ag))
      (setq helm-follow-mode-persistent t)
      ))

  (with-eval-after-load 'helm
    (progn
      ;; (setq helm-fuzzy-matching-highlight-fn (quote helm-flx-fuzzy-highlight-match))
      (setq helm-source-names-using-follow (quote ("AG")))
      (setq helm-quick-update t)
      (setq helm-bookmark-show-location t)
      (setq helm-buffers-fuzzy-matching t)
      (setq helm-follow-mode t)
      (setq helm-follow-mode-persistent t)
      (setq helm-ag-always-set-extra-option t)
      (setq helm-ag-insert-at-point 'symbol)
      (setq helm-swoop-pre-input-function
            (lambda ()
              (let (($pre-input (thing-at-point 'symbol)))
                (if (eq (length $pre-input) 0)
                    helm-swoop-pattern ;; this variable keeps the last used words
                  $pre-input))))
      )
    )
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/Users/huangxiaojing/Library/Android/sdk")
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(org-agenda-files
   (quote
    ("~/AelosMini/app/src/main/java/com/lejurobot/aelos/aelosmini/base/BaseActivity.java" "~/working/GTD/working.org" "~/working/GTD/home.org" "~/working/GTD/sleep.org" "~/working/GTD/emacs.org" "~/working/GTD/android.org" "~/working/GTD/study_play.org" "~/working/GTD/amusement.org" "~/working/GTD/career_planning.org" "~/working/GTD/data_structures_and_algorithms.org")))
 '(package-selected-packages
   (quote
    (f badwolf-theme memoize font-lock+ all-the-icons lua-mode moe-theme metaweblog xml-rpc org2blog kotlin-mode company-flx powerline packed avy iedit smartparens highlight evil undo-tree helm helm-core projectile magit magit-popup git-commit async hydra s unfill mwim git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ dash git-gutter flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck eshell-z eshell-prompt-extras esh-help diff-hl auto-dictionary web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode xterm-color smeargle shell-pop orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download multi-term mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore helm-company git ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ccc" :background "#050505")))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/Users/huangxiaojing/Library/Android/sdk")
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(org-agenda-files
   (quote
    ("~/working/GTD/working.org" "~/working/GTD/home.org" "~/working/GTD/sleep.org" "~/working/GTD/emacs.org" "~/working/GTD/android.org" "~/working/GTD/study_play.org" "~/working/GTD/amusement.org" "~/working/GTD/career_planning.org" "~/working/GTD/data_structures_and_algorithms.org")))
 '(package-selected-packages
   (quote
    (grandshell-theme f badwolf-theme memoize font-lock+ all-the-icons lua-mode moe-theme metaweblog xml-rpc org2blog kotlin-mode company-flx powerline packed avy iedit smartparens highlight evil undo-tree helm helm-core projectile magit magit-popup git-commit async hydra s unfill mwim git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ dash git-gutter flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck eshell-z eshell-prompt-extras esh-help diff-hl auto-dictionary web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode xterm-color smeargle shell-pop orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download multi-term mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore helm-company git ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ccc" :background "#050505")))))
)
