;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cedet-configs.el					 ;;
;; This file loads the CEDET & ECB libraries,		 ;;
;; and there after sets the custom configuration options ;;
;; 							 ;;
;; Development tools:					 ;;
;; Collection of Emacs Development Environment Tools	 ;;
;; & Emacs Code Browser					 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Notes:
;; Parsing
;; add or remove system include paths with either of these
;; `semantic-customize-system-include-path' ; only for c-mode
;; `semantic-add-system-include'
;; `semantic-remove-system-include'
;; `semanticdb-find-default-throttle' ; controls what to parse

;; Projects
;; use this variable to specify your own functions to determine project roots
;; functions given one directory, and it returns a string or a list of strings
;; root directories for various projects (or use EDE)
;; `semanticdb-project-root-functions'
;; modifies this variable `semanticdb-project-roots'

;; Suggestion by Eric M. Ludlam <eric at siege-engine dot com> on the
;; cedet-semantic mailiing list. (07 May 2010)
;; ,----
;; | If you are comfortable with writing a little bit of lisp code, you can 
;; | probably write a little function to derive the include directories.  For 
;; | example, if the eclipse generated makefile has a variable called:
;; |
;; | INCLUDES=this that theother
;; |
;; | then you could write something like this:
;; |
;; | (defun getincludesfromelicpse (fname)
;; |    "Get includes from the makefile in FNAME."
;; |    (let ((buff (find-file-noselect fname))
;; | 	(var nil))
;; |      (save-excursion
;; |        (set-buffer buff)
;; |        (setq var (semantic-find-tags-by-name "INCLUDES" (current-buffer)))
;; |        (semantic-tag-variable-default (car var))
;; |        )))
;; |
;; | and use that to initialize your include directory list.
;; `----

;; Debugging
;; `semanticdb-dump-all-table-summary' to dump DB for include dirs
;; `semanticdb-find-test-translate-path' to test specific tables found
;; for includes not found - `semanticdb-find-adebug-lost-includes'
;; first tune search path, then search throttle

;; Completions
;; `semantic-complete-analyze-inline-idle' performs completion in buffer
;; `semantic-complete-inline-analyzer-idle-displayor-class' determines completion
;; type. e.g. semantic-displayor-ghost, or tooltip or completions buffer

;; Analyzer
;; `semantic-analyze-current-context' - useful for debugging analyser 
;; `semantic-ia-show-summary', `semantic-ia-show-doc' &
;; `semantic-ia-describe-class'
;; `semantic-ia-fast-jump' - jump to tag 
;; `semantic-ia-fast-mouse-jump' - bind to mouse event
;; e.g. (global-set-key '[(S-mouse-1)] 'mouse-jump)

;; CEDET load
(load-file "~/.emacs.d/elisp/cedet/common/cedet.elc")
;; (load-file "/usr/share/emacs/23.1/lisp/cedet/common/cedet.elc")
(setq semantic-load-turn-useful-things-on t)
(semantic-load-enable-gaudy-code-helpers)

;; loading CEDET libraries
(require 'semantic-ia)
(require 'semantic-gcc)
(require 'eassist)
;; (require 'ede)
;; (global-ede-mode t)

;; eassist-header-switches associates source files with header files
;; based on file name extension eassist-switch-h-cpp uses this var.
;; the order to specify is '(source1 source2 ... header)
(add-to-list 'eassist-header-switches '("C" "cxx" "h"))

;;; custom hooks
(defun my-cedet-c-mode-common-hook ()
  "My c-mode-common-hook for CEDET settings."
  ;; ;; tabs with respect to the previous non-blank line
  ;; (define-key c-mode-base-map (kbd "S-<iso-lefttab>") 'indent-relative)
  ;; (flyspell-prog-mode)
  ;; eassist keybinds
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)
  ;; switches b/w the implementation and prototype declaration
  (define-key c-mode-base-map (kbd "M-p") 'semantic-analyze-proto-impl-toggle)
  ;; `C-<tab>' completes symbol with semantic loaded
  (define-key c-mode-base-map (kbd "C-<tab>") 'semantic-ia-complete-symbol-menu)
  (define-key c-mode-base-map (kbd "C-x <backtab>") 'semantic-ia-complete-symbol)
  (define-key c-mode-base-map (kbd "C-c v") 'semantic-ia-show-variants)
  (define-key c-mode-base-map (kbd "C-c h") 'semantic-decoration-include-visit)
  (define-key c-mode-base-map (kbd "C-c d") 'semantic-ia-show-doc))
(add-hook 'c-mode-common-hook 'my-cedet-c-mode-common-hook)

(defun my-cedet-python-mode-hook ()
  "My python-mode-hook for CEDET settings."
  ;; (flyspell-prog-mode)  
  (define-key python-mode-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'python-mode-hook 'my-cedet-python-mode-hook)

(defun my-cedet-lisp-mode-hook ()
  "My lisp-mode-hook for CEDET settings."
  ;; (flyspell-prog-mode)
  (unless (eq major-mode 'lisp-interaction-mode)
    (define-key lisp-mode-shared-map (kbd "C-<tab>") 'semantic-ia-complete-symbol-menu)
    (define-key lisp-mode-shared-map (kbd "C-x <backtab>") 'semantic-ia-complete-symbol)
    ;; (define-key lisp-mode-shared-map (kbd "C-c v") semantic-ia-show-variants)
    (define-key lisp-mode-shared-map (kbd "M-m") 'eassist-list-methods)))
(add-hook 'lisp-mode-hook 'my-cedet-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-lisp-mode-hook)


;; CEDET parsing customisations
;; 4 ROOT
;; use local ROOT
(setq rootsys "/opt/root-5.27/include")
(semantic-add-system-include rootsys 'c++-mode)

(setq rootmacros "~/codebaby/macros")
(semantic-add-system-include rootmacros 'c++-mode)

;; 4 Athena

;; (defun ntuplemaker(server)
;;   "Setup include path for the Ntuplemaker package.
;;   Takes the location of the package as `server'. If `server' is \"local\",
;;   ignores it, sets up the include path according to `server' otherwise."
;;   (interactive "s remote server: ")
;;   ;; NtupleMaker packages
;;   (setq ntuplemaker-basic (concat server "~/athena/15.5.0/NtupleMaker/BasicNtupleTools"))
;;   (semantic-add-system-include ntuplemaker-basic 'c++-mode)
;;   (setq ntuplemaker-detail (concat server "~/athena/15.5.0/NtupleMaker/DetailedNtupleTools"))
;;   (semantic-add-system-include ntuplemaker-detail 'c++-mode)
;;   (setq ntuplemaker-CBNTalgs (concat server "~/athena/15.5.0/NtupleMaker/CBNTAlgs"))
;;   (semantic-add-system-include ntuplemaker-CBNTalgs 'c++-mode)
;;   (setq ntuplemaker-utils (concat server "~/athena/15.5.0/NtupleMaker/NtupleUtils"))
;;   (semantic-add-system-include ntuplemaker-utils 'c++-mode)
;;   (setq ntuplemaker-trigger (concat server "~/athena/15.5.0/NtupleMaker/TriggerNtupleTools"))
;;   (semantic-add-system-include ntuplemaker-trigger 'c++-mode))

(defun package(server)
  "Setup include path for your package.
  Takes the location of the package as `server'. If `server' is \"local\",
  ignores it, sets up the include path according to `server' otherwise."
  (interactive "s remote server: ")
  (if (string-equal server "local")
      (setq server "")
    (setq server (concat "/ssh:" server ":")))
  ;; Global Monitoring
  (setq DQT (concat server "~sali/public/athena/15.6.5.3/DataQuality/DataQualityTools"))
  (semantic-add-system-include DQT 'c++-mode)
  ;; Muon monitoring
  (setq muon-mon (concat server "~sali/public/athena/15.6.5.3/Reconstruction/MuonIdentification/MuonCombinedValidation/MuonCombinedTrackMon"))
  (semantic-add-system-include muon-mon 'c++-mode))

(defun athena(server)
  "Setup include path for useful athena packages.
  Takes the location of the package as `server'. If `server' is \"local\",
  ignores it, sets up the include path according to `server' otherwise."
  (interactive "s remote server: ")
  ;; Track, TrkExInterface & TrkToolInterface                                                                                                                        
  (if (string-equal server "local")
      (setq server "")
    (setq server (concat "/ssh:" server ":")))
  (setq track (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Tracking/TrkEvent/TrkTrack"))
  (setq trackparticle (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Reconstruction/Particle"))
  (setq trkparameter (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasConditions/15.6.3/Tracking/TrkEvent/TrkParameters"))
  (setq trksummary (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Tracking/TrkEvent/TrkTrackSummary"))
  (setq trkex-int (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Tracking/TrkExtrapolation/TrkExInterfaces"))
  (setq trktool-int (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasReconstruction/15.6.3/Tracking/TrkTools/TrkToolInterfaces"))
  (semantic-add-system-include track 'c++-mode)
  (semantic-add-system-include trackparticle 'c++-mode)
  (semantic-add-system-include trkparameter 'c++-mode)
  (semantic-add-system-include trksummary 'c++-mode)
  (semantic-add-system-include trkex-int 'c++-mode)
  (semantic-add-system-include trktool-int 'c++-mode)

  ;; JetEvent
  (setq jet (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Reconstruction/Jet/JetEvent"))
  (semantic-add-system-include jet 'c++-mode)
  ;; egammaEvent
  (setq egamma (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Reconstruction/egamma/egammaEvent"))
  (semantic-add-system-include egamma 'c++-mode)
  ;; MissingETEvent
  (setq met (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Reconstruction/MissingETEvent"))
  (semantic-add-system-include met 'c++-mode)
  ;; muonEvent
  (setq muon (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Reconstruction/MuonIdentification/muonEvent"))
  (semantic-add-system-include muon 'c++-mode)
  ;; EventInfo
  (setq event-info (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasCore/15.6.3/Event/EventInfo"))
  (semantic-add-system-include event-info 'c++-mode)

  ;; StoreGate
  (setq storegate (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasCore/15.6.3/Control/StoreGate"))
  (semantic-add-system-include storegate 'c++-mode)
  ;; GaudiKernel
  (setq gaudikernel (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/GAUDI/v20r4p6-LCG56d/GaudiKernel"))
  (semantic-add-system-include gaudikernel 'c++-mode)
  ;; AthenaMonitoring
  (setq athenamonitoring (concat server "/country/switzerland/atlas-sw/ATLASLocalRootBase/i686/Athena/Kits_SL4/15.6.3/AtlasEvent/15.6.3/Control/AthenaMonitoring"))
  (semantic-add-system-include athenamonitoring 'c++-mode))

;; ECB
;; ECB load path
;; setting load-path is absolutely essential for ECB to work
;; (add-to-list 'load-path "~jallad/.emacs.d/elisp/ecb")

;; activate ECB by `M-x ecb-activate' or `M-x ecb-minor-mode'
;; (require 'ecb-autoloads)


;; code by David Engster <deng@randomsample.de>
;; (defun get-remote-variable (variable server)
;;   "Get remote environment VARIABLE from SERVER via ssh & bash."
;;   (with-temp-buffer
;;     (call-process "/usr/bin/ssh" nil t nil server "bash" "-i" "-c" "\"athena_setup;env\"")
;;     (if (re-search-backward (format "^%s=\\(.*\\)" variable) nil t)
;; 	(match-string 1)
;;       nil)))

;; (defun set-rootsys ()
;;   "Set rootsys"
;;   (setq rootsys
;;   	(concat (get-remote-variable "ROOTSYS" "strong.phys.sfu.ca") "/include")))
;;   ;; ;; use local ROOT
;;   ;; (setq rootsys "/home/suvayu/root/include"))

;; (add-hook 'semantic-idle-scheduler-mode-hook
;; 	  (lambda()
;; 	    "Reparse custom include directories."
;; 	    ;; (if (string= "" 'rootsys)
;; 	    	(semantic-add-system-include (set-rootsys) 'c++-mode)	    
;; 	      ;; (semantic-add-system-include rootsys 'c++-mode))
;; ))

;; (setq semantic-before-idle-scheduler-reparse-hooks '(semantic-idle-scheduler-mode-hook))

;; (defun add-subdirs-to-system-include-path ()
;;   "Add all subdirectories of current directory to `load-path'.
;; More precisely, this uses only the subdirectories whose names
;; start with letters or digits; it excludes any subdirectory named `RCS'
;; or `CVS', and any subdirectory that contains a file named `.nosearch'."
;;   (let (dirs
;; 	attrs
;; 	(pending (list default-directory)))
;;     ;; This loop does a breadth-first tree walk on DIR's subtree,
;;     ;; putting each subdir into DIRS as its contents are examined.
;;     (while pending
;;       (push (pop pending) dirs)
;;       (let* ((this-dir (car dirs))
;; 	     (contents (directory-files this-dir))
;; 	     (default-directory this-dir)
;; 	     (canonicalized (if (fboundp 'untranslated-canonical-name)
;; 				(untranslated-canonical-name this-dir))))
;; 	;; The Windows version doesn't report meaningful inode
;; 	;; numbers, so use the canonicalized absolute file name of the
;; 	;; directory instead.
;; 	(setq attrs (or canonicalized
;; 			(nthcdr 10 (file-attributes this-dir))))
;; 	(unless (member attrs normal-top-level-add-subdirs-inode-list)
;; 	  (push attrs normal-top-level-add-subdirs-inode-list)
;; 	  (dolist (file contents)
;; 	    ;; The lower-case variants of RCS and CVS are for DOS/Windows.
;; 	    (unless (member file '("." ".." "RCS" "CVS" "rcs" "cvs"))
;; 	      (when (and (string-match "\\`[[:alnum:]]" file)
;; 			 ;; avoid doing a `stat' when it isn't necessary
;; 			 ;; because that can cause trouble when an NFS server
;; 			 ;; is down.
;; 			 (not (string-match "\\.elc?\\'" file))
;; 			 (file-directory-p file))
;; 		(let ((expanded (expand-file-name file)))
;; 		  (unless (file-exists-p (expand-file-name ".nosearch"
;; 							   expanded))
;; 		    (setq pending (nconc pending (list expanded)))))))))))
;;     (semantic-add-system-include (cdr (nreverse dirs)) 'c++-mode)))
