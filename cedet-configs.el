;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cedet-configs.el					 ;;
;; This file loads the CEDET & ECB libraries,		 ;;
;; and there after sets the custom configuration options ;;
;; 							 ;;
;; Development tools:					 ;;
;; Collection of Emacs Development Environment Tools	 ;;
;; & Emacs Code Browser					 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doxymacs
;; (require 'doxymacs)

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
(defun my-c-mode-common-hook ()
  ;; ;; tabs with respect to the previous non-blank line
  ;; (define-key c-mode-base-map (kbd "S-<iso-lefttab>") 'indent-relative)
  (flyspell-prog-mode)
  ;; eassist keybinds
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)
  ;; switches b/w the implementation and prototype declaration
  (define-key c-mode-base-map (kbd "M-p") 'semantic-analyze-proto-impl-toggle)
  ;; `C-<tab>' completes symbol with semantic loaded
  (define-key c-mode-base-map (kbd "C-<tab>") 'semantic-ia-complete-symbol-menu)
  (define-key c-mode-base-map (kbd "s-<tab>") 'semantic-ia-complete-symbol)
  (define-key c-mode-base-map (kbd "s-h") 'semantic-decoration-include-visit)
  (define-key c-mode-base-map (kbd "s-d") 'semantic-ia-show-doc))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-python-mode-hook ()
  (flyspell-prog-mode)  
  (define-key python-mode-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun my-lisp-mode-hook ()
  (flyspell-prog-mode)
  (eldoc-mode t)
  (define-key lisp-mode-shared-map (kbd "C-<tab>") 'semantic-ia-complete-symbol-menu)
  (define-key lisp-mode-shared-map (kbd "s-<tab>") 'semantic-ia-complete-symbol)
  (define-key lisp-mode-shared-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)


;; CEDET parsing customisations
;; 4 ROOT
;; use local ROOT
(setq rootsys "/opt/root/include")
(semantic-add-system-include rootsys 'c++-mode)

;; 4 Athena

(defun ntuplemaker(server)
  "Setup include path for the Ntuplemaker package.
  Takes the location of the package as `server'. If `server' is \"local\",
  ignores it, sets up the include path according to `server' otherwise."
  (interactive "s remote server: ")
  ;; NtupleMaker packages
  (setq ntuplemaker-basic (concat server "~/athena/15.5.0/NtupleMaker/BasicNtupleTools"))
  (semantic-add-system-include ntuplemaker-basic 'c++-mode)
  (setq ntuplemaker-detail (concat server "~/athena/15.5.0/NtupleMaker/DetailedNtupleTools"))
  (semantic-add-system-include ntuplemaker-detail 'c++-mode)
  (setq ntuplemaker-CBNTalgs (concat server "~/athena/15.5.0/NtupleMaker/CBNTAlgs"))
  (semantic-add-system-include ntuplemaker-CBNTalgs 'c++-mode)
  (setq ntuplemaker-utils (concat server "~/athena/15.5.0/NtupleMaker/NtupleUtils"))
  (semantic-add-system-include ntuplemaker-utils 'c++-mode)
  (setq ntuplemaker-trigger (concat server "~/athena/15.5.0/NtupleMaker/TriggerNtupleTools"))
  (semantic-add-system-include ntuplemaker-trigger 'c++-mode))

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
