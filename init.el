;;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is all kinds of necessary
(require 'package)
(setq package-enable-at-startup nil)

;;; remove SC if you are not using sunrise commander and org if you like outdated packages
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;;; Experimental email stuff.
(when (file-readable-p "~/.email/email.org")
  (org-babel-load-file (expand-file-name "~/.email/email.org")))

;;; Anything below is personal preference.
;;; I recommend changing these values with the "customize" menu
;;; You can change the font to suit your liking, it won't break anything.
;;; The one currently set up is called Terminus.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("e513ab7e3d29a7cd6f28c5141cf119107a9ecf5c09dde34cbe9c16866c586d93" "1068ae7acf99967cc322831589497fee6fb430490147ca12ca7dd3e38d9b552a" default)))
 '(elfeed-feeds
   (quote
    ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7UGbBVrqLlq6CRxWWUmyKw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCRDVE5L1LTWhmPAKKbUBGtg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCbKWv2x9t6u8yZoB3KcPtnw" "https://www.youtube.com/feeds/videos.xml?channel_id=UC8Q7XEy86Q7T-3kNpNjYgwA" "https://www.youtube.com/feeds/videos.xml?channel_id=UC2bkHVIDjXS7sgrgjFtzOXQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCLPA0Dhi1VXQDGbkXwjDUzQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCE_hjYd8HzRTVVXEr7jjHng" "https://www.youtube.com/feeds/videos.xml?channel_id=UCR1D15p_vdP3HkrH8wgjQRw" "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uzKdS5m6tMvmY9yrBOjvQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCoQsV1On1wnbGn705cf_9Gw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCLFXk9J3O-hhOk0msOjKYdQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UC08J6mHDz7LGFKYGk24aZ7w" "https://www.youtube.com/feeds/videos.xml?channel_id=UC7Ucs42FZy3uYzjrqzOIHsw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCD6VugMZKRhSyzWEWA9W2fg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCZKyj7wDE51SMbkrRBT6SdA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCbWcXB0PoqOsAvAdfzWMf0w" "https://www.youtube.com/feeds/videos.xml?channel_id=UCtUbO6rBht0daVIOGML3c8w" "https://www.youtube.com/feeds/videos.xml?channel_id=UCeFKyZ8pvMLBtCSNi2MXFEQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q" "https://www.youtube.com/feeds/videos.xml?channel_id=UCSdma21fnJzgmPodhC9SJ3g" "https://www.youtube.com/feeds/videos.xml?channel_id=UCJQfl8QxjNen736AVO3ecFg" "https://www.youtube.com/feeds/videos.xml?channel_id=UC4BZtFgtCuHUt0p8J-XENiA" "https://www.youtube.com/feeds/videos.xml?channel_id=UC1JTQBa5QxZCpXrFSkMxmPw" "https://www.youtube.com/feeds/videos.xml?channel_id=UC8Jb0pdRKRNnfeOBpjmzsSQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCAX4ogRqB75_auq7CbiszKg" "https://www.youtube.com/feeds/videos.xml?channel_id=UC7a_SNxd34tycAfyAykhBng" "https://www.youtube.com/feeds/videos.xml?channel_id=UCRbOPaGDB_xOQkVM8Rnn62Q" "https://www.youtube.com/feeds/videos.xml?channel_id=UCE1jXbVAGJQEORz9nZqb5bQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCT7njg__VOy3n-SvXemDHvg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCCkkkf3aQu2VoWBw_paxkQw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCAL3JXZSzSm8AlZyD3nQdBA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCYZtp0YIxYOipX15v_h_jnA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCWPTiFpzm8559H-9Err59gw" "https://www.youtube.com/feeds/videos.xml?channel_id=UClOGLGPOqlAiLmOvXW5lKbw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCJ6KZTTnkE-s2XFJJmoTAkw" "https://www.youtube.com/feeds/videos.xml?channel_id=UC-e_7qz0m9FdP970j3-SFBg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCPhVxopWpXWy-qHMXRVPLEQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCyhnYIvIKK_--PiJXCMKxQQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCWqr2tH3dPshNhPjV5h1xRw" "https://www.youtube.com/feeds/videos.xml?channel_id=UC5miyvhPsWWyfTulnJ43koQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCMm211NGh4Ls5SAMZJF7E8A" "https://www.youtube.com/feeds/videos.xml?channel_id=UCPgqonWzGADVVV6gGVGDdPA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCcAvljdM2NMdMYq_pvT9pBw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCsvn_Po0SmunchJYOWpOxMg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCLmzk98n_v2doN2Y20S-Zog" "https://www.youtube.com/feeds/videos.xml?channel_id=UCgxtI7tTpLmhBbBbZz1VicA" "https://www.youtube.com/feeds/videos.xml?channel_id=UC168YOUo8WHRedlWUKdlTLg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCrTNhL_yO3tPTdQ5XgmmWjA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCb_sF2m3-2azOqeNEdMwQPw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCdJdEguB1F1CiYe7OEi3SBg" "https://prequeladventure.com/feed/" "https://www.boywhofell.com/comic/rss" "http://www.paranatural.net/rss.php" "https://mangadex.org/rss/follows/rasPqBkfdYtzZRFTMv74ygD9SKh6Ge8H" "https://jaiminisbox.com/reader/feeds/rss")))
 '(org-agenda-files (quote ("~/ドキュメント/agenda.org")))
 '(package-selected-packages
   (quote
    (flycheck-gometalinter lsp-mode mingus flycheck-ledger ledger-mode helm-projectile fish-completion docker w3m lispy geiser flycheck-golangci-lint csv-mode pretty-hydra hydra love-minor-mode auto-complete slime-company slime company-jedi zzz-to-char rainbow-delimiters avy ivy projectile sunrise-x-modeline sunrise-x-buttons sunrise-commander twittering-mode zerodark-theme pretty-mode flycheck-clang-analyzer flycheck-irony flycheck yasnippet-snippets yasnippet company-c-headers company-shell company-irony irony irony-mode company-lua mark-multiple expand-region swiper popup-kill-ring dmenu ido-vertical-mode ido-vertical ox-html5slide centered-window-mode htmlize ox-twbs diminish erc-hl-nicks symon rainbow-mode switch-window dashboard smex company sudo-edit emms magit org-bullets hungry-delete beacon linum-relative spaceline fancy-battery exwm which-key use-package)))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "1ASC" :family "Fira Code"))))
 '(fringe ((t (:background "#292b2e"))))
 '(org-ellipsis ((t (:foreground "#abb2bf" :underline nil)))))
