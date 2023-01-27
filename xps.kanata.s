;; TODO:
;; - could plausibly detect changes to focused window class and activate different layers
;;   depending on focused window
;;   - could map ctrl+u to ctrl+backspace for the moderately annoying ssh key passphrase prompt
;;   - could map ctrl+w to ctrl+backspace for Firefox

;; 102d determined using evtest - I could not make deflocalkeys-linux work as expected- perhaps
;; because I didn't understand
(defsrc
  grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab  q    w    e    r    t    y    u    i    o    p    [    ]
  caps a    s    d    f    g    h    j    k    l    ;    \    ret
  lsft 102d z    x    c    v    b    n    m    ,    .    /    rsft
  lctl lmet lalt           spc                 ralt rctl
)

(defalias
  cap (tap-hold 200 200 esc caps)
  rcl (tap-hold-release 200 200 / rctl)
  lcl (tap-hold-release 200 200 102d lctl)
  atl (multi alt (layer-while-held hold-layer))
  rct (multi rctl (layer-while-held hold-layer))
  lct (multi lctl (layer-while-held hold-layer))
  rsh (multi rsft (layer-while-held hold-layer))
  lsh (multi lsft (layer-while-held hold-layer))
  ;; This fd key-chord can also be achieved with a layer mapped to macros. See this commit for
  ;; details: 679c733d007f3cd52f916003307f516ac83c1b1f
  eff (macro sldr f)
)

(deflayer qwerty
  grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab  q    w    e    r    t    y    u    i    o    p    [    ]
  @cap a    s    d    @eff g    h    j    k    l    ;    \    ret
  @lsh @lcl z    x    c    v    b    n    m    ,    .    @rcl @rsh
  @lct lmet @atl           spc                 ralt @rct
)

;; TODO: (deflayer caps-word) ;; map lower-case to upper-case until the space bar is pressed.
;;                               Perhaps make this the normal capslock behaviour
;; TODO: (deflayer numbers) ;; map e.g.
;;         789
;;         uio
;;         jkl
;;          ,
;;       to
;;         789
;;         456
;;         123
;;          0

(deflayer hold-layer
  _    _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _    _    f    _    _    _    _    _    _    _    _
  _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _              _                   _    _
)

(defseq fd-escape (f d))
(deffakekeys fd-escape esc)
