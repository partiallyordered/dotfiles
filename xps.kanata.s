;; TODO:
;; - show current layer on the status bar
;; - could implement modal input and indicate mode (normal/input) on the status bar
;; - could plausibly detect changes to focused window class and activate different layers
;;   depending on focused window
;;   - could map ctrl+u to ctrl+backspace for the moderately annoying ssh key passphrase prompt
;;   - Firefox (and others?)
;;     - basically implement "readline" "insert mode" mapping
;;       - map ctrl+w to ctrl+backspace
;;       - map ctrl+e to end
;;       - map ctrl+a to home (or would this be annoying?)
;;       - map ctrl+u to (macro shift-home backspace)
;;       - map ctrl+y to (I can't remember?)
;;       - map ctrl+h to backspace
;;       - map ctrl+k to (macro shift-end backspace)
;; - Sometimes when pressing e.g. forward-slash and expecting a forward-slash to be emitted, one is
;;   not. This is probably because I'm pressing the next key while the forward-slash is still
;;   depressed. This could be mitigated by using tap-hold-release, but this might require some
;;   diligence and/or training. The reason for this is because when using tap-hold-release,
;;   sometimes I would release the next key before the modifier key. E.g. when attempting to emit
;;   ctrl+p, I might do the following:
;;   1. press forward-slash
;;   2. press p
;;   3. release forward-slash
;;   4. release p
;;   resulting in the emission of "/p", i.e. forward-slash followed by p
;;   - What happens with the "real" ctrl keys, though?
;; - Use space as a modifier key? With tap-hold-release probably, to prevent accidental modifier
;;   presses. In particular, as a "symbols" layer, perhaps?
;; - Remap ctrl keys to be e.g. the symbols layers?? (Too much..??)

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
  lcz (tap-hold-release 200 200 z lctl)
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
  @lsh @lcl @lcz x    c    v    b    n    m    ,    .    @rcl @rsh
  @lct lmet @atl           spc                 ralt @rct
)

;; TODO: (deflayer caps-word) ;; Map lower-case to upper-case until the space bar is pressed.
;;                               Probably achieved by emitting capslock, and entering a new layer
;;                               where all keys are transparent, except the space bar, which emits
;;                               capslock and returns to the previous layer
;;                               Perhaps make this the normal capslock behaviour.
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
