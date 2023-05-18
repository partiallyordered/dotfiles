;; TODO:
;; - some form of UI for real-time visualisation of modes etc. egui?
;; - show current layer on the status bar
;; - could implement modal input and indicate mode (normal/input) on the status bar
;; - could plausibly detect changes to focused window class and activate different layers
;;   depending on focused window
;;   - could map ctrl+u to ctrl+backspace for the moderately annoying ssh key passphrase prompt
;;   - Firefox, Signal, Slack, all "web kiosk" applications (and others?)
;;     - basically implement "readline" "insert mode" mapping
;;       - map ctrl+w to ctrl+backspace
;;       - map ctrl+e to end
;;       - map ctrl+a to home (or would this be annoying?)
;;       - map ctrl+u to (macro shift-home backspace)
;;       - map ctrl+y to (I can't remember?)
;;       - map ctrl+h to backspace
;;       - map ctrl+k to (macro shift-end backspace)
;;     - add a keybinding to revert to normal keybindings, i.e. not the "pseudo-readline"
;;       keybindings described here
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
;; - Consider mapping the semicolon key to tap-hold so that when it's held it acts like a "vim
;;   key" and emits vim control sequences. For example, holding semicolon and pressing x could emit
;;   :x

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
  cap (tap-hold 50 150 esc (caps-word 2000))
  ;; tap-hold-press is working fairly well with the shift keys- I don't recall why I'm using
  ;; tap-hold-release with the ctrl keys. The interaction between the two is tricky.
  rcl (tap-hold-press 1 150 / rctl)
  lcl (tap-hold-press 1 150 102d lctl)

  ;; tricky to get the timing right here because whenever space is accidentally held with another
  ;; character (than the ones we intend to emit from the layer) space is not emitted at all
  ;; spa (tap-hold-press 1 100 spc (layer-while-held sym-layer))

  ;; doesn't much accidentally emit the wrong character but
  ;; - when the hold delay is high (200ms!) it's sure possible to really feel the delay before the
  ;;   space is actually emitted, and this makes typing feel really janky
  ;; - when the hold delay is low (100ms) it's difficult to actually "tap" space- because we're
  ;;   "holding" space within 100ms!
  ;; - increasing the tap delay seems to improve usage but not jankiness (haven't understood the
  ;;   tap delay well)
  spa (tap-hold 150 150 spc (layer-while-held sym-layer))

  ;; works perfectly for emitting parentheses as I expect, but
  ;; - holding the space bar no longer emits multiple spaces
  ;; - starting a word with a capital letter doesn't go well
  ;; spa (tap-hold-press 1 200 spc (layer-while-held sym-layer-three))

  ;; spa (layer-while-held sym-layer) ;; space actually doesn't emit space at all

  ;; spa (multi spc (layer-while-held sym-layer-two)) ;; still emits a space before emitting the thing from sym-layer- also sometimes accidentally holding space when I'd prefer not to be.. perhaps this is a matter of practice..
  ;; lpr (multi bspc S-9)
  ;; rpr (multi bspc S-0)

  ;; the chorded solution sorta works but has the problems:
  ;; - attempting to produce an additional output (i.e. holding the space bar and pressing "d"
  ;;   repeatedly) does not work, and instead seems to produce an upper-case d
  ;; - every time a space is typed there is a delay (I guess the chord delay)
  ;; - the same problem the other solutions have, of accidental triggering- though it seems it's
  ;;   possible to adapt to this
  ;; - the additional problem of accidental "reverse" triggering, where there is, for example, a
  ;;   "d" at the end of a word, which, when pressed at the same time as the space bar, causes an
  ;;   opening bracket to be emitted.
  ;; assign @chd to d, @chf to f, @chs to space
  ;; chs (chord syms spc)
  ;; chd (chord syms d)
  ;; chf (chord syms f)

  ;; when assigned to the "d" key, emits the opening brace *after* the space has been emitted
  ;; dfk (fork d S-9 (spc))

  ;; - can only emit a single opening parenthesis (subsequent parentheses are backspaced)
  ;; - common accidental activation
  ;; - does not have the "responsiveness" problem the "tap-hold-*" implementations have
  ;; - easier to define than the chorded solution
  ;; dfk (fork d (macro bspc 1 S-9) (spc)) ;; when assigned to the "d" key, emits the opening brace *after* the space has been emitted
)

;; (defchords syms 150
;;   (spc d  ) S-9
;;   (spc   f) S-0
;;   (spc    ) spc
;;   (    d  ) d
;;   (      f) f
;; )

(deflayer qwerty
  grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab  q    w    e    r    t    y    u    i    o    p    [    ]
  @cap a    s    d    f    g    h    j    k    l    ;    \    ret
  lsft @lcl z    x    c    v    b    n    m    ,    .    @rcl rsft
  lctl lmet alt            spc                 ralt rctl
)

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

;; TODO: consider putting accented characters here also
(deflayer sym-layer
  _    _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _    S-9  S-0  _    _    S-[  S-]  _    _    _    _
  _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _              _                   _    _
)
;; (deflayer sym-layer-two
;;   _    _    _    _    _    _    _    _    _    _    _    _    _    _
;;   _    _    _    _    _    _    _    _    _    _    _    _    _
;;   _    _    _    @lpr @rpr _    _    S-[  S-]  _    _    _    _
;;   _    _    _    _    _    _    _    _    _    _    _    _    _
;;   _    _    _              _                   _    _
;; )
(defalias
  1 1
  2 2
  3 3
  4 4
  5 5
  6 6
  7 7
  8 8
  9 9
  0 0
)
(deflayer sym-layer-three
  (macro spc 1 grv)  (macro spc 1 @1)   (macro spc 1 @2)   (macro spc 1 @3)   (macro spc 1 @4)   (macro spc 1 @5)   (macro spc 1 @6)   (macro spc 1 @7)   (macro spc 1 @8)   (macro spc 1 @9)   (macro spc 1 @0)   (macro spc 1 -)    (macro spc 1 =)    (macro spc 1 bspc)
  (macro spc 1 tab)  (macro spc 1 q)    (macro spc 1 w)    (macro spc 1 e)    (macro spc 1 r)    (macro spc 1 t)    (macro spc 1 y)    (macro spc 1 u)    (macro spc 1 i)    (macro spc 1 o)    (macro spc 1 p)    (macro spc 1 [)    (macro spc 1 ])
  (macro spc 1 caps) (macro spc 1 a)    (macro spc 1 s)    S-9                S-0                (macro spc 1 g)    (macro spc 1 h)    S-[                S-]                (macro spc 1 l)    (macro spc 1 ;)    (macro spc 1 \)    (macro spc 1 ret)
  (macro spc 1 lsft) (macro spc 1 lctl) (macro spc 1 z)    (macro spc 1 x)    (macro spc 1 c)    (macro spc 1 v)    (macro spc 1 b)    (macro spc 1 n)    (macro spc 1 m)    (macro spc 1 ,)    (macro spc 1 .)    (macro spc 1 rctl) (macro spc 1 rsft)
  (macro spc 1 lctl) (macro spc 1 lmet)                    (macro spc 1 alt)  spc                                                                                            (macro spc 1 ralt) (macro spc 1 rctl)
)
