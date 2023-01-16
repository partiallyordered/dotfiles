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
  rcl (tap-hold 200 200 / rctl)
  lcl (tap-hold 200 200 102d lctl)
  atl (multi alt (layer-while-held hold-layer))
  rct (multi rctl (layer-while-held hold-layer))
  lct (multi lctl (layer-while-held hold-layer))
  rsh (multi rsft (layer-while-held hold-layer))
  lsh (multi lsft (layer-while-held hold-layer))
  ;; This fd key-chord can also be achieved with a layer mapped to macros. See this commit for
  ;; details: 679c733d007f3cd52f916003307f516ac83c1b1f
  eff (tap-hold-release 200 200 sldr f)
)

(deflayer qwerty
  grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab  q    w    e    r    t    y    u    i    o    p    [    ]
  @cap a    s    d    @eff g    h    j    k    l    ;    \    ret
  @lsh @lcl z    x    c    v    b    n    m    ,    .    @rcl @rsh
  @lct lmet @atl           spc                 ralt @rct
)

;; TODO: (deflayer caps-word) ;; do something 

(deflayer hold-layer
  _    _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _    _    f    _    _    _    _    _    _    _    _
  _    _    _    _    _    _    _    _    _    _    _    _    _
  _    _    _              _                   _    _
)

(defseq fd-escape (d))

(defseq fgrv-seq (grv))
(defseq f1-seq (1))
(defseq f2-seq (2))
(defseq f3-seq (3))
(defseq f4-seq (4))
(defseq f5-seq (5))
(defseq f6-seq (6))
(defseq f7-seq (7))
(defseq f8-seq (8))
(defseq f9-seq (9))
(defseq f0-seq (0))
(defseq f-dash-seq (-))
(defseq f-equals-seq (=))
(defseq fbspc-seq (bspc))
(defseq ftab-seq (tab))
(defseq fq-seq (q))
(defseq fw-seq (w))
(defseq fe-seq (e))
(defseq fr-seq (r))
(defseq ft-seq (t))
(defseq fy-seq (y))
(defseq fu-seq (u))
(defseq fi-seq (i))
(defseq fo-seq (o))
(defseq fp-seq (p))
(defseq f-open-bracket-seq ([))
(defseq f-close-bracket-seq (]))
(defseq fcaps-seq (caps))
(defseq fa-seq (a))
(defseq fs-seq (s))
(defseq ff-seq (f))
(defseq fg-seq (g))
(defseq fh-seq (h))
(defseq fj-seq (j))
(defseq fk-seq (k))
(defseq fl-seq (l))
(defseq f-semicolon-seq (;))
(defseq f-backslash-seq (\))
(defseq fret-seq (ret))
(defseq flsft-seq (lsft))
(defseq fz-seq (z))
(defseq fx-seq (x))
(defseq fc-seq (c))
(defseq fv-seq (v))
(defseq fb-seq (b))
(defseq fn-seq (n))
(defseq fm-seq (m))
(defseq f-comma-seq (,))
(defseq f-period-seq (.))
(defseq f-forward-slash-seq (/))
(defseq frsft-seq (rsft))
(defseq flctl-seq (lctl))
(defseq flmet-seq (lmet))
(defseq flalt-seq (lalt))
(defseq fspc-seq (spc))
(defseq fralt-seq (ralt))
(defseq frctl-seq (rctl))

(deffakekeys fd-escape (macro esc))

(deffakekeys fgrv-seq (macro f grv))
(deffakekeys f1-seq (macro f 1))
(deffakekeys f2-seq (macro f 2))
(deffakekeys f3-seq (macro f 3))
(deffakekeys f4-seq (macro f 4))
(deffakekeys f5-seq (macro f 5))
(deffakekeys f6-seq (macro f 6))
(deffakekeys f7-seq (macro f 7))
(deffakekeys f8-seq (macro f 8))
(deffakekeys f9-seq (macro f 9))
(deffakekeys f0-seq (macro f 0))
(deffakekeys f-dash-seq (macro f -))
(deffakekeys f-equals-seq (macro f =))
(deffakekeys fbspc-seq (macro f bspc))
(deffakekeys ftab-seq (macro f tab))
(deffakekeys fq-seq (macro f q))
(deffakekeys fw-seq (macro f w))
(deffakekeys fe-seq (macro f e))
(deffakekeys fr-seq (macro f r))
(deffakekeys ft-seq (macro f t))
(deffakekeys fy-seq (macro f y))
(deffakekeys fu-seq (macro f u))
(deffakekeys fi-seq (macro f i))
(deffakekeys fo-seq (macro f o))
(deffakekeys fp-seq (macro f p))
(deffakekeys f-open-bracket-seq (macro f [))
(deffakekeys f-close-bracket-seq (macro f ]))
(deffakekeys fcaps-seq (macro f caps))
(deffakekeys fa-seq (macro f a))
(deffakekeys fs-seq (macro f s))
(deffakekeys ff-seq (macro f f))
(deffakekeys fg-seq (macro f g))
(deffakekeys fh-seq (macro f h))
(deffakekeys fj-seq (macro f j))
(deffakekeys fk-seq (macro f k))
(deffakekeys fl-seq (macro f l))
(deffakekeys f-semicolon-seq (macro f ;))
(deffakekeys f-backslash-seq (macro f \))
(deffakekeys fret-seq (macro f ret))
(deffakekeys flsft-seq (macro f lsft))
(deffakekeys fz-seq (macro f z))
(deffakekeys fx-seq (macro f x))
(deffakekeys fc-seq (macro f c))
(deffakekeys fv-seq (macro f v))
(deffakekeys fb-seq (macro f b))
(deffakekeys fn-seq (macro f n))
(deffakekeys fm-seq (macro f m))
(deffakekeys f-comma-seq (macro f ,))
(deffakekeys f-period-seq (macro f .))
(deffakekeys f-forward-slash-seq (macro f /))
(deffakekeys frsft-seq (macro f rsft))
(deffakekeys flctl-seq (macro f lctl))
(deffakekeys flmet-seq (macro f lmet))
(deffakekeys flalt-seq (macro f lalt))
(deffakekeys fspc-seq (macro f spc))
(deffakekeys fralt-seq (macro f ralt))
(deffakekeys frctl-seq (macro f rctl))
