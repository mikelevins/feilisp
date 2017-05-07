;;; building stems for Fei
;;; gen-binary at bottom combines two random stems to create a word

(defparameter $all-vowels '(i u o e a))

(defparameter $all-diphthongs '(ie ia iu ui ue uo ua ei eu eo ea ou oe ou oe oa ai au ae ao))

(defparameter $all-vowels-and-diphthongs (append $all-vowels $all-diphthongs))

(defparameter $all-consonants
  '(b p d t g c
    m n ng
    f v th dh s lh sh ch *
    rh r
    l y wh w))

(defparameter $all-consonant-clusters
  '(br bw bl bn              rb wb mb
    pr p* pw ps pl pn        rp *p wp sp mp 
    dr dw dl dn              rd wd ld nd
    tr t* tw ts tl tn        rt *t wt lt nt
    gr gw gl gn              rg wg lg ngg
    cr c* cw cs cl cn        rc *c wc sc lc ngc  
    mw mn                    rm wm lm
    nw                       rn wn ln mn
    ngw                      rng wng
    fr f* fw fl fm fn        rf *f wf lf
    vr vw vl vm vn           rv wv lv
    thr thw thl thm thn      rth wth sth lth
    dhr dhw dhl dhm dhn      rdh wdh ldh
    sr s* sw sl sm sn        rs *s ws ls ms ns ngs
    lhw                      
    shr sh* shw shl shm shn  rsh wsh lsh nsh ngsh
    chr chw                  rch hch wch sch lch ngch  
    *r
                             wrh srh
    rn                       wr
    lw ln                    rl wl
                             ry sy
    whr whs
    wn                       rw))

(defparameter $initial-consonants
  '(b p d t g c m n ng f v th dh s lh sh ch * rh r l y wh w
    br bw bl pr p* pw ps pl pn dr dw dl tr t* tw ts
    gr gw gl cr c* cw cs cl mw mn nw ngw fr f* fw fl fm fn
    vr vw vl vm vn thr thw thl thm thn dhr dhw dh dhl dhm dhn
    sr s* sw sl sm sn lhw shr sh* shw shl shm shn
    chr chw *r lw))

(defparameter $final-consonants
  '(t c m n ng f th s sh ch rh r l w y
    rt *t wt lt nt
    rc *c wc sc lc ngc cs rm wm lm rn wn ln mn
    rng wng rf wf lf rth wth sth lth
    rs ws ls ms ns ngs
    rsh wsh lsh nsh ngsh
    rch wch sch lch ngch wr rl rw))

(defun all-vc-stems ()
  (let* ((V (append $all-vowels $all-diphthongs))
         (VC (loop for vowel in V
                collect (loop for c in $final-consonants
                           collect (list vowel c))))
         (stems (reduce #'append VC))
         (stem-stringlists (mapcar (lambda (s)(mapcar #'symbol-name s))
                                   stems))
         (stem-strings (mapcar (lambda (x)(reduce (lambda (y z)(concatenate 'string y z)) x))
                               stem-stringlists)))
    (mapcar #'string-downcase stem-strings)))

(defun all-cvc-stems ()
  (let* ((V (append $all-vowels $all-diphthongs))
         (CVC (loop for c1 in $initial-consonants
                 collect (loop for vowel in V
                            collect (loop for c2 in $final-consonants
                                       collect (list c1 vowel c2)))))
         (stems (reduce #'append (reduce #'append CVC)))
         (stem-stringlists (mapcar (lambda (s)(mapcar #'symbol-name s))
                                   stems))
         (stem-strings (mapcar (lambda (x)(reduce (lambda (y z)(concatenate 'string y z)) x))
                               stem-stringlists)))
    (mapcar #'string-downcase stem-strings)))

(defparameter $all-cvc-stems nil)

(defun any-cvc-stem ()
  (unless $all-cvc-stems
    (setf $all-cvc-stems
          (all-cvc-stems)))
  (elt $all-cvc-stems
       (random (length $all-cvc-stems))))

(defun n-cvc-stems (n)
  (loop for i from 0 below n collect (any-cvc-stem)))


(defmethod symbol-text ((s symbol))
  (string-downcase (symbol-name s)))

(defmethod symbol< ((s1 symbol)(s2 symbol))
  (string< (symbol-text s1)
           (symbol-text s2)))

;;; all the base stems
(defparameter $stems
  '(bael
    baer
    bearc
    bel
    berc
    brel
    broarh
    bros
    broulh
    bróth
    buec
    bár
    ceoth
    clauth
    cliw
    coalh
    cream
    criw
    crol
    cruacs
    cruarc
    cruich
    cuel
    cuolh
    curc
    cweith
    cweth
    cáth
    def
    dhalc
    dhmaey
    dhwais
    dhúm
    dorc
    drom
    druich
    dán
    fein
    feit
    fell
    feor
    foeth
    fown
    frow
    fuosh
    fál
    féy
    fíth
    fúw
    glaur
    gloch
    grouc
    heir
    hraom
    hraoth
    hálth
    híw
    laer
    laolh
    leash
    lelt
    lew
    lhoc
    lhouth
    lhuics
    lhuw
    loal
    louc
    lwaef
    lwain
    lwel
    lách
    lálh
    lánn
    láth
    lím
    lín
    maol
    maor
    mauth
    mell
    moar
    mol
    mout
    mow
    mun
    mwaoth
    mách
    máng
    máth
    neim
    ngol
    nierh
    noeth
    nám
    nícs
    nól
    pen
    perch
    prauch
    pál
    pín
    púc
    raesh
    reith
    rhaim
    rhuann
    rhuw
    routh
    ránn
    róf
    sel
    shouc
    soalt
    soen
    sor
    soum
    sueth
    taw
    teith
    ter
    thacs
    thalc
    thiuth
    thuin
    tháll
    thánn
    thás
    thúr
    trerh
    tuish
    tál
    vaum
    vec
    vel
    vlír
    ván
    weas
    welh
    won
    yúw))

(defmethod match-prefix ((p symbol)(s symbol))
  (let* ((p-text (symbol-text p))
         (s-text (symbol-text s))
         (len (length p-text)))
    (if (< (length s-text) len)
        nil
        (equal p-text (subseq s-text 0 len)))))

(defmethod find-prefix ((p symbol) (ls cons))
  (remove-if-not (lambda (x)(match-prefix x p)) ls))

(defmethod stem-start ((s symbol))
  (let* ((dps (find-prefix s $all-diphthongs))
         (vws (find-prefix s $all-vowels))
         (cls (find-prefix s $all-consonant-clusters))
         (cns (find-prefix s $all-consonants))
         (all (sort (append dps vws cls cns)
                    (lambda (x y)(> (length (symbol-text x))
                                    (length (symbol-text y)))))))
    (first all)))

(defparameter $stem-starts
  (sort (remove-duplicates (mapcar #'stem-start $stems))
        #'symbol<))


(defmethod match-suffix ((sym symbol)(suf symbol))
  (let* ((suf-text (symbol-text suf))
         (sym-text (symbol-text sym))
         (suf-len (length suf-text))
         (sym-len (length sym-text)))
    (if (< sym-len suf-len)
        nil
        (equal suf-text
               (subseq sym-text (- sym-len suf-len))))))

(defmethod find-suffix ((s symbol) (ls cons))
  (remove-if-not (lambda (x)(match-suffix s x)) ls))

(defmethod stem-end ((s symbol))
  (let* ((dps (find-suffix s $all-diphthongs))
         (vws (find-suffix s $all-vowels))
         (cls (find-suffix s $all-consonant-clusters))
         (cns (find-suffix s $all-consonants))
         (all (sort (append dps vws cls cns)
                    (lambda (x y)(> (length (symbol-text x))
                                    (length (symbol-text y)))))))
    (first all)))

(defparameter $stem-ends
  (sort (remove-duplicates (mapcar #'stem-end $stems))
        #'symbol<))

;;; combining mutations
;;; consonants and clusters can combine when agglutinating base predicates.
;;; when they do, the resulting consonant clusters must belong to $all-consonant-clusters
;;; that's not true for all possible results of combining, and so combining rules
;;; must be applied in order to mutate the resulting clusters into allowed clusters.
;;; the following functions accomplish that.


;;; compute all the possible combinations so we can construct transformations for them
(defparameter $all-consonant-pairs
  (remove-if (lambda (x)(some #'null x))
             (reduce #'append
                     (loop for end in $stem-ends
                        collect (loop for start in $stem-starts
                                   collect (list end start))))))


;;; (let ((*print-case* :downcase))(loop for comb in $all-consonant-pairs do (format t "~%~{((~S ~S) . )~}" comb)))

(defmethod combine ((c1 symbol)(c2 symbol))
  (let* ((pair (list c1 c2))
         (entry (assoc pair $all-consonant-combinations :test #'equal)))
    (when entry (cdr entry))))

;;; to combine consonants, look them up in this list
(defparameter $all-consonant-combinations
  '(((c b) . cf)
    ((c br) . cr)
    ((c c) . cc)
    ((c cl) . cl)
    ((c cr) . cr)
    ((c cw) . cw)
    ((c d) . ct)
    ((c dh) . th)
    ((c dhm) . thm)
    ((c dhw) . thw)
    ((c dr) . thr)
    ((c f) . f)
    ((c fr) . fr)
    ((c gl) . cl)
    ((c gr) . cr)
    ((c l) . cl)
    ((c lh) . clh)
    ((c lw) . cw)
    ((c m) . cm)
    ((c mw) . cw)
    ((c n) . cn)
    ((c ng) . ng)
    ((c p) . cf)
    ((c pr) . cr)
    ((c r) . cr)
    ((c rh) . crh)
    ((c s) . cs)
    ((c sh) . csh)
    ((c t) . ct)
    ((c th) . cth)
    ((c tr) . cr)
    ((c v) . cf)
    ((c vl) . cl)
    ((c w) . cw)
    ((c y) . cy)
    ((ch b) . chf)
    ((ch br) . chfr)
    ((ch c) . ch)
    ((ch cl) . chl)
    ((ch cr) . chr)
    ((ch cw) . chw)
    ((ch d) . cht)
    ((ch dh) . chth)
    ((ch dhm) . chm)
    ((ch dhw) . chw)
    ((ch dr) . chr)
    ((ch f) . chf)
    ((ch fr) . chfr)
    ((ch gl) . chl)
    ((ch gr) . chr)
    ((ch l) . chl)
    ((ch lh) . chlh)
    ((ch lw) . chw)
    ((ch m) . chm)
    ((ch mw) . chw)
    ((ch n) . chn)
    ((ch ng) . ng)
    ((ch p) . chf)
    ((ch pr) . chr)
    ((ch r) . chr)
    ((ch rh) . chrh)
    ((ch s) . chs)
    ((ch sh) . chs)
    ((ch t) . chth)
    ((ch th) . chth)
    ((ch tr) . chr)
    ((ch v) . chf)
    ((ch vl) . chl)
    ((ch w) . chw)
    ((ch y) . chy)
    ((cs b) . chf)
    ((cs br) . chr)
    ((cs c) . ch)
    ((cs cl) . chl)
    ((cs cr) . chr)
    ((cs cw) . chw)
    ((cs d) . chth)
    ((cs dh) . chth)
    ((cs dhm) . chm)
    ((cs dhw) . chw)
    ((cs dr) . chr)
    ((cs f) . chf)
    ((cs fr) . chr)
    ((cs gl) . chl)
    ((cs gr) . chr)
    ((cs l) . chl)
    ((cs lh) . chlh)
    ((cs lw) . chw)
    ((cs m) . chm)
    ((cs mw) . chw)
    ((cs n) . chn)
    ((cs ng) . ng)
    ((cs p) . chf)
    ((cs pr) . chr)
    ((cs r) . chr)
    ((cs rh) . chrh)
    ((cs s) . chs)
    ((cs sh) . chsh)
    ((cs t) . chth)
    ((cs th) . chth)
    ((cs tr) . chr)
    ((cs v) . chf)
    ((cs vl) . chl)
    ((cs w) . chw)
    ((cs y) . chy)
    ((f b) . v)
    ((f br) . vr)
    ((f c) . f)
    ((f cl) . fl)
    ((f cr) . fr)
    ((f cw) . fw)
    ((f d) . dh)
    ((f dh) . dh)
    ((f dhm) . dhm)
    ((f dhw) . dhw)
    ((f dr) . dhr)
    ((f f) . f)
    ((f fr) . fr)
    ((f gl) . fl)
    ((f gr) . fr)
    ((f l) . fl)
    ((f lh) . fl)
    ((f lw) . lhw)
    ((f m) . fm)
    ((f mw) . fw)
    ((f n) . fn)
    ((f ng) . ng)
    ((f p) . f)
    ((f pr) . fr)
    ((f r) . fr)
    ((f rh) . frh)
    ((f s) . ss)
    ((f sh) . sh)
    ((f t) . th)
    ((f th) . th)
    ((f tr) . fr)
    ((f v) . v)
    ((f vl) . vl)
    ((f w) . fwh)
    ((f y) . fy)
    ((l b) . lv)
    ((l br) . lr)
    ((l c) . lg)
    ((l cl) . ll)
    ((l cr) . rr)
    ((l cw) . lw)
    ((l d) . ldh)
    ((l dh) . ldh)
    ((l dhm) . lm)
    ((l dhw) . lw)
    ((l dr) . lr)
    ((l f) . lv)
    ((l fr) . lr)
    ((l gl) . ll)
    ((l gr) . lr)
    ((l l) . ll)
    ((l lh) . lh)
    ((l lw) . lw)
    ((l m) . lm)
    ((l mw) . lw)
    ((l n) . ln)
    ((l ng) . ng)
    ((l p) . lf)
    ((l pr) . lr)
    ((l r) . lr)
    ((l rh) . lr)
    ((l s) . ls)
    ((l sh) . lsh)
    ((l t) . lth)
    ((l th) . lth)
    ((l tr) . lr)
    ((l v) . lv)
    ((l vl) . ll)
    ((l w) . lw)
    ((l y) . ly)
    ((lc b) . lv)
    ((lc br) . lr)
    ((lc c) . lch)
    ((lc cl) . ll)
    ((lc cr) . lr)
    ((lc cw) . lw)
    ((lc d) . ldh)
    ((lc dh) . ldh)
    ((lc dhm) . lm)
    ((lc dhw) . lw)
    ((lc dr) . lr)
    ((lc f) . lv)
    ((lc fr) . lr)
    ((lc gl) . ll)
    ((lc gr) . lr)
    ((lc l) . ll)
    ((lc lh) . lh)
    ((lc lw) . lw)
    ((lc m) . lm)
    ((lc mw) . lw)
    ((lc n) . ln)
    ((lc ng) . ng)
    ((lc p) . lf)
    ((lc pr) . lr)
    ((lc r) . lr)
    ((lc rh) . lrh)
    ((lc s) . ls)
    ((lc sh) . lsh)
    ((lc t) . lth)
    ((lc th) . lth)
    ((lc tr) . lr)
    ((lc v) . lv)
    ((lc vl) . ll)
    ((lc w) . lw)
    ((lc y) . ly)
    ((lh b) . lv)
    ((lh br) . lr)
    ((lh c) . lch)
    ((lh cl) . ll)
    ((lh cr) . lt)
    ((lh cw) . lw)
    ((lh d) . ldh)
    ((lh dh) . ldh)
    ((lh dhm) . lm)
    ((lh dhw) . lw)
    ((lh dr) . lr)
    ((lh f) . lf)
    ((lh fr) . lr)
    ((lh gl) . ll)
    ((lh gr) . lr)
    ((lh l) . ll)
    ((lh lh) . lh)
    ((lh lw) . lw)
    ((lh m) . lm)
    ((lh mw) . lw)
    ((lh n) . ln)
    ((lh ng) . ln)
    ((lh p) . lp)
    ((lh pr) . lr)
    ((lh r) . lr)
    ((lh rh) . lrh)
    ((lh s) . ls)
    ((lh sh) . lsh)
    ((lh t) . lth)
    ((lh th) . lth)
    ((lh tr) . lr)
    ((lh v) . lv)
    ((lh vl) . ll)
    ((lh w) . lw)
    ((lh y) . ly)
    ((lt b) . lv)
    ((lt br) . lr)
    ((lt c) . lch)
    ((lt cl) . ll)
    ((lt cr) . lr)
    ((lt cw) . lw)
    ((lt d) . ldh)
    ((lt dh) . ldh)
    ((lt dhm) . lm)
    ((lt dhw) . lw)
    ((lt dr) . lr)
    ((lt f) . lf)
    ((lt fr) . lr)
    ((lt gl) . ll)
    ((lt gr) . lr)
    ((lt l) . ll)
    ((lt lh) . lh)
    ((lt lw) . lw)
    ((lt m) . lm)
    ((lt mw) . lw)
    ((lt n) . ln)
    ((lt ng) . ln)
    ((lt p) . lf)
    ((lt pr) . lr)
    ((lt r) . lr)
    ((lt rh) . lrh)
    ((lt s) . ls)
    ((lt sh) . lsh)
    ((lt t) . lth)
    ((lt th) . lth)
    ((lt tr) . lr)
    ((lt v) . lv)
    ((lt vl) . ll)
    ((lt w) . lw)
    ((lt y) . ly)
    ((lth b) . lv)
    ((lth br) . lr)
    ((lth c) . lch)
    ((lth cl) . ll)
    ((lth cr) . lr)
    ((lth cw) . lw)
    ((lth d) . ldh)
    ((lth dh) . ldh)
    ((lth dhm) . lm)
    ((lth dhw) . lw)
    ((lth dr) . lr)
    ((lth f) . lf)
    ((lth fr) . lr)
    ((lth gl) . ll)
    ((lth gr) . lr)
    ((lth l) . ll)
    ((lth lh) . ll)
    ((lth lw) . lw)
    ((lth m) . lm)
    ((lth mw) . lw)
    ((lth n) . ln)
    ((lth ng) . ln)
    ((lth p) . lf)
    ((lth pr) . lr)
    ((lth r) . lr)
    ((lth rh) . lrh)
    ((lth s) . ls)
    ((lth sh) . lsh)
    ((lth t) . lth)
    ((lth th) . lth)
    ((lth tr) . lr)
    ((lth v) . lv)
    ((lth vl) . ll)
    ((lth w) . lw)
    ((lth y) . ly)
    ((m b) . mv)
    ((m br) . mr)
    ((m c) . mg)
    ((m cl) . ml)
    ((m cr) . mr)
    ((m cw) . mw)
    ((m d) . mb)
    ((m dh) . mv)
    ((m dhm) . mm)
    ((m dhw) . mw)
    ((m dr) . mr)
    ((m f) . mv)
    ((m fr) . mr)
    ((m gl) . ml)
    ((m gr) . mr)
    ((m l) . ml)
    ((m lh) . ml)
    ((m lw) . mw)
    ((m m) . mm)
    ((m mw) . mw)
    ((m n) . mn)
    ((m ng) . ng)
    ((m p) . mv)
    ((m pr) . mr)
    ((m r) . mr)
    ((m rh) . mr)
    ((m s) . ms)
    ((m sh) . msh)
    ((m t) . mdh)
    ((m th) . mdh)
    ((m tr) . mr)
    ((m v) . mv)
    ((m vl) . ml)
    ((m w) . mw)
    ((m y) . my)
    ((n b) . mv)
    ((n br) . mr)
    ((n c) . ngg)
    ((n cl) . ngl)
    ((n cr) . ngr)
    ((n cw) . ngw)
    ((n d) . ndh)
    ((n dh) . ndh)
    ((n dhm) . nm)
    ((n dhw) . nw)
    ((n dr) . nr)
    ((n f) . mv)
    ((n fr) . nr)
    ((n gl) . nl)
    ((n gr) . nr)
    ((n l) . nl)
    ((n lh) . nl)
    ((n lw) . nw)
    ((n m) . mm)
    ((n mw) . mw)
    ((n n) . nn)
    ((n ng) . ng)
    ((n p) . mv)
    ((n pr) . mr)
    ((n r) . nr)
    ((n rh) . nrh)
    ((n s) . ns)
    ((n sh) . nsh)
    ((n t) . nth)
    ((n th) . nth)
    ((n tr) . nr)
    ((n v) . mv)
    ((n vl) . nl)
    ((n w) . nw)
    ((n y) . ny)
    ((ng b) . mv)
    ((ng br) . mr)
    ((ng c) . ngc)
    ((ng cl) . ngl)
    ((ng cr) . ngr)
    ((ng cw) . ngw)
    ((ng d) . ngdh)
    ((ng dh) . ngdh)
    ((ng dhm) . ngm)
    ((ng dhw) . ngw)
    ((ng dr) . ngr)
    ((ng f) . ngv)
    ((ng fr) . ngr)
    ((ng gl) . ngl)
    ((ng gr) . ngr)
    ((ng l) . ngl)
    ((ng lh) . ngl)
    ((ng lw) . ngw)
    ((ng m) . ngm)
    ((ng mw) . ngw)
    ((ng n) . ngn)
    ((ng ng) . ngg)
    ((ng p) . ngf)
    ((ng pr) . ngr)
    ((ng r) . ngr)
    ((ng rh) . ngrh)
    ((ng s) . ngs)
    ((ng sh) . ngsh)
    ((ng t) . ngth)
    ((ng th) . ngth)
    ((ng tr) . ngr)
    ((ng v) . ngv)
    ((ng vl) . ngl)
    ((ng w) . ngw)
    ((ng y) . ngy)
    ((r b) . rv)
    ((r br) . rr)
    ((r c) . rc)
    ((r cl) . rcl)
    ((r cr) . rc)
    ((r cw) . rcw)
    ((r d) . rd)
    ((r dh) . rdh)
    ((r dhm) . rm)
    ((r dhw) . rw)
    ((r dr) . rr)
    ((r f) . rv)
    ((r fr) . rr)
    ((r gl) . rgl)
    ((r gr) . rr)
    ((r l) . rl)
    ((r lh) . rlh)
    ((r lw) . rw)
    ((r m) . rm)
    ((r mw) . rw)
    ((r n) . rn)
    ((r ng) . rng)
    ((r p) . rf)
    ((r pr) . rr)
    ((r r) . rr)
    ((r rh) . rr)
    ((r s) . rs)
    ((r sh) . rsh)
    ((r t) . rt)
    ((r th) . rth)
    ((r tr) . rr)
    ((r v) . rv)
    ((r vl) . rl)
    ((r w) . rw)
    ((r y) . ry)
    ((rc b) . rv)
    ((rc br) . rr)
    ((rc c) . rc)
    ((rc cl) . rcl)
    ((rc cr) . rr)
    ((rc cw) . rw)
    ((rc d) . rd)
    ((rc dh) . rdh)
    ((rc dhm) . rm)
    ((rc dhw) . rw)
    ((rc dr) . rr)
    ((rc f) . rf)
    ((rc fr) . rr)
    ((rc gl) . rl)
    ((rc gr) . rr)
    ((rc l) . rl)
    ((rc lh) . rlh)
    ((rc lw) . rw)
    ((rc m) . rm)
    ((rc mw) . rw)
    ((rc n) . rn)
    ((rc ng) . rng)
    ((rc p) . rf)
    ((rc pr) . rr)
    ((rc r) . rr)
    ((rc rh) . rr)
    ((rc s) . rs)
    ((rc sh) . rsh)
    ((rc t) . rt)
    ((rc th) . rth)
    ((rc tr) . rr)
    ((rc v) . rv)
    ((rc vl) . rl)
    ((rc w) . rw)
    ((rc y) . ry)
    ((rch b) . rv)
    ((rch br) . rr)
    ((rch c) . rc)
    ((rch cl) . rl)
    ((rch cr) . rr)
    ((rch cw) . rw)
    ((rch d) . rd)
    ((rch dh) . rdh)
    ((rch dhm) . rm)
    ((rch dhw) . rw)
    ((rch dr) . rr)
    ((rch f) . rf)
    ((rch fr) . rr)
    ((rch gl) . rl)
    ((rch gr) . rr)
    ((rch l) . rl)
    ((rch lh) . rlh)
    ((rch lw) . rw)
    ((rch m) . rm)
    ((rch mw) . rw)
    ((rch n) . rn)
    ((rch ng) . rng)
    ((rch p) . rf)
    ((rch pr) . rr)
    ((rch r) . rr)
    ((rch rh) . rr)
    ((rch s) . rs)
    ((rch sh) . rsh)
    ((rch t) . rt)
    ((rch th) . rth)
    ((rch tr) . rr)
    ((rch v) . rv)
    ((rch vl) . rl)
    ((rch w) . rw)
    ((rch y) . ry)
    ((rh b) . rv)
    ((rh br) . rr)
    ((rh c) . rc)
    ((rh cl) . rl)
    ((rh cr) . rr)
    ((rh cw) . rw)
    ((rh d) . rd)
    ((rh dh) . rdh)
    ((rh dhm) . rm)
    ((rh dhw) . rw)
    ((rh dr) . rr)
    ((rh f) . rf)
    ((rh fr) . rr)
    ((rh gl) . rl)
    ((rh gr) . rr)
    ((rh l) . rl)
    ((rh lh) . rlh)
    ((rh lw) . rw)
    ((rh m) . rm)
    ((rh mw) . rw)
    ((rh n) . rn)
    ((rh ng) . rng)
    ((rh p) . rf)
    ((rh pr) . rr)
    ((rh r) . rr)
    ((rh rh) . rr)
    ((rh s) . rs)
    ((rh sh) . rsh)
    ((rh t) . rt)
    ((rh th) . rth)
    ((rh tr) . rr)
    ((rh v) . rv)
    ((rh vl) . rl)
    ((rh w) . rw)
    ((rh y) . ry)
    ((s b) . sp)
    ((s br) . sr)
    ((s c) . sc)
    ((s cl) . sl)
    ((s cr) . sr)
    ((s cw) . sw)
    ((s d) . st)
    ((s dh) . sth)
    ((s dhm) . sm)
    ((s dhw) . sw)
    ((s dr) . sr)
    ((s f) . sf)
    ((s fr) . sr)
    ((s gl) . sl)
    ((s gr) . sr)
    ((s l) . sl)
    ((s lh) . slh)
    ((s lw) . sw)
    ((s m) . sm)
    ((s mw) . sw)
    ((s n) . sn)
    ((s ng) . sn)
    ((s p) . sp)
    ((s pr) . sr)
    ((s r) . sr)
    ((s rh) . srh)
    ((s s) . ss)
    ((s sh) . sh)
    ((s t) . sth)
    ((s th) . sth)
    ((s tr) . sr)
    ((s v) . sf)
    ((s vl) . sl)
    ((s w) . sw)
    ((s y) . sy)
    ((sh b) . shf)
    ((sh br) . shr)
    ((sh c) . shch)
    ((sh cl) . shl)
    ((sh cr) . shr)
    ((sh cw) . shw)
    ((sh d) . shth)
    ((sh dh) . shth)
    ((sh dhm) . shm)
    ((sh dhw) . shw)
    ((sh dr) . shr)
    ((sh f) . shf)
    ((sh fr) . shr)
    ((sh gl) . shl)
    ((sh gr) . shr)
    ((sh l) . shl)
    ((sh lh) . shlh)
    ((sh lw) . shw)
    ((sh m) . shm)
    ((sh mw) . shw)
    ((sh n) . shn)
    ((sh ng) . shn)
    ((sh p) . shf)
    ((sh pr) . shr)
    ((sh r) . shr)
    ((sh rh) . shrh)
    ((sh s) . sh)
    ((sh sh) . sh)
    ((sh t) . shth)
    ((sh th) . shth)
    ((sh tr) . shr)
    ((sh v) . shf)
    ((sh vl) . shl)
    ((sh w) . shw)
    ((sh y) . shy)
    ((t b) . tf)
    ((t br) . tr)
    ((t c) . tch)
    ((t cl) . cl)
    ((t cr) . tr)
    ((t cw) . tw)
    ((t d) . dh)
    ((t dh) . dh)
    ((t dhm) . dhm)
    ((t dhw) . dhw)
    ((t dr) . dr)
    ((t f) . tf)
    ((t fr) . tr)
    ((t gl) . cl)
    ((t gr) . tr)
    ((t l) . cl)
    ((t lh) . clh)
    ((t lw) . tw)
    ((t m) . tm)
    ((t mw) . tw)
    ((t n) . tn)
    ((t ng) . tn)
    ((t p) . tf)
    ((t pr) . tr)
    ((t r) . tr)
    ((t rh) . trh)
    ((t s) . ts)
    ((t sh) . tsh)
    ((t t) . th)
    ((t th) . th)
    ((t tr) . tr)
    ((t v) . tf)
    ((t vl) . cl)
    ((t w) . tw)
    ((t y) . ty)
    ((th b) . thf)
    ((th br) . thr)
    ((th c) . thch)
    ((th cl) . thl)
    ((th cr) . thr)
    ((th cw) . thw)
    ((th d) . dh)
    ((th dh) . dh)
    ((th dhm) . dhm)
    ((th dhw) . dhw)
    ((th dr) . dhr)
    ((th f) . ff)
    ((th fr) . fr)
    ((th gl) . thl)
    ((th gr) . thr)
    ((th l) . thl)
    ((th lh) . thlh)
    ((th lw) . thw)
    ((th m) . thm)
    ((th mw) . thw)
    ((th n) . thn)
    ((th ng) . thn)
    ((th p) . thf)
    ((th pr) . thr)
    ((th r) . thr)
    ((th rh) . thrh)
    ((th s) . th)
    ((th sh) . th)
    ((th t) . th)
    ((th th) . th)
    ((th tr) . thr)
    ((th v) . thf)
    ((th vl) . thl)
    ((th w) . thw)
    ((th y) . thy)
    ((w b) . wb)
    ((w br) . wr)
    ((w c) . wg)
    ((w cl) . wl)
    ((w cr) . wr)
    ((w cw) . ww)
    ((w d) . wd)
    ((w dh) . wdh)
    ((w dhm) . wm)
    ((w dhw) . ww)
    ((w dr) . wr)
    ((w f) . wv)
    ((w fr) . wr)
    ((w gl) . wl)
    ((w gr) . wr)
    ((w l) . wl)
    ((w lh) . wl)
    ((w lw) . ww)
    ((w m) . wm)
    ((w mw) . ww)
    ((w n) . wn)
    ((w ng) . wng)
    ((w p) . wv)
    ((w pr) . wr)
    ((w r) . wr)
    ((w rh) . wr)
    ((w s) . ws)
    ((w sh) . wsh)
    ((w t) . wd)
    ((w th) . wdh)
    ((w tr) . wr)
    ((w v) . wv)
    ((w vl) . wl)
    ((w w) . ww)
    ((w y) . ww)
    ((wn b) . wnv)
    ((wn br) . wr)
    ((wn c) . wng)
    ((wn cl) . wl)
    ((wn cr) . wr)
    ((wn cw) . ww)
    ((wn d) . wd)
    ((wn dh) . wdh)
    ((wn dhm) . wm)
    ((wn dhw) . ww)
    ((wn dr) . wr)
    ((wn f) . wv)
    ((wn fr) . wr)
    ((wn gl) . wl)
    ((wn gr) . wr)
    ((wn l) . wl)
    ((wn lh) . wl)
    ((wn lw) . ww)
    ((wn m) . wm)
    ((wn mw) . ww)
    ((wn n) . wn)
    ((wn ng) . wng)
    ((wn p) . wv)
    ((wn pr) . wr)
    ((wn r) . wr)
    ((wn rh) . wr)
    ((wn s) . ws)
    ((wn sh) . wsh)
    ((wn t) . wd)
    ((wn th) . wdh)
    ((wn tr) . wr)
    ((wn v) . wv)
    ((wn vl) . wl)
    ((wn w) . ww)
    ((wn y) . wy)
    ((y b) . yv)
    ((y br) . yr)
    ((y c) . yg)
    ((y cl) . yl)
    ((y cr) . yr)
    ((y cw) . ww)
    ((y d) . yd)
    ((y dh) . ydh)
    ((y dhm) . ym)
    ((y dhw) . ww)
    ((y dr) . yr)
    ((y f) . yv)
    ((y fr) . yr)
    ((y gl) . yl)
    ((y gr) . yr)
    ((y l) . yl)
    ((y lh) . yl)
    ((y lw) . ww)
    ((y m) . ym)
    ((y mw) . ww)
    ((y n) . yn)
    ((y ng) . yng)
    ((y p) . yv)
    ((y pr) . yr)
    ((y r) . yr)
    ((y rh) . yrh)
    ((y s) . ys)
    ((y sh) . ysh)
    ((y t) . yd)
    ((y th) . ydh)
    ((y tr) . yr)
    ((y v) . yv)
    ((y vl) . yl)
    ((y w) . ww)
    ((y y) . yy)))


;;; generate a new cv using orthodox stem starts 

(defun gen-vc ()
  (let ((left (any $all-vowels-and-diphthongs))
        (right (any $stem-ends)))
    (intern (string-upcase (concatenate 'string (symbol-text left)(symbol-text right))))))

(defun gen-cv ()
  (let ((left (any $stem-starts))
        (right (any $all-vowels-and-diphthongs)))
    (intern (string-upcase (concatenate 'string (symbol-text left)(symbol-text right))))))

(defun gen-cvc ()
  (let ((left (any $stem-starts))
        (mid (any $all-vowels-and-diphthongs))
        (right (any $stem-ends)))
    (intern (string-upcase (concatenate 'string (symbol-text left)(symbol-text mid)(symbol-text right))))))

;;; build a word from two stems, using consonant-cluster-combining rules
(defmethod combine-stems ((s1 symbol)(s2 symbol))
  (let* ((s1-end (stem-end s1))
         (left (subseq (symbol-text s1)
                               0 (- (length (symbol-text s1))
                                    (length (symbol-text s1-end)))))
         (s2-start (stem-start s2))
         (right (subseq (symbol-text s2)
                                (length (symbol-text s2-start))))
         (comb (symbol-text (combine s1-end s2-start))))
    (intern (string-upcase (concatenate 'string left comb right)))))

(defun any (ls)
  (elt ls (random (length ls))))

(defun gen-binary ()
  (let ((left (any $stems))
        (right (any $stems)))
    (values (combine-stems left right)
            left
            right)))
