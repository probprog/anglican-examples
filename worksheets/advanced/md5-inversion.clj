;; gorilla-repl.fileformat = 1

;; **
;;; # Cryptographic Hash Inversion
;;; 
;;; In probabilistic programming systems like Anglican, one may easily write programs that, intentionally or unintentionally, result in inference problems that are arbitrarily hard - practically impossible, in fact. These types of inference problems are not the types of problems that probabilistic programming languages are intended to solve. In this worksheet, we go through one such program, as a caution. Do not write probabilistic programs like this (other than for educational or testing purposes).
;;; 
;;; This worksheet is inspired by [Dan Roy](http://danroy.org/), who discusses the possibility of writing probabilistic programs to perform cryptographic hash inversion. It's certainly possible to envision a probabilistic program that takes in a hashed string, and randomly samples strings until it's managed to find a string that hashes to the supplied value. That is, a probabilistic program that inverts a hash.
;;; 
;;; If the hash is well-designed, the hash of a string is approximately a uniformly random sequence of characters. Therefore, inverting a hash essentially requires running through combinatorially-many possibilities completely at random until we happen to stumble on a string with the correct hash. In this worksheet, we attempt to make it clear that this sort of program is inherently not a good idea. However, what's to stop someone from writing a program like this? Nothing.
;; **

;; **
;;; We start by using Clojure's Java interop to write a function that MD5-hashes a supplied string:
;; **

;; @@
(ns md5-inversion
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit]
        [clojure.string :only (join split blank?)]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)
 
(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-class'>java.math.BigInteger</span>","value":"java.math.BigInteger"}],"value":"[nil,java.math.BigInteger]"},{"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/md5</span>","value":"#'md5-inversion/md5"}],"value":"[[nil,java.math.BigInteger],#'md5-inversion/md5]"}
;; <=

;; **
;;; Next, we'll prepare a simple generative model for strings. We implement a distribution that samples a random length, and then samples random characters to fill it.
;; **

;; @@
(def unigram-character-distribution 
  (let [printing-chars (map char (range 31 126))
        num-printing-chars (count printing-chars)
        equal-probs (repeat num-printing-chars 
                            (/ 1.0 num-printing-chars) )]
    (categorical (->> equal-probs 
                      (zipmap printing-chars)
                      (into [])))))

(defdist stringdist 
  "generates strings of printable ascii chars, the length distributed according to
  a Poisson distribution with parameter L"
  [L] []
  (sample* [this] (join (map str (repeatedly 
                                   (sample* (poisson L)) 
                                   #(sample* unigram-character-distribution)))))
  (observe* [this value] 
            (+ (observe* (poisson L) (count value)) 
               (reduce + 
                       (map #(observe* unigram-character-distribution %)
                            (seq value))))))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/unigram-character-distribution</span>","value":"#'md5-inversion/unigram-character-distribution"},{"type":"html","content":"<span class='clj-unkown'>#multifn[print-method 0xe3a1a7a]</span>","value":"#multifn[print-method 0xe3a1a7a]"}],"value":"[#'md5-inversion/unigram-character-distribution,#multifn[print-method 0xe3a1a7a]]"}
;; <=

;; **
;;; We can test our string distribution by sampling a few random strings. The results, as expected, are like monkeys banging on keyboards.
;; **

;; @@
(repeatedly 100 #(sample* (stringdist 10)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;6d] k0W&quot;</span>","value":"\"6d] k0W\""},{"type":"html","content":"<span class='clj-string'>&quot;clxF\\\\EP&lt;&#x27;qA&quot;</span>","value":"\"clxF\\\\EP<'qA\""},{"type":"html","content":"<span class='clj-string'>&quot;e%o,-K&quot;</span>","value":"\"e%o,-K\""},{"type":"html","content":"<span class='clj-string'>&quot;\u001F&gt;TG\\&quot;6&quot;</span>","value":"\"\u001F>TG\\\"6\""},{"type":"html","content":"<span class='clj-string'>&quot;JJC&lt;m`k$f|+XD9UE,&quot;</span>","value":"\"JJC<m`k$f|+XD9UE,\""},{"type":"html","content":"<span class='clj-string'>&quot;R,*6g\\&quot;|M)%k&quot;</span>","value":"\"R,*6g\\\"|M)%k\""},{"type":"html","content":"<span class='clj-string'>&quot;5b fv&quot;</span>","value":"\"5b fv\""},{"type":"html","content":"<span class='clj-string'>&quot;oAi&quot;</span>","value":"\"oAi\""},{"type":"html","content":"<span class='clj-string'>&quot;1Cp-Wai-4ag&quot;</span>","value":"\"1Cp-Wai-4ag\""},{"type":"html","content":"<span class='clj-string'>&quot;QKY3=n1Qat0y+&quot;</span>","value":"\"QKY3=n1Qat0y+\""},{"type":"html","content":"<span class='clj-string'>&quot;#)F:Kt@&quot;</span>","value":"\"#)F:Kt@\""},{"type":"html","content":"<span class='clj-string'>&quot;@_|}^P[w&quot;</span>","value":"\"@_|}^P[w\""},{"type":"html","content":"<span class='clj-string'>&quot;0c3%wp5v!\\&quot;I5&quot;</span>","value":"\"0c3%wp5v!\\\"I5\""},{"type":"html","content":"<span class='clj-string'>&quot;n/x7.%*]sNx4pO.OvU&quot;</span>","value":"\"n/x7.%*]sNx4pO.OvU\""},{"type":"html","content":"<span class='clj-string'>&quot;D9mcxmD2j&quot;</span>","value":"\"D9mcxmD2j\""},{"type":"html","content":"<span class='clj-string'>&quot;OIQ;s`PR&quot;</span>","value":"\"OIQ;s`PR\""},{"type":"html","content":"<span class='clj-string'>&quot;LEU(PmHC!&#x27;{|c5^&quot;</span>","value":"\"LEU(PmHC!'{|c5^\""},{"type":"html","content":"<span class='clj-string'>&quot;O!1le UsN{&quot;</span>","value":"\"O!1le UsN{\""},{"type":"html","content":"<span class='clj-string'>&quot;Yu}&gt;Z=:vVC&lt;wD0&quot;</span>","value":"\"Yu}>Z=:vVC<wD0\""},{"type":"html","content":"<span class='clj-string'>&quot;^;WlSL,x89&quot;</span>","value":"\"^;WlSL,x89\""},{"type":"html","content":"<span class='clj-string'>&quot;D$E\\&quot;)u9XR&quot;</span>","value":"\"D$E\\\")u9XR\""},{"type":"html","content":"<span class='clj-string'>&quot;@nPX|`k_&quot;</span>","value":"\"@nPX|`k_\""},{"type":"html","content":"<span class='clj-string'>&quot;6AoT&#x27;i&amp;W&quot;</span>","value":"\"6AoT'i&W\""},{"type":"html","content":"<span class='clj-string'>&quot;Zg.6s| 5&quot;</span>","value":"\"Zg.6s| 5\""},{"type":"html","content":"<span class='clj-string'>&quot;e#XH#!@z/dbp=rN7Z&quot;</span>","value":"\"e#XH#!@z/dbp=rN7Z\""},{"type":"html","content":"<span class='clj-string'>&quot;RG*ieOYS&quot;</span>","value":"\"RG*ieOYS\""},{"type":"html","content":"<span class='clj-string'>&quot;S[az3:_&quot;</span>","value":"\"S[az3:_\""},{"type":"html","content":"<span class='clj-string'>&quot;xEg[Y&quot;</span>","value":"\"xEg[Y\""},{"type":"html","content":"<span class='clj-string'>&quot;\\&quot;hbEzvD&quot;</span>","value":"\"\\\"hbEzvD\""},{"type":"html","content":"<span class='clj-string'>&quot;yW&quot;</span>","value":"\"yW\""},{"type":"html","content":"<span class='clj-string'>&quot;z\\\\42v`8Szo&quot;</span>","value":"\"z\\\\42v`8Szo\""},{"type":"html","content":"<span class='clj-string'>&quot;#Bgs:3vn6U&quot;</span>","value":"\"#Bgs:3vn6U\""},{"type":"html","content":"<span class='clj-string'>&quot;ea;Fl3WSe2D&quot;</span>","value":"\"ea;Fl3WSe2D\""},{"type":"html","content":"<span class='clj-string'>&quot;xHy^\\&quot;#25T2WE7JX&quot;</span>","value":"\"xHy^\\\"#25T2WE7JX\""},{"type":"html","content":"<span class='clj-string'>&quot;  GgT /`aN &amp;i&quot;</span>","value":"\"  GgT /`aN &i\""},{"type":"html","content":"<span class='clj-string'>&quot;=)E[g&lt;lo&quot;</span>","value":"\"=)E[g<lo\""},{"type":"html","content":"<span class='clj-string'>&quot;=&amp;B*WHD=FgQ&quot;</span>","value":"\"=&B*WHD=FgQ\""},{"type":"html","content":"<span class='clj-string'>&quot;_K\\&quot;IHvx)#O&quot;</span>","value":"\"_K\\\"IHvx)#O\""},{"type":"html","content":"<span class='clj-string'>&quot;JCg|E|loU&quot;</span>","value":"\"JCg|E|loU\""},{"type":"html","content":"<span class='clj-string'>&quot;2\\\\g7Ve:K &quot;</span>","value":"\"2\\\\g7Ve:K \""},{"type":"html","content":"<span class='clj-string'>&quot;}z5rCRx&quot;</span>","value":"\"}z5rCRx\""},{"type":"html","content":"<span class='clj-string'>&quot;TVl?&lt;o2k&quot;</span>","value":"\"TVl?<o2k\""},{"type":"html","content":"<span class='clj-string'>&quot;(lkhZCC,L)W5Y&quot;</span>","value":"\"(lkhZCC,L)W5Y\""},{"type":"html","content":"<span class='clj-string'>&quot;V{6i=g67B#&gt;n.O&quot;</span>","value":"\"V{6i=g67B#>n.O\""},{"type":"html","content":"<span class='clj-string'>&quot;Ryk0:E1P}1K8_siU,&quot;</span>","value":"\"Ryk0:E1P}1K8_siU,\""},{"type":"html","content":"<span class='clj-string'>&quot;CM-x&#x27;Y)9G&amp;&#x27;pV&quot;</span>","value":"\"CM-x'Y)9G&'pV\""},{"type":"html","content":"<span class='clj-string'>&quot;? *S3\\&quot;Mlt}yk|&quot;</span>","value":"\"? *S3\\\"Mlt}yk|\""},{"type":"html","content":"<span class='clj-string'>&quot;2dRI5&quot;</span>","value":"\"2dRI5\""},{"type":"html","content":"<span class='clj-string'>&quot;0$g2tPIf&amp;\u001FcH&quot;</span>","value":"\"0$g2tPIf&\u001FcH\""},{"type":"html","content":"<span class='clj-string'>&quot;VE/nSucDGpJ&lt;;=HT&quot;</span>","value":"\"VE/nSucDGpJ<;=HT\""},{"type":"html","content":"<span class='clj-string'>&quot;{A#@m2mZQ&quot;</span>","value":"\"{A#@m2mZQ\""},{"type":"html","content":"<span class='clj-string'>&quot;F)D\\\\Zy8&quot;</span>","value":"\"F)D\\\\Zy8\""},{"type":"html","content":"<span class='clj-string'>&quot;*Dy*EdK&quot;</span>","value":"\"*Dy*EdK\""},{"type":"html","content":"<span class='clj-string'>&quot;q2h;lg &gt;?qF[&quot;</span>","value":"\"q2h;lg >?qF[\""},{"type":"html","content":"<span class='clj-string'>&quot;=mqHd5\\\\w?^`{&quot;</span>","value":"\"=mqHd5\\\\w?^`{\""},{"type":"html","content":"<span class='clj-string'>&quot;/9OCqYA|{GWs&lt;/S2H]&quot;</span>","value":"\"/9OCqYA|{GWs</S2H]\""},{"type":"html","content":"<span class='clj-string'>&quot;CUzgwdVoR&quot;</span>","value":"\"CUzgwdVoR\""},{"type":"html","content":"<span class='clj-string'>&quot;4aXp&quot;</span>","value":"\"4aXp\""},{"type":"html","content":"<span class='clj-string'>&quot;:,&amp;Mq64\\\\Ud!&quot;</span>","value":"\":,&Mq64\\\\Ud!\""},{"type":"html","content":"<span class='clj-string'>&quot;yLJ!+i5; &quot;</span>","value":"\"yLJ!+i5; \""},{"type":"html","content":"<span class='clj-string'>&quot;z4EBJ^p(%!&quot;</span>","value":"\"z4EBJ^p(%!\""},{"type":"html","content":"<span class='clj-string'>&quot;0\\&quot;V-UXSjZIt^$&quot;</span>","value":"\"0\\\"V-UXSjZIt^$\""},{"type":"html","content":"<span class='clj-string'>&quot;E$jmp/}hb+DB5H&quot;</span>","value":"\"E$jmp/}hb+DB5H\""},{"type":"html","content":"<span class='clj-string'>&quot;P9gHDp#jG%^&quot;</span>","value":"\"P9gHDp#jG%^\""},{"type":"html","content":"<span class='clj-string'>&quot;%&gt;VrPA\u001FlK_a&quot;</span>","value":"\"%>VrPA\u001FlK_a\""},{"type":"html","content":"<span class='clj-string'>&quot;#NpQ@k/)=&#x27;O#N&#x27;NY&quot;</span>","value":"\"#NpQ@k/)='O#N'NY\""},{"type":"html","content":"<span class='clj-string'>&quot;!SnFb0Od&lt;XLQo,8`&quot;</span>","value":"\"!SnFb0Od<XLQo,8`\""},{"type":"html","content":"<span class='clj-string'>&quot;].$jv&quot;</span>","value":"\"].$jv\""},{"type":"html","content":"<span class='clj-string'>&quot;=GK2%j0N.2l/I;u@&quot;</span>","value":"\"=GK2%j0N.2l/I;u@\""},{"type":"html","content":"<span class='clj-string'>&quot;+g{Oy:&quot;</span>","value":"\"+g{Oy:\""},{"type":"html","content":"<span class='clj-string'>&quot;%8Nf7&gt;=*_9qt.Ee)&quot;</span>","value":"\"%8Nf7>=*_9qt.Ee)\""},{"type":"html","content":"<span class='clj-string'>&quot;9wk} sVCI&quot;</span>","value":"\"9wk} sVCI\""},{"type":"html","content":"<span class='clj-string'>&quot;mE7wjLo\\\\x(MoK&quot;</span>","value":"\"mE7wjLo\\\\x(MoK\""},{"type":"html","content":"<span class='clj-string'>&quot;?teaHw0WFa&quot;</span>","value":"\"?teaHw0WFa\""},{"type":"html","content":"<span class='clj-string'>&quot; $My&quot;</span>","value":"\" $My\""},{"type":"html","content":"<span class='clj-string'>&quot;CTODdNWG?&quot;</span>","value":"\"CTODdNWG?\""},{"type":"html","content":"<span class='clj-string'>&quot;z4L4WGx8mYZ0Xs&quot;</span>","value":"\"z4L4WGx8mYZ0Xs\""},{"type":"html","content":"<span class='clj-string'>&quot;M&#x27;\u001FHFae&#x27;!&quot;</span>","value":"\"M'\u001FHFae'!\""},{"type":"html","content":"<span class='clj-string'>&quot;;AS&amp;+P:\\&quot;&quot;</span>","value":"\";AS&+P:\\\"\""},{"type":"html","content":"<span class='clj-string'>&quot;)B_FQHcR&quot;</span>","value":"\")B_FQHcR\""},{"type":"html","content":"<span class='clj-string'>&quot;Gh&amp;@)Wv:vL&gt;5&quot;</span>","value":"\"Gh&@)Wv:vL>5\""},{"type":"html","content":"<span class='clj-string'>&quot;TTaasl4.&quot;</span>","value":"\"TTaasl4.\""},{"type":"html","content":"<span class='clj-string'>&quot;\\&quot;Z_P5GWZ=RZAE;&quot;</span>","value":"\"\\\"Z_P5GWZ=RZAE;\""},{"type":"html","content":"<span class='clj-string'>&quot;C^R&#x27;4&#x27;s%l*&quot;</span>","value":"\"C^R'4's%l*\""},{"type":"html","content":"<span class='clj-string'>&quot;(n H|@Ry&quot;</span>","value":"\"(n H|@Ry\""},{"type":"html","content":"<span class='clj-string'>&quot;O|3B!S#H7.0$_&quot;</span>","value":"\"O|3B!S#H7.0$_\""},{"type":"html","content":"<span class='clj-string'>&quot;n5J&lt;b_Y&quot;</span>","value":"\"n5J<b_Y\""},{"type":"html","content":"<span class='clj-string'>&quot;\u001FTOy1XL&quot;</span>","value":"\"\u001FTOy1XL\""},{"type":"html","content":"<span class='clj-string'>&quot;i03CclkaA&quot;</span>","value":"\"i03CclkaA\""},{"type":"html","content":"<span class='clj-string'>&quot;Qm4C8 #OnR4]&quot;</span>","value":"\"Qm4C8 #OnR4]\""},{"type":"html","content":"<span class='clj-string'>&quot;_-b6?&quot;</span>","value":"\"_-b6?\""},{"type":"html","content":"<span class='clj-string'>&quot;5O^PI&amp;_to T{XD&quot;</span>","value":"\"5O^PI&_to T{XD\""},{"type":"html","content":"<span class='clj-string'>&quot; yG92.V=&quot;</span>","value":"\" yG92.V=\""},{"type":"html","content":"<span class='clj-string'>&quot;81Eau&#x27;]&quot;</span>","value":"\"81Eau']\""},{"type":"html","content":"<span class='clj-string'>&quot;\u001Fxx6F*)5&quot;</span>","value":"\"\u001Fxx6F*)5\""},{"type":"html","content":"<span class='clj-string'>&quot;U&#x27;qO!ed&#x27;ZY&quot;</span>","value":"\"U'qO!ed'ZY\""},{"type":"html","content":"<span class='clj-string'>&quot;hR|huC&quot;</span>","value":"\"hR|huC\""},{"type":"html","content":"<span class='clj-string'>&quot;Ye0)]Za$-l&quot;</span>","value":"\"Ye0)]Za$-l\""},{"type":"html","content":"<span class='clj-string'>&quot;TzKa/^@*lR &quot;</span>","value":"\"TzKa/^@*lR \""},{"type":"html","content":"<span class='clj-string'>&quot;Ra4DIb/tb#&quot;</span>","value":"\"Ra4DIb/tb#\""}],"value":"(\"6d] k0W\" \"clxF\\\\EP<'qA\" \"e%o,-K\" \"\u001F>TG\\\"6\" \"JJC<m`k$f|+XD9UE,\" \"R,*6g\\\"|M)%k\" \"5b fv\" \"oAi\" \"1Cp-Wai-4ag\" \"QKY3=n1Qat0y+\" \"#)F:Kt@\" \"@_|}^P[w\" \"0c3%wp5v!\\\"I5\" \"n/x7.%*]sNx4pO.OvU\" \"D9mcxmD2j\" \"OIQ;s`PR\" \"LEU(PmHC!'{|c5^\" \"O!1le UsN{\" \"Yu}>Z=:vVC<wD0\" \"^;WlSL,x89\" \"D$E\\\")u9XR\" \"@nPX|`k_\" \"6AoT'i&W\" \"Zg.6s| 5\" \"e#XH#!@z/dbp=rN7Z\" \"RG*ieOYS\" \"S[az3:_\" \"xEg[Y\" \"\\\"hbEzvD\" \"yW\" \"z\\\\42v`8Szo\" \"#Bgs:3vn6U\" \"ea;Fl3WSe2D\" \"xHy^\\\"#25T2WE7JX\" \"  GgT /`aN &i\" \"=)E[g<lo\" \"=&B*WHD=FgQ\" \"_K\\\"IHvx)#O\" \"JCg|E|loU\" \"2\\\\g7Ve:K \" \"}z5rCRx\" \"TVl?<o2k\" \"(lkhZCC,L)W5Y\" \"V{6i=g67B#>n.O\" \"Ryk0:E1P}1K8_siU,\" \"CM-x'Y)9G&'pV\" \"? *S3\\\"Mlt}yk|\" \"2dRI5\" \"0$g2tPIf&\u001FcH\" \"VE/nSucDGpJ<;=HT\" \"{A#@m2mZQ\" \"F)D\\\\Zy8\" \"*Dy*EdK\" \"q2h;lg >?qF[\" \"=mqHd5\\\\w?^`{\" \"/9OCqYA|{GWs</S2H]\" \"CUzgwdVoR\" \"4aXp\" \":,&Mq64\\\\Ud!\" \"yLJ!+i5; \" \"z4EBJ^p(%!\" \"0\\\"V-UXSjZIt^$\" \"E$jmp/}hb+DB5H\" \"P9gHDp#jG%^\" \"%>VrPA\u001FlK_a\" \"#NpQ@k/)='O#N'NY\" \"!SnFb0Od<XLQo,8`\" \"].$jv\" \"=GK2%j0N.2l/I;u@\" \"+g{Oy:\" \"%8Nf7>=*_9qt.Ee)\" \"9wk} sVCI\" \"mE7wjLo\\\\x(MoK\" \"?teaHw0WFa\" \" $My\" \"CTODdNWG?\" \"z4L4WGx8mYZ0Xs\" \"M'\u001FHFae'!\" \";AS&+P:\\\"\" \")B_FQHcR\" \"Gh&@)Wv:vL>5\" \"TTaasl4.\" \"\\\"Z_P5GWZ=RZAE;\" \"C^R'4's%l*\" \"(n H|@Ry\" \"O|3B!S#H7.0$_\" \"n5J<b_Y\" \"\u001FTOy1XL\" \"i03CclkaA\" \"Qm4C8 #OnR4]\" \"_-b6?\" \"5O^PI&_to T{XD\" \" yG92.V=\" \"81Eau']\" \"\u001Fxx6F*)5\" \"U'qO!ed'ZY\" \"hR|huC\" \"Ye0)]Za$-l\" \"TzKa/^@*lR \" \"Ra4DIb/tb#\")"}
;; <=

;; **
;;; We decide on a secret message, and then condition on a randomly-chosen string as matching the hash of that secret message exactly. We do this using the Dirac distribution, defined below, which assigns zero probability to any string other than the correct one - a hard constraint.
;; **

;; @@
(defdist dirac
  "Dirac distribution"
  [x] []
  (sample* [this] x)
  (observe* [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))

(def secret-message (md5 "Yura, have we solved AI yet?"))

secret-message

(defquery md5-inverse [L] 
  "conditional distribution of strings that map to the same MD5 hash"
  
  (declare :primitive md5 dirac stringdist)
  (let [msg (sample (stringdist L))]
    (observe (dirac secret-message) (md5 msg))
    msg))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-unkown'>#multifn[print-method 0xe3a1a7a]</span>","value":"#multifn[print-method 0xe3a1a7a]"},{"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/secret-message</span>","value":"#'md5-inversion/secret-message"}],"value":"[#multifn[print-method 0xe3a1a7a],#'md5-inversion/secret-message]"},{"type":"html","content":"<span class='clj-string'>&quot;d8200511bb1b61d179fc5b3fb1e76d99&quot;</span>","value":"\"d8200511bb1b61d179fc5b3fb1e76d99\""}],"value":"[[#multifn[print-method 0xe3a1a7a],#'md5-inversion/secret-message],\"d8200511bb1b61d179fc5b3fb1e76d99\"]"},{"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/md5-inverse</span>","value":"#'md5-inversion/md5-inverse"}],"value":"[[[#multifn[print-method 0xe3a1a7a],#'md5-inversion/secret-message],\"d8200511bb1b61d179fc5b3fb1e76d99\"],#'md5-inversion/md5-inverse]"}
;; <=

;; **
;;; If inference were perfect, sampling from the distribution defined by the query would yield only the secret message (or, technically, any strings that happen to have same hash). However, it is fantastically unlikely that any of the sampled strings will have precisely the right hash.
;; **

;; @@
(def particles 1000)

(def samples
  (take particles
        (doquery :smc md5-inverse [10]
               :number-of-particles particles
               :drop-invalid false)))

(def any-strings-correct?
  (some #(> % (- (/ 1.0 0.0)))
        (map :log-weight samples)))

(if any-strings-correct?
  "Found a string with the right hash! With your luck, you should consider buying a lottery ticket!"
  "Predictably, failed to find a string with the right hash.")

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/particles</span>","value":"#'md5-inversion/particles"},{"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/samples</span>","value":"#'md5-inversion/samples"}],"value":"[#'md5-inversion/particles,#'md5-inversion/samples]"},{"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/any-strings-correct?</span>","value":"#'md5-inversion/any-strings-correct?"}],"value":"[[#'md5-inversion/particles,#'md5-inversion/samples],#'md5-inversion/any-strings-correct?]"},{"type":"html","content":"<span class='clj-string'>&quot;Predictably, failed to find a string with the right hash.&quot;</span>","value":"\"Predictably, failed to find a string with the right hash.\""}],"value":"[[[#'md5-inversion/particles,#'md5-inversion/samples],#'md5-inversion/any-strings-correct?],\"Predictably, failed to find a string with the right hash.\"]"}
;; <=

;; **
;;; In fact, none of the strings we sampled will even be close to the secret message:
;; **

;; @@
(def strings
  (map :result samples))

(take 50 strings)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/strings</span>","value":"#'md5-inversion/strings"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;n?ljt g&quot;</span>","value":"\"n?ljt g\""},{"type":"html","content":"<span class='clj-string'>&quot;wJ1Z@E4?nL&quot;</span>","value":"\"wJ1Z@E4?nL\""},{"type":"html","content":"<span class='clj-string'>&quot;BO`4nLl\u001f[-r XvM-Xd+&quot;</span>","value":"\"BO`4nLl\u001f[-r XvM-Xd+\""},{"type":"html","content":"<span class='clj-string'>&quot;.qgw;Ex&quot;</span>","value":"\".qgw;Ex\""},{"type":"html","content":"<span class='clj-string'>&quot;ja@nzl&quot;</span>","value":"\"ja@nzl\""},{"type":"html","content":"<span class='clj-string'>&quot;;+Z:J1&quot;</span>","value":"\";+Z:J1\""},{"type":"html","content":"<span class='clj-string'>&quot;CSJ(\\\\)lq2 \\&quot;oRS&quot;</span>","value":"\"CSJ(\\\\)lq2 \\\"oRS\""},{"type":"html","content":"<span class='clj-string'>&quot;]st6!+(&gt;u8&quot;</span>","value":"\"]st6!+(>u8\""},{"type":"html","content":"<span class='clj-string'>&quot;^ 27&amp;^a@$L&quot;</span>","value":"\"^ 27&^a@$L\""},{"type":"html","content":"<span class='clj-string'>&quot;Zq#6 &quot;</span>","value":"\"Zq#6 \""},{"type":"html","content":"<span class='clj-string'>&quot;@[b&amp;Wey]dX&quot;</span>","value":"\"@[b&Wey]dX\""},{"type":"html","content":"<span class='clj-string'>&quot;%^&#x27;k&amp;J%3Ilc;&amp;G&quot;</span>","value":"\"%^'k&J%3Ilc;&G\""},{"type":"html","content":"<span class='clj-string'>&quot;XCw4zFl7&amp;q{&quot;</span>","value":"\"XCw4zFl7&q{\""},{"type":"html","content":"<span class='clj-string'>&quot;{/rl&quot;</span>","value":"\"{/rl\""},{"type":"html","content":"<span class='clj-string'>&quot;m[,Oq=3`G.n&lt;*e2q!&quot;</span>","value":"\"m[,Oq=3`G.n<*e2q!\""},{"type":"html","content":"<span class='clj-string'>&quot;tO =++g;R\\&quot;n&quot;</span>","value":"\"tO =++g;R\\\"n\""},{"type":"html","content":"<span class='clj-string'>&quot;yNKQEsLR-J:Tyz&quot;</span>","value":"\"yNKQEsLR-J:Tyz\""},{"type":"html","content":"<span class='clj-string'>&quot;Wh|uY9(u8h8/&quot;</span>","value":"\"Wh|uY9(u8h8/\""},{"type":"html","content":"<span class='clj-string'>&quot;*vn\\&quot;0zk2Aamzr&quot;</span>","value":"\"*vn\\\"0zk2Aamzr\""},{"type":"html","content":"<span class='clj-string'>&quot;09:dDXj,&gt;&quot;</span>","value":"\"09:dDXj,>\""},{"type":"html","content":"<span class='clj-string'>&quot;i;,XsN&#x27;`Td@7&quot;</span>","value":"\"i;,XsN'`Td@7\""},{"type":"html","content":"<span class='clj-string'>&quot;UQ&amp;V&#x27;&amp;*|&amp;kQfTMs&quot;</span>","value":"\"UQ&V'&*|&kQfTMs\""},{"type":"html","content":"<span class='clj-string'>&quot;1B38BNP.\\&quot;-rYG&quot;</span>","value":"\"1B38BNP.\\\"-rYG\""},{"type":"html","content":"<span class='clj-string'>&quot;uRhk[cx=&quot;</span>","value":"\"uRhk[cx=\""},{"type":"html","content":"<span class='clj-string'>&quot;1\\&quot;C1T ,+NvrI#!wZ&quot;</span>","value":"\"1\\\"C1T ,+NvrI#!wZ\""},{"type":"html","content":"<span class='clj-string'>&quot;L!8N_^8|M&quot;</span>","value":"\"L!8N_^8|M\""},{"type":"html","content":"<span class='clj-string'>&quot;NVDGu*Ck+TT}o3&quot;</span>","value":"\"NVDGu*Ck+TT}o3\""},{"type":"html","content":"<span class='clj-string'>&quot;K)FpKH2g&quot;</span>","value":"\"K)FpKH2g\""},{"type":"html","content":"<span class='clj-string'>&quot;U`Pv2&#x27;u&quot;</span>","value":"\"U`Pv2'u\""},{"type":"html","content":"<span class='clj-string'>&quot;}\\\\^ufTR`_wa&quot;</span>","value":"\"}\\\\^ufTR`_wa\""},{"type":"html","content":"<span class='clj-string'>&quot;A%[B6SZEF83&quot;</span>","value":"\"A%[B6SZEF83\""},{"type":"html","content":"<span class='clj-string'>&quot;0krhBzz&quot;</span>","value":"\"0krhBzz\""},{"type":"html","content":"<span class='clj-string'>&quot;J&amp;bo3TCqIw?&amp;f&quot;</span>","value":"\"J&bo3TCqIw?&f\""},{"type":"html","content":"<span class='clj-string'>&quot;jzyUyu`=SuS&quot;</span>","value":"\"jzyUyu`=SuS\""},{"type":"html","content":"<span class='clj-string'>&quot;(g%lF8O&gt;?y2&quot;</span>","value":"\"(g%lF8O>?y2\""},{"type":"html","content":"<span class='clj-string'>&quot;nUCeDZ$),Y&quot;</span>","value":"\"nUCeDZ$),Y\""},{"type":"html","content":"<span class='clj-string'>&quot;^-u5B6lr9Iu&quot;</span>","value":"\"^-u5B6lr9Iu\""},{"type":"html","content":"<span class='clj-string'>&quot;pM.A0.cqPGi^&quot;</span>","value":"\"pM.A0.cqPGi^\""},{"type":"html","content":"<span class='clj-string'>&quot;Bxz#M:9&lt;&#x27;&quot;</span>","value":"\"Bxz#M:9<'\""},{"type":"html","content":"<span class='clj-string'>&quot;A4{Z\u001fv-D?&amp;9ff&amp;|&gt;&quot;</span>","value":"\"A4{Z\u001fv-D?&9ff&|>\""},{"type":"html","content":"<span class='clj-string'>&quot;cY.a*:M\u001fT zb#&quot;</span>","value":"\"cY.a*:M\u001fT zb#\""},{"type":"html","content":"<span class='clj-string'>&quot;/8LJ]y.y`&quot;</span>","value":"\"/8LJ]y.y`\""},{"type":"html","content":"<span class='clj-string'>&quot;;&gt;QN$hBjd{&quot;</span>","value":"\";>QN$hBjd{\""},{"type":"html","content":"<span class='clj-string'>&quot;*gg@J%Ov&quot;</span>","value":"\"*gg@J%Ov\""},{"type":"html","content":"<span class='clj-string'>&quot;mcaX2Fj2kVW:&quot;</span>","value":"\"mcaX2Fj2kVW:\""},{"type":"html","content":"<span class='clj-string'>&quot;@9]&lt;|l7SzB^[`&quot;</span>","value":"\"@9]<|l7SzB^[`\""},{"type":"html","content":"<span class='clj-string'>&quot;HKfc*:BfI@JJJ&quot;</span>","value":"\"HKfc*:BfI@JJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;Gi_QLL2&quot;</span>","value":"\"Gi_QLL2\""},{"type":"html","content":"<span class='clj-string'>&quot;4o:])&gt;z&quot;</span>","value":"\"4o:])>z\""},{"type":"html","content":"<span class='clj-string'>&quot;6?xT?xH\\&quot;:&quot;</span>","value":"\"6?xT?xH\\\":\""}],"value":"(\"n?ljt g\" \"wJ1Z@E4?nL\" \"BO`4nLl\u001f[-r XvM-Xd+\" \".qgw;Ex\" \"ja@nzl\" \";+Z:J1\" \"CSJ(\\\\)lq2 \\\"oRS\" \"]st6!+(>u8\" \"^ 27&^a@$L\" \"Zq#6 \" \"@[b&Wey]dX\" \"%^'k&J%3Ilc;&G\" \"XCw4zFl7&q{\" \"{/rl\" \"m[,Oq=3`G.n<*e2q!\" \"tO =++g;R\\\"n\" \"yNKQEsLR-J:Tyz\" \"Wh|uY9(u8h8/\" \"*vn\\\"0zk2Aamzr\" \"09:dDXj,>\" \"i;,XsN'`Td@7\" \"UQ&V'&*|&kQfTMs\" \"1B38BNP.\\\"-rYG\" \"uRhk[cx=\" \"1\\\"C1T ,+NvrI#!wZ\" \"L!8N_^8|M\" \"NVDGu*Ck+TT}o3\" \"K)FpKH2g\" \"U`Pv2'u\" \"}\\\\^ufTR`_wa\" \"A%[B6SZEF83\" \"0krhBzz\" \"J&bo3TCqIw?&f\" \"jzyUyu`=SuS\" \"(g%lF8O>?y2\" \"nUCeDZ$),Y\" \"^-u5B6lr9Iu\" \"pM.A0.cqPGi^\" \"Bxz#M:9<'\" \"A4{Z\u001fv-D?&9ff&|>\" \"cY.a*:M\u001fT zb#\" \"/8LJ]y.y`\" \";>QN$hBjd{\" \"*gg@J%Ov\" \"mcaX2Fj2kVW:\" \"@9]<|l7SzB^[`\" \"HKfc*:BfI@JJJ\" \"Gi_QLL2\" \"4o:])>z\" \"6?xT?xH\\\":\")"}],"value":"[#'md5-inversion/strings,(\"n?ljt g\" \"wJ1Z@E4?nL\" \"BO`4nLl\u001f[-r XvM-Xd+\" \".qgw;Ex\" \"ja@nzl\" \";+Z:J1\" \"CSJ(\\\\)lq2 \\\"oRS\" \"]st6!+(>u8\" \"^ 27&^a@$L\" \"Zq#6 \" \"@[b&Wey]dX\" \"%^'k&J%3Ilc;&G\" \"XCw4zFl7&q{\" \"{/rl\" \"m[,Oq=3`G.n<*e2q!\" \"tO =++g;R\\\"n\" \"yNKQEsLR-J:Tyz\" \"Wh|uY9(u8h8/\" \"*vn\\\"0zk2Aamzr\" \"09:dDXj,>\" \"i;,XsN'`Td@7\" \"UQ&V'&*|&kQfTMs\" \"1B38BNP.\\\"-rYG\" \"uRhk[cx=\" \"1\\\"C1T ,+NvrI#!wZ\" \"L!8N_^8|M\" \"NVDGu*Ck+TT}o3\" \"K)FpKH2g\" \"U`Pv2'u\" \"}\\\\^ufTR`_wa\" \"A%[B6SZEF83\" \"0krhBzz\" \"J&bo3TCqIw?&f\" \"jzyUyu`=SuS\" \"(g%lF8O>?y2\" \"nUCeDZ$),Y\" \"^-u5B6lr9Iu\" \"pM.A0.cqPGi^\" \"Bxz#M:9<'\" \"A4{Z\u001fv-D?&9ff&|>\" \"cY.a*:M\u001fT zb#\" \"/8LJ]y.y`\" \";>QN$hBjd{\" \"*gg@J%Ov\" \"mcaX2Fj2kVW:\" \"@9]<|l7SzB^[`\" \"HKfc*:BfI@JJJ\" \"Gi_QLL2\" \"4o:])>z\" \"6?xT?xH\\\":\")]"}
;; <=

;; **
;;; So, to sum up: in a probabilistic programming language that allows the creation of arbitrary distributions,  conditioning on these distributions, and the use of arbitrary primitives, it must be possible to write programs like this. Programs that sample from a huge space of possibilities, and place a hard constraint - a Dirac observation - on one particular result. Such programs do not play to the strengths of probabilistic probabilistic programming languages; they are more accurately understood as problems of satisfiability rather than problems of inference.
;;; 
;;; This is, in fact, why the Dirac distribution doesn't come prepackaged within Anglican - to discourage the writing of precisely this kind of program. Now, there are some situations in which Dirac observations are perfectly reasonable things to do. But if you choose to do so, think of this worksheet - and make sure you're not doing something stupid.
;; **
