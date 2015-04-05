;; gorilla-repl.fileformat = 1

;; **
;;; # Cryptographic Hash Inversion
;;; 
;;; In probabilistic programming systems like Anglican one may easily write programs that intentionally or unintentionally result in inference problems that are arbitrarily hard.  We've seen in [3SAT](3SAT.clj) that sat solving is expressible in Anglican.  Now we see another difficult problem is expressible. 
;;; 
;;; One of [Dan Roy](http://danroy.org/)'s favorite examples is inversion of cryptographic hash functions.  Again, what's to stop someone from writing a program that tries to invert such a function?  Nothing.
;;; 
;;; We'll use Clojure's Java interop to snarf an MD5 algorithm.
;;; 
;; **

;; @@
(ns md5-inversion
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [mrepl core]
        [embang runtime emit]
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/md5</span>","value":"#'md5-inversion/md5"}
;; <=

;; **
;;; And we'll roll our own nice generative model for strings in order to write the nasty MD5 inverter as inference.  
;; **

;; **
;;; 
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
  "generates strings of printable ascii chars with poisson l
   distributed length" 
  [l] []
  (sample [this] (join (map str (repeatedly 
          (sample (poisson l)) 
          #(sample unigram-character-distribution)))))
  (observe [this value] 
           (+ (observe (poisson l) (count value)) 
              (reduce + 
                (map #(observe unigram-character-distribution %)
                     (seq value))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@4f01518&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@4f01518>"}
;; <=

;; **
;;; We can check out our string distribution by sampling from it a few times, like monkeys on keyboards, except worse, probably.
;; **

;; @@
(repeatedly 100 #(sample (stringdist 10)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;`f&#x27;h&quot;</span>","value":"\"`f'h\""},{"type":"html","content":"<span class='clj-string'>&quot;lo\\&quot;\\\\0cQN+&quot;</span>","value":"\"lo\\\"\\\\0cQN+\""},{"type":"html","content":"<span class='clj-string'>&quot;2X}8&quot;</span>","value":"\"2X}8\""},{"type":"html","content":"<span class='clj-string'>&quot;ubv7!\\&quot;l6oR_&quot;</span>","value":"\"ubv7!\\\"l6oR_\""},{"type":"html","content":"<span class='clj-string'>&quot;j}[+}bR=Z\u001F,{&lt;&gt;g&quot;</span>","value":"\"j}[+}bR=Z\u001F,{<>g\""},{"type":"html","content":"<span class='clj-string'>&quot;M\u001FVEpw3Op:-3i&quot;</span>","value":"\"M\u001FVEpw3Op:-3i\""},{"type":"html","content":"<span class='clj-string'>&quot;AY&lt;-y/T@&quot;</span>","value":"\"AY<-y/T@\""},{"type":"html","content":"<span class='clj-string'>&quot;:-oZ|#f&gt;)&quot;</span>","value":"\":-oZ|#f>)\""},{"type":"html","content":"<span class='clj-string'>&quot;{$H6v/ &quot;</span>","value":"\"{$H6v/ \""},{"type":"html","content":"<span class='clj-string'>&quot;&#x27;Zg2G +Y)%px&quot;</span>","value":"\"'Zg2G +Y)%px\""},{"type":"html","content":"<span class='clj-string'>&quot;5QYWqaFY\\&quot;+b&quot;</span>","value":"\"5QYWqaFY\\\"+b\""},{"type":"html","content":"<span class='clj-string'>&quot;H509R)5x9zO8_&quot;</span>","value":"\"H509R)5x9zO8_\""},{"type":"html","content":"<span class='clj-string'>&quot;uN QazCu&#x27;iD,Bu&quot;</span>","value":"\"uN QazCu'iD,Bu\""},{"type":"html","content":"<span class='clj-string'>&quot;&amp;9b.+K]&#x27;2;AZy#&quot;</span>","value":"\"&9b.+K]'2;AZy#\""},{"type":"html","content":"<span class='clj-string'>&quot;4-xUf.m;|=&quot;</span>","value":"\"4-xUf.m;|=\""},{"type":"html","content":"<span class='clj-string'>&quot;d%b=EwI&quot;</span>","value":"\"d%b=EwI\""},{"type":"html","content":"<span class='clj-string'>&quot;0LJY[ }%r4o6[.F&quot;</span>","value":"\"0LJY[ }%r4o6[.F\""},{"type":"html","content":"<span class='clj-string'>&quot;VGC@lK@\\\\c9+F,1&lt;F)-&quot;</span>","value":"\"VGC@lK@\\\\c9+F,1<F)-\""},{"type":"html","content":"<span class='clj-string'>&quot;zry$|e@-EgnE1&quot;</span>","value":"\"zry$|e@-EgnE1\""},{"type":"html","content":"<span class='clj-string'>&quot;_zS$w&quot;</span>","value":"\"_zS$w\""},{"type":"html","content":"<span class='clj-string'>&quot;&#x27;m2MjS4pY0AZV&quot;</span>","value":"\"'m2MjS4pY0AZV\""},{"type":"html","content":"<span class='clj-string'>&quot;|LSb=nAGP}&quot;</span>","value":"\"|LSb=nAGP}\""},{"type":"html","content":"<span class='clj-string'>&quot;G/IoAbKM/L&quot;</span>","value":"\"G/IoAbKM/L\""},{"type":"html","content":"<span class='clj-string'>&quot;P2GQ\u001Fo4%:&lt;#&quot;</span>","value":"\"P2GQ\u001Fo4%:<#\""},{"type":"html","content":"<span class='clj-string'>&quot;@Oi[O!h&amp;N^2?&quot;</span>","value":"\"@Oi[O!h&N^2?\""},{"type":"html","content":"<span class='clj-string'>&quot;Zh&amp;X@dY=K&quot;</span>","value":"\"Zh&X@dY=K\""},{"type":"html","content":"<span class='clj-string'>&quot;4_\u001Fz9$Z%&lt;2^&quot;</span>","value":"\"4_\u001Fz9$Z%<2^\""},{"type":"html","content":"<span class='clj-string'>&quot;wy\\\\\u001FV&quot;</span>","value":"\"wy\\\\\u001FV\""},{"type":"html","content":"<span class='clj-string'>&quot;a[jk&gt;xvJw\\\\q&quot;</span>","value":"\"a[jk>xvJw\\\\q\""},{"type":"html","content":"<span class='clj-string'>&quot;:\\\\W,6_(`&quot;</span>","value":"\":\\\\W,6_(`\""},{"type":"html","content":"<span class='clj-string'>&quot;,E u9H?p?IL&quot;</span>","value":"\",E u9H?p?IL\""},{"type":"html","content":"<span class='clj-string'>&quot;Gq8S_&quot;</span>","value":"\"Gq8S_\""},{"type":"html","content":"<span class='clj-string'>&quot;]u}Y1&quot;</span>","value":"\"]u}Y1\""},{"type":"html","content":"<span class='clj-string'>&quot;crwe1*Ey0CA&quot;</span>","value":"\"crwe1*Ey0CA\""},{"type":"html","content":"<span class='clj-string'>&quot;/rL*E&quot;</span>","value":"\"/rL*E\""},{"type":"html","content":"<span class='clj-string'>&quot;^yO]0&quot;</span>","value":"\"^yO]0\""},{"type":"html","content":"<span class='clj-string'>&quot;Y7_T^bx+e&quot;</span>","value":"\"Y7_T^bx+e\""},{"type":"html","content":"<span class='clj-string'>&quot;x#V\\\\`JBK&lt;&quot;</span>","value":"\"x#V\\\\`JBK<\""},{"type":"html","content":"<span class='clj-string'>&quot;578p4y&quot;</span>","value":"\"578p4y\""},{"type":"html","content":"<span class='clj-string'>&quot;oV0vpJ)|/f&quot;</span>","value":"\"oV0vpJ)|/f\""},{"type":"html","content":"<span class='clj-string'>&quot;M};Thk_ilA&quot;</span>","value":"\"M};Thk_ilA\""},{"type":"html","content":"<span class='clj-string'>&quot;U]5T&gt;m^s&quot;</span>","value":"\"U]5T>m^s\""},{"type":"html","content":"<span class='clj-string'>&quot;&#x27;p_a8=(&lt;eU!&quot;</span>","value":"\"'p_a8=(<eU!\""},{"type":"html","content":"<span class='clj-string'>&quot;8df u1F*Jbrs)&quot;</span>","value":"\"8df u1F*Jbrs)\""},{"type":"html","content":"<span class='clj-string'>&quot;Pc8 }vx$*L)Pl?&quot;</span>","value":"\"Pc8 }vx$*L)Pl?\""},{"type":"html","content":"<span class='clj-string'>&quot;^,@(\\&quot;_KO*I$Va&quot;</span>","value":"\"^,@(\\\"_KO*I$Va\""},{"type":"html","content":"<span class='clj-string'>&quot;$9[51_r=c8F(&quot;</span>","value":"\"$9[51_r=c8F(\""},{"type":"html","content":"<span class='clj-string'>&quot;QesZ1SSi&#x27;[\\&quot;M{Ik4gK&quot;</span>","value":"\"QesZ1SSi'[\\\"M{Ik4gK\""},{"type":"html","content":"<span class='clj-string'>&quot;jiWee dTS.OdR&quot;</span>","value":"\"jiWee dTS.OdR\""},{"type":"html","content":"<span class='clj-string'>&quot;e%X(&quot;</span>","value":"\"e%X(\""},{"type":"html","content":"<span class='clj-string'>&quot;E-fjWI}5&quot;</span>","value":"\"E-fjWI}5\""},{"type":"html","content":"<span class='clj-string'>&quot;zW.?7]9YXF,cdF0CSF&quot;</span>","value":"\"zW.?7]9YXF,cdF0CSF\""},{"type":"html","content":"<span class='clj-string'>&quot; &lt;@C^&quot;</span>","value":"\" <@C^\""},{"type":"html","content":"<span class='clj-string'>&quot;3BSI7/`{`|&quot;</span>","value":"\"3BSI7/`{`|\""},{"type":"html","content":"<span class='clj-string'>&quot;\\\\|Yq)\\\\%&quot;</span>","value":"\"\\\\|Yq)\\\\%\""},{"type":"html","content":"<span class='clj-string'>&quot;tZBOCH)!{EcA&quot;</span>","value":"\"tZBOCH)!{EcA\""},{"type":"html","content":"<span class='clj-string'>&quot;&gt;G}-m,P!v@X%=k3&quot;</span>","value":"\">G}-m,P!v@X%=k3\""},{"type":"html","content":"<span class='clj-string'>&quot;&#x27;0emM$I%&quot;</span>","value":"\"'0emM$I%\""},{"type":"html","content":"<span class='clj-string'>&quot;}Ra%B{r&lt;I80[TQob&quot;</span>","value":"\"}Ra%B{r<I80[TQob\""},{"type":"html","content":"<span class='clj-string'>&quot;UM$K;(m1Q7s(&quot;</span>","value":"\"UM$K;(m1Q7s(\""},{"type":"html","content":"<span class='clj-string'>&quot;|!MCa6Np&quot;</span>","value":"\"|!MCa6Np\""},{"type":"html","content":"<span class='clj-string'>&quot;!qG,.J)W7*^&quot;</span>","value":"\"!qG,.J)W7*^\""},{"type":"html","content":"<span class='clj-string'>&quot;,GQ-Qg4Bu&lt;y&quot;</span>","value":"\",GQ-Qg4Bu<y\""},{"type":"html","content":"<span class='clj-string'>&quot;gg`K0Gmw5R=4xsm&quot;</span>","value":"\"gg`K0Gmw5R=4xsm\""},{"type":"html","content":"<span class='clj-string'>&quot;N9keT0,&quot;</span>","value":"\"N9keT0,\""},{"type":"html","content":"<span class='clj-string'>&quot;ta/$1hHF%xOZ&quot;</span>","value":"\"ta/$1hHF%xOZ\""},{"type":"html","content":"<span class='clj-string'>&quot;o9-J@\u001FoTJUl&quot;</span>","value":"\"o9-J@\u001FoTJUl\""},{"type":"html","content":"<span class='clj-string'>&quot;\u001Frf2QnsJ{I\u001F&quot;</span>","value":"\"\u001Frf2QnsJ{I\u001F\""},{"type":"html","content":"<span class='clj-string'>&quot;=GbG1&#x27;a&quot;</span>","value":"\"=GbG1'a\""},{"type":"html","content":"<span class='clj-string'>&quot;7k4pc_yIq_5bJ}CW}&quot;</span>","value":"\"7k4pc_yIq_5bJ}CW}\""},{"type":"html","content":"<span class='clj-string'>&quot;Bqldi2-&quot;</span>","value":"\"Bqldi2-\""},{"type":"html","content":"<span class='clj-string'>&quot;vk?Ai2(\u001FI*e&quot;</span>","value":"\"vk?Ai2(\u001FI*e\""},{"type":"html","content":"<span class='clj-string'>&quot;c-.;T%\\&quot;CO70&quot;</span>","value":"\"c-.;T%\\\"CO70\""},{"type":"html","content":"<span class='clj-string'>&quot;:Y_Ds %L&quot;</span>","value":"\":Y_Ds %L\""},{"type":"html","content":"<span class='clj-string'>&quot;8Xbtps$&quot;</span>","value":"\"8Xbtps$\""},{"type":"html","content":"<span class='clj-string'>&quot;0,O]ey]Tm5e;r.Y&quot;</span>","value":"\"0,O]ey]Tm5e;r.Y\""},{"type":"html","content":"<span class='clj-string'>&quot;9(Ld-v16&quot;</span>","value":"\"9(Ld-v16\""},{"type":"html","content":"<span class='clj-string'>&quot;neVww&quot;</span>","value":"\"neVww\""},{"type":"html","content":"<span class='clj-string'>&quot;.:{-{$mU@w&quot;</span>","value":"\".:{-{$mU@w\""},{"type":"html","content":"<span class='clj-string'>&quot;e[Og*XV&amp;LS&quot;</span>","value":"\"e[Og*XV&LS\""},{"type":"html","content":"<span class='clj-string'>&quot;Uir4&quot;</span>","value":"\"Uir4\""},{"type":"html","content":"<span class='clj-string'>&quot;W(;!W&lt;|&lt;4:BUtj&quot;</span>","value":"\"W(;!W<|<4:BUtj\""},{"type":"html","content":"<span class='clj-string'>&quot;D&quot;</span>","value":"\"D\""},{"type":"html","content":"<span class='clj-string'>&quot;]w8Oe&#x27;&quot;</span>","value":"\"]w8Oe'\""},{"type":"html","content":"<span class='clj-string'>&quot;&amp;?`7p&quot;</span>","value":"\"&?`7p\""},{"type":"html","content":"<span class='clj-string'>&quot;Z\\\\2!&quot;</span>","value":"\"Z\\\\2!\""},{"type":"html","content":"<span class='clj-string'>&quot;\u001Fij\\&quot;;U^29h&quot;</span>","value":"\"\u001Fij\\\";U^29h\""},{"type":"html","content":"<span class='clj-string'>&quot;PpB&gt;Y5`Xb]O_PJ&quot;</span>","value":"\"PpB>Y5`Xb]O_PJ\""},{"type":"html","content":"<span class='clj-string'>&quot;ln,4/bdSIJ.%&quot;</span>","value":"\"ln,4/bdSIJ.%\""},{"type":"html","content":"<span class='clj-string'>&quot;_6ZTr(:%&quot;</span>","value":"\"_6ZTr(:%\""},{"type":"html","content":"<span class='clj-string'>&quot; oWzQ*&#x27;Wa*+p8bc)&quot;</span>","value":"\" oWzQ*'Wa*+p8bc)\""},{"type":"html","content":"<span class='clj-string'>&quot;\u001F{\\&quot;@}&quot;</span>","value":"\"\u001F{\\\"@}\""},{"type":"html","content":"<span class='clj-string'>&quot;\\\\Qx1QP&amp;&quot;</span>","value":"\"\\\\Qx1QP&\""},{"type":"html","content":"<span class='clj-string'>&quot;.Z2AV$1*5:{t&quot;</span>","value":"\".Z2AV$1*5:{t\""},{"type":"html","content":"<span class='clj-string'>&quot;W`o#S;+bM4Wh&quot;</span>","value":"\"W`o#S;+bM4Wh\""},{"type":"html","content":"<span class='clj-string'>&quot;wMq]KHBp5\\&quot;&quot;</span>","value":"\"wMq]KHBp5\\\"\""},{"type":"html","content":"<span class='clj-string'>&quot;.F1=Nd[&quot;</span>","value":"\".F1=Nd[\""},{"type":"html","content":"<span class='clj-string'>&quot;m|5;QP9:&amp;i&quot;</span>","value":"\"m|5;QP9:&i\""},{"type":"html","content":"<span class='clj-string'>&quot;A?Oh]4\\&quot;-Jw&quot;</span>","value":"\"A?Oh]4\\\"-Jw\""},{"type":"html","content":"<span class='clj-string'>&quot;YC\\\\dcReiP/0u&quot;</span>","value":"\"YC\\\\dcReiP/0u\""}],"value":"(\"`f'h\" \"lo\\\"\\\\0cQN+\" \"2X}8\" \"ubv7!\\\"l6oR_\" \"j}[+}bR=Z\u001F,{<>g\" \"M\u001FVEpw3Op:-3i\" \"AY<-y/T@\" \":-oZ|#f>)\" \"{$H6v/ \" \"'Zg2G +Y)%px\" \"5QYWqaFY\\\"+b\" \"H509R)5x9zO8_\" \"uN QazCu'iD,Bu\" \"&9b.+K]'2;AZy#\" \"4-xUf.m;|=\" \"d%b=EwI\" \"0LJY[ }%r4o6[.F\" \"VGC@lK@\\\\c9+F,1<F)-\" \"zry$|e@-EgnE1\" \"_zS$w\" \"'m2MjS4pY0AZV\" \"|LSb=nAGP}\" \"G/IoAbKM/L\" \"P2GQ\u001Fo4%:<#\" \"@Oi[O!h&N^2?\" \"Zh&X@dY=K\" \"4_\u001Fz9$Z%<2^\" \"wy\\\\\u001FV\" \"a[jk>xvJw\\\\q\" \":\\\\W,6_(`\" \",E u9H?p?IL\" \"Gq8S_\" \"]u}Y1\" \"crwe1*Ey0CA\" \"/rL*E\" \"^yO]0\" \"Y7_T^bx+e\" \"x#V\\\\`JBK<\" \"578p4y\" \"oV0vpJ)|/f\" \"M};Thk_ilA\" \"U]5T>m^s\" \"'p_a8=(<eU!\" \"8df u1F*Jbrs)\" \"Pc8 }vx$*L)Pl?\" \"^,@(\\\"_KO*I$Va\" \"$9[51_r=c8F(\" \"QesZ1SSi'[\\\"M{Ik4gK\" \"jiWee dTS.OdR\" \"e%X(\" \"E-fjWI}5\" \"zW.?7]9YXF,cdF0CSF\" \" <@C^\" \"3BSI7/`{`|\" \"\\\\|Yq)\\\\%\" \"tZBOCH)!{EcA\" \">G}-m,P!v@X%=k3\" \"'0emM$I%\" \"}Ra%B{r<I80[TQob\" \"UM$K;(m1Q7s(\" \"|!MCa6Np\" \"!qG,.J)W7*^\" \",GQ-Qg4Bu<y\" \"gg`K0Gmw5R=4xsm\" \"N9keT0,\" \"ta/$1hHF%xOZ\" \"o9-J@\u001FoTJUl\" \"\u001Frf2QnsJ{I\u001F\" \"=GbG1'a\" \"7k4pc_yIq_5bJ}CW}\" \"Bqldi2-\" \"vk?Ai2(\u001FI*e\" \"c-.;T%\\\"CO70\" \":Y_Ds %L\" \"8Xbtps$\" \"0,O]ey]Tm5e;r.Y\" \"9(Ld-v16\" \"neVww\" \".:{-{$mU@w\" \"e[Og*XV&LS\" \"Uir4\" \"W(;!W<|<4:BUtj\" \"D\" \"]w8Oe'\" \"&?`7p\" \"Z\\\\2!\" \"\u001Fij\\\";U^29h\" \"PpB>Y5`Xb]O_PJ\" \"ln,4/bdSIJ.%\" \"_6ZTr(:%\" \" oWzQ*'Wa*+p8bc)\" \"\u001F{\\\"@}\" \"\\\\Qx1QP&\" \".Z2AV$1*5:{t\" \"W`o#S;+bM4Wh\" \"wMq]KHBp5\\\"\" \".F1=Nd[\" \"m|5;QP9:&i\" \"A?Oh]4\\\"-Jw\" \"YC\\\\dcReiP/0u\")"}
;; <=

;; **
;;; Again we'll again make use of the Dirac distribution to impose the hard constraint that the hash is satisfied exactly.
;; **

;; @@
(defdist dirac
  "Dirac distribution"
  [x] []
      (sample [this] x)
      (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))

(def secret-message (md5 "Yura, have we solved AI yet?"))
secret-message
(with-primitive-procedures 
  [dirac unigram-character-distribution stringdist md5] 
  (defquery md5-inverse [L md5str] 
    "conditional distribution of strings that map to same MD5 hash"
    (let [mesg (sample (stringdist L))]
    (observe (dirac md5str) (md5 mesg))
   (predict :message mesg))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;md5-inversion/md5-inverse</span>","value":"#'md5-inversion/md5-inverse"}
;; <=

;; **
;;; We can now sample from the the distribution defined by the query to learn whether or not each conjunction is satisfiable.    
;; **

;; @@
(->> (doquery :pcascade md5-inverse [10 secret-message] :debug true)
     (take 1000)
     (filter #(not 
                (= Double/NEGATIVE_INFINITY 
                   (:embang.state/log-weight %))))
     (map get-predicts)
     (map :message)
     (distinct)
     )

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}
;; <=

;; @@

;; @@
