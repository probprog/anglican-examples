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
        [anglican core runtime emit [state :only [get-predicts]]] 
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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@2d4cca52&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@2d4cca52>"}
;; <=

;; **
;;; We can check out our string distribution by sampling from it a few times, like monkeys on keyboards, except worse, probably.
;; **

;; @@
(repeatedly 100 #(sample (stringdist 10)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;8E_&lt;d9V9&quot;</span>","value":"\"8E_<d9V9\""},{"type":"html","content":"<span class='clj-string'>&quot;`!\u001FN&amp;%&amp;}?%Dt}&quot;</span>","value":"\"`!\u001FN&%&}?%Dt}\""},{"type":"html","content":"<span class='clj-string'>&quot;\\\\3G.\\\\B^&amp;&quot;</span>","value":"\"\\\\3G.\\\\B^&\""},{"type":"html","content":"<span class='clj-string'>&quot;#OJ|a:$=&quot;</span>","value":"\"#OJ|a:$=\""},{"type":"html","content":"<span class='clj-string'>&quot;6YuA,$`n3&quot;</span>","value":"\"6YuA,$`n3\""},{"type":"html","content":"<span class='clj-string'>&quot;d$YI%!j$&quot;</span>","value":"\"d$YI%!j$\""},{"type":"html","content":"<span class='clj-string'>&quot;8{5WRiqt7&quot;</span>","value":"\"8{5WRiqt7\""},{"type":"html","content":"<span class='clj-string'>&quot;MHg&lt;(iIUC&quot;</span>","value":"\"MHg<(iIUC\""},{"type":"html","content":"<span class='clj-string'>&quot;$pKU&gt;IdWzO7F8&quot;</span>","value":"\"$pKU>IdWzO7F8\""},{"type":"html","content":"<span class='clj-string'>&quot;H)K=o&lt;BJgh,G-,&amp;\u001F&quot;</span>","value":"\"H)K=o<BJgh,G-,&\u001F\""},{"type":"html","content":"<span class='clj-string'>&quot;90o&quot;</span>","value":"\"90o\""},{"type":"html","content":"<span class='clj-string'>&quot;,&#x27;kj!=&quot;</span>","value":"\",'kj!=\""},{"type":"html","content":"<span class='clj-string'>&quot;8&gt;J8uo}X]hBjL&amp;0Jo&quot;</span>","value":"\"8>J8uo}X]hBjL&0Jo\""},{"type":"html","content":"<span class='clj-string'>&quot;n=e/\\&quot;y&lt;Dx6op7%e^#&quot;</span>","value":"\"n=e/\\\"y<Dx6op7%e^#\""},{"type":"html","content":"<span class='clj-string'>&quot;u^kFt_\\\\iK&quot;</span>","value":"\"u^kFt_\\\\iK\""},{"type":"html","content":"<span class='clj-string'>&quot;oxPSieU&quot;</span>","value":"\"oxPSieU\""},{"type":"html","content":"<span class='clj-string'>&quot;[o.ge X\\&quot;Gn&quot;</span>","value":"\"[o.ge X\\\"Gn\""},{"type":"html","content":"<span class='clj-string'>&quot;D./%8K&quot;</span>","value":"\"D./%8K\""},{"type":"html","content":"<span class='clj-string'>&quot;]][y)tp(t|Sk[J|&gt;&quot;</span>","value":"\"]][y)tp(t|Sk[J|>\""},{"type":"html","content":"<span class='clj-string'>&quot;@1;&amp;wPO\u001F&quot;</span>","value":"\"@1;&wPO\u001F\""},{"type":"html","content":"<span class='clj-string'>&quot;h$K1V&gt;n}. &quot;</span>","value":"\"h$K1V>n}. \""},{"type":"html","content":"<span class='clj-string'>&quot;36WC&gt;TgBQj T&quot;</span>","value":"\"36WC>TgBQj T\""},{"type":"html","content":"<span class='clj-string'>&quot;r9GL[6L|9iGQbV&quot;</span>","value":"\"r9GL[6L|9iGQbV\""},{"type":"html","content":"<span class='clj-string'>&quot;c7LBt7qG\\\\5;&quot;</span>","value":"\"c7LBt7qG\\\\5;\""},{"type":"html","content":"<span class='clj-string'>&quot;wSax$m]bFle&lt;a.&quot;</span>","value":"\"wSax$m]bFle<a.\""},{"type":"html","content":"<span class='clj-string'>&quot;Y=Eq{=JZ}!*&quot;</span>","value":"\"Y=Eq{=JZ}!*\""},{"type":"html","content":"<span class='clj-string'>&quot;zmo&amp;{&quot;</span>","value":"\"zmo&{\""},{"type":"html","content":"<span class='clj-string'>&quot;\\&quot;,QZ&amp;Yht^*6&quot;</span>","value":"\"\\\",QZ&Yht^*6\""},{"type":"html","content":"<span class='clj-string'>&quot;FK|  G7xH_3[g9)_-{.&quot;</span>","value":"\"FK|  G7xH_3[g9)_-{.\""},{"type":"html","content":"<span class='clj-string'>&quot;8K65&quot;</span>","value":"\"8K65\""},{"type":"html","content":"<span class='clj-string'>&quot;ZFLA0&#x27;f*`L\\&quot;i]0g]BU&quot;</span>","value":"\"ZFLA0'f*`L\\\"i]0g]BU\""},{"type":"html","content":"<span class='clj-string'>&quot;2ur]SU$fCKRJ&quot;</span>","value":"\"2ur]SU$fCKRJ\""},{"type":"html","content":"<span class='clj-string'>&quot;A3&lt;c#x0&amp;&quot;</span>","value":"\"A3<c#x0&\""},{"type":"html","content":"<span class='clj-string'>&quot;SNP7:&quot;</span>","value":"\"SNP7:\""},{"type":"html","content":"<span class='clj-string'>&quot;VaNt06H&lt;47^=,1&quot;</span>","value":"\"VaNt06H<47^=,1\""},{"type":"html","content":"<span class='clj-string'>&quot;8&#x27;J5Ul3!o&quot;</span>","value":"\"8'J5Ul3!o\""},{"type":"html","content":"<span class='clj-string'>&quot;&amp;K?80*53kK&quot;</span>","value":"\"&K?80*53kK\""},{"type":"html","content":"<span class='clj-string'>&quot;k8O+G3&quot;</span>","value":"\"k8O+G3\""},{"type":"html","content":"<span class='clj-string'>&quot;&#x27;S(^h%&quot;</span>","value":"\"'S(^h%\""},{"type":"html","content":"<span class='clj-string'>&quot;r]EQ.L\\&quot;nH&quot;</span>","value":"\"r]EQ.L\\\"nH\""},{"type":"html","content":"<span class='clj-string'>&quot;_1:&gt;1sEPWBMxBrV&quot;</span>","value":"\"_1:>1sEPWBMxBrV\""},{"type":"html","content":"<span class='clj-string'>&quot;?@AB-!5 }]B1a&quot;</span>","value":"\"?@AB-!5 }]B1a\""},{"type":"html","content":"<span class='clj-string'>&quot;4&#x27;S_1\u001FgHRw&quot;</span>","value":"\"4'S_1\u001FgHRw\""},{"type":"html","content":"<span class='clj-string'>&quot;!\\&quot;s8]C#One8&quot;</span>","value":"\"!\\\"s8]C#One8\""},{"type":"html","content":"<span class='clj-string'>&quot;z2v: 5|&quot;</span>","value":"\"z2v: 5|\""},{"type":"html","content":"<span class='clj-string'>&quot;QWw%n]]\\\\m?a`Y&quot;</span>","value":"\"QWw%n]]\\\\m?a`Y\""},{"type":"html","content":"<span class='clj-string'>&quot;rnT[21$6Z&#x27;b*o1T&quot;</span>","value":"\"rnT[21$6Z'b*o1T\""},{"type":"html","content":"<span class='clj-string'>&quot;kYzpF8y+RL\\\\y&quot;</span>","value":"\"kYzpF8y+RL\\\\y\""},{"type":"html","content":"<span class='clj-string'>&quot;y%;M/9U}&quot;</span>","value":"\"y%;M/9U}\""},{"type":"html","content":"<span class='clj-string'>&quot;nzhC4V&quot;</span>","value":"\"nzhC4V\""},{"type":"html","content":"<span class='clj-string'>&quot;)U,@Y-jysACU3S&quot;</span>","value":"\")U,@Y-jysACU3S\""},{"type":"html","content":"<span class='clj-string'>&quot;f@4{Q.wHXo{&quot;</span>","value":"\"f@4{Q.wHXo{\""},{"type":"html","content":"<span class='clj-string'>&quot;v@sXc R}$}&quot;</span>","value":"\"v@sXc R}$}\""},{"type":"html","content":"<span class='clj-string'>&quot;pgebV9SU@6;\\&quot;_Q&quot;</span>","value":"\"pgebV9SU@6;\\\"_Q\""},{"type":"html","content":"<span class='clj-string'>&quot;R\\\\I&quot;</span>","value":"\"R\\\\I\""},{"type":"html","content":"<span class='clj-string'>&quot;)?fai#Dh{&quot;</span>","value":"\")?fai#Dh{\""},{"type":"html","content":"<span class='clj-string'>&quot;*8sSG9Yc)xyhmLI&quot;</span>","value":"\"*8sSG9Yc)xyhmLI\""},{"type":"html","content":"<span class='clj-string'>&quot;F\\\\N.y$m!&#x27;8&quot;</span>","value":"\"F\\\\N.y$m!'8\""},{"type":"html","content":"<span class='clj-string'>&quot;y#1{R:cy9SaT&#x27;4f&quot;</span>","value":"\"y#1{R:cy9SaT'4f\""},{"type":"html","content":"<span class='clj-string'>&quot;k{?_5D&quot;</span>","value":"\"k{?_5D\""},{"type":"html","content":"<span class='clj-string'>&quot;?\u001F.[[#^B4&quot;</span>","value":"\"?\u001F.[[#^B4\""},{"type":"html","content":"<span class='clj-string'>&quot;v|bJwkh&quot;</span>","value":"\"v|bJwkh\""},{"type":"html","content":"<span class='clj-string'>&quot;&gt;7,=5j&#x27;&quot;</span>","value":"\">7,=5j'\""},{"type":"html","content":"<span class='clj-string'>&quot;!8z!,mjk&quot;</span>","value":"\"!8z!,mjk\""},{"type":"html","content":"<span class='clj-string'>&quot;\\&quot;4y&lt;zBb&quot;</span>","value":"\"\\\"4y<zBb\""},{"type":"html","content":"<span class='clj-string'>&quot;&amp;%0feo@sI\\&quot; 9t&quot;</span>","value":"\"&%0feo@sI\\\" 9t\""},{"type":"html","content":"<span class='clj-string'>&quot;:t(1&gt;L&gt;H&quot;</span>","value":"\":t(1>L>H\""},{"type":"html","content":"<span class='clj-string'>&quot;oy5!tG%BC&quot;</span>","value":"\"oy5!tG%BC\""},{"type":"html","content":"<span class='clj-string'>&quot;ral9ZX1TS%[m4=&quot;</span>","value":"\"ral9ZX1TS%[m4=\""},{"type":"html","content":"<span class='clj-string'>&quot;d]Zq/&quot;</span>","value":"\"d]Zq/\""},{"type":"html","content":"<span class='clj-string'>&quot;I^|[aD&quot;</span>","value":"\"I^|[aD\""},{"type":"html","content":"<span class='clj-string'>&quot;&amp;u y&lt;1T-5 ,o*&quot;</span>","value":"\"&u y<1T-5 ,o*\""},{"type":"html","content":"<span class='clj-string'>&quot;&gt;MBG\u001F&quot;</span>","value":"\">MBG\u001F\""},{"type":"html","content":"<span class='clj-string'>&quot;`%W?\\\\2x$&quot;</span>","value":"\"`%W?\\\\2x$\""},{"type":"html","content":"<span class='clj-string'>&quot;UM G)zo&gt;U)Xk4G?&quot;</span>","value":"\"UM G)zo>U)Xk4G?\""},{"type":"html","content":"<span class='clj-string'>&quot;_!U64m2 V[maje|7&quot;</span>","value":"\"_!U64m2 V[maje|7\""},{"type":"html","content":"<span class='clj-string'>&quot;?X6;R[E&quot;</span>","value":"\"?X6;R[E\""},{"type":"html","content":"<span class='clj-string'>&quot;|X%v7b:v=k&quot;</span>","value":"\"|X%v7b:v=k\""},{"type":"html","content":"<span class='clj-string'>&quot;M9I?48LwN&quot;</span>","value":"\"M9I?48LwN\""},{"type":"html","content":"<span class='clj-string'>&quot;O&lt;o9{DNA)&quot;</span>","value":"\"O<o9{DNA)\""},{"type":"html","content":"<span class='clj-string'>&quot;_htp6_e&quot;</span>","value":"\"_htp6_e\""},{"type":"html","content":"<span class='clj-string'>&quot;2/g\\&quot;&gt;yn;d)&quot;</span>","value":"\"2/g\\\">yn;d)\""},{"type":"html","content":"<span class='clj-string'>&quot;dD#\u001FF4BS5&quot;</span>","value":"\"dD#\u001FF4BS5\""},{"type":"html","content":"<span class='clj-string'>&quot;gFI%+P=w&quot;</span>","value":"\"gFI%+P=w\""},{"type":"html","content":"<span class='clj-string'>&quot;\\\\&lt;Uq&quot;</span>","value":"\"\\\\<Uq\""},{"type":"html","content":"<span class='clj-string'>&quot;#[\u001FR&#x27;c(k&quot;</span>","value":"\"#[\u001FR'c(k\""},{"type":"html","content":"<span class='clj-string'>&quot;-9&gt;\u001F0b\\&quot;|UWV?]r&quot;</span>","value":"\"-9>\u001F0b\\\"|UWV?]r\""},{"type":"html","content":"<span class='clj-string'>&quot;ak9=0&gt;&gt;Ar&quot;</span>","value":"\"ak9=0>>Ar\""},{"type":"html","content":"<span class='clj-string'>&quot;tZeP8%TW{&quot;</span>","value":"\"tZeP8%TW{\""},{"type":"html","content":"<span class='clj-string'>&quot;mS7|EAjKt&amp;tV0%#&quot;</span>","value":"\"mS7|EAjKt&tV0%#\""},{"type":"html","content":"<span class='clj-string'>&quot;n a Hm 8BQ\u001FG}&quot;</span>","value":"\"n a Hm 8BQ\u001FG}\""},{"type":"html","content":"<span class='clj-string'>&quot;Z$)8hGfeF&quot;</span>","value":"\"Z$)8hGfeF\""},{"type":"html","content":"<span class='clj-string'>&quot;0)}m\\\\cpUV&quot;</span>","value":"\"0)}m\\\\cpUV\""},{"type":"html","content":"<span class='clj-string'>&quot;S wBWnluxJ4&quot;</span>","value":"\"S wBWnluxJ4\""},{"type":"html","content":"<span class='clj-string'>&quot;gIIk{$YaC&quot;</span>","value":"\"gIIk{$YaC\""},{"type":"html","content":"<span class='clj-string'>&quot;2k3_byvM4yoj&quot;</span>","value":"\"2k3_byvM4yoj\""},{"type":"html","content":"<span class='clj-string'>&quot;bOdgC#:&quot;</span>","value":"\"bOdgC#:\""},{"type":"html","content":"<span class='clj-string'>&quot;IUb82sW&quot;</span>","value":"\"IUb82sW\""},{"type":"html","content":"<span class='clj-string'>&quot;+&gt;k\\&quot;FH%Y\u001FQl\\\\tj&quot;</span>","value":"\"+>k\\\"FH%Y\u001FQl\\\\tj\""},{"type":"html","content":"<span class='clj-string'>&quot;u^LjbyL&#x27;|WT&quot;</span>","value":"\"u^LjbyL'|WT\""}],"value":"(\"8E_<d9V9\" \"`!\u001FN&%&}?%Dt}\" \"\\\\3G.\\\\B^&\" \"#OJ|a:$=\" \"6YuA,$`n3\" \"d$YI%!j$\" \"8{5WRiqt7\" \"MHg<(iIUC\" \"$pKU>IdWzO7F8\" \"H)K=o<BJgh,G-,&\u001F\" \"90o\" \",'kj!=\" \"8>J8uo}X]hBjL&0Jo\" \"n=e/\\\"y<Dx6op7%e^#\" \"u^kFt_\\\\iK\" \"oxPSieU\" \"[o.ge X\\\"Gn\" \"D./%8K\" \"]][y)tp(t|Sk[J|>\" \"@1;&wPO\u001F\" \"h$K1V>n}. \" \"36WC>TgBQj T\" \"r9GL[6L|9iGQbV\" \"c7LBt7qG\\\\5;\" \"wSax$m]bFle<a.\" \"Y=Eq{=JZ}!*\" \"zmo&{\" \"\\\",QZ&Yht^*6\" \"FK|  G7xH_3[g9)_-{.\" \"8K65\" \"ZFLA0'f*`L\\\"i]0g]BU\" \"2ur]SU$fCKRJ\" \"A3<c#x0&\" \"SNP7:\" \"VaNt06H<47^=,1\" \"8'J5Ul3!o\" \"&K?80*53kK\" \"k8O+G3\" \"'S(^h%\" \"r]EQ.L\\\"nH\" \"_1:>1sEPWBMxBrV\" \"?@AB-!5 }]B1a\" \"4'S_1\u001FgHRw\" \"!\\\"s8]C#One8\" \"z2v: 5|\" \"QWw%n]]\\\\m?a`Y\" \"rnT[21$6Z'b*o1T\" \"kYzpF8y+RL\\\\y\" \"y%;M/9U}\" \"nzhC4V\" \")U,@Y-jysACU3S\" \"f@4{Q.wHXo{\" \"v@sXc R}$}\" \"pgebV9SU@6;\\\"_Q\" \"R\\\\I\" \")?fai#Dh{\" \"*8sSG9Yc)xyhmLI\" \"F\\\\N.y$m!'8\" \"y#1{R:cy9SaT'4f\" \"k{?_5D\" \"?\u001F.[[#^B4\" \"v|bJwkh\" \">7,=5j'\" \"!8z!,mjk\" \"\\\"4y<zBb\" \"&%0feo@sI\\\" 9t\" \":t(1>L>H\" \"oy5!tG%BC\" \"ral9ZX1TS%[m4=\" \"d]Zq/\" \"I^|[aD\" \"&u y<1T-5 ,o*\" \">MBG\u001F\" \"`%W?\\\\2x$\" \"UM G)zo>U)Xk4G?\" \"_!U64m2 V[maje|7\" \"?X6;R[E\" \"|X%v7b:v=k\" \"M9I?48LwN\" \"O<o9{DNA)\" \"_htp6_e\" \"2/g\\\">yn;d)\" \"dD#\u001FF4BS5\" \"gFI%+P=w\" \"\\\\<Uq\" \"#[\u001FR'c(k\" \"-9>\u001F0b\\\"|UWV?]r\" \"ak9=0>>Ar\" \"tZeP8%TW{\" \"mS7|EAjKt&tV0%#\" \"n a Hm 8BQ\u001FG}\" \"Z$)8hGfeF\" \"0)}m\\\\cpUV\" \"S wBWnluxJ4\" \"gIIk{$YaC\" \"2k3_byvM4yoj\" \"bOdgC#:\" \"IUb82sW\" \"+>k\\\"FH%Y\u001FQl\\\\tj\" \"u^LjbyL'|WT\")"}
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
;;; We can now sample from the the distribution defined by the query, to sample from the preimage of the MD5 hash.
;;; 
;;; Note: this is unlikely to find a reasonable result after only a small number of samples!
;; **

;; @@
(->> (doquery :pcascade md5-inverse [10 secret-message] :debug true)
     (take 1000)
     (filter #(not (= Double/NEGATIVE_INFINITY (:log-weight %))))
     (map get-predicts)
     (map :message)
     (distinct))

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[],"value":"()"}
;; <=
