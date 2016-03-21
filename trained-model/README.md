Trained 21 March 2016 on CORA/GROBID. Get it using:

`wget https://s3.amazonaws.com/iesl-citation-models/CitationTagger.tgz`

Notes:

* The uncompressed model is 93M
* It achieves the following performance on the CORA/GROBID test set:


OVERALL  f1=0.887916 p=0.866716 r=0.910180 (tp=19983 fp=3073 fn=1972 true=21955 pred=23056) acc=0.962744 (75328/78243)

author   f1=0.583977 p=0.583977 r=0.583977 (tp=1210 fp=862 fn=862 true=2072 pred=2072)

booktitle f1=0.929309 p=0.927100 r=0.931529 (tp=585 fp=46 fn=43 true=628 pred=631)

date     f1=0.994437 p=0.993976 r=0.994898 (tp=2145 fp=13 fn=11 true=2156 pred=2158)

editor   f1=0.524887 p=0.508772 r=0.542056 (tp=58 fp=56 fn=49 true=107 pred=114)

institution f1=0.936709 p=0.936709 r=0.936709 (tp=74 fp=5 fn=5 true=79 pred=79)

issue    f1=0.887218 p=0.830986 r=0.951613 (tp=177 fp=36 fn=9 true=186 pred=213)

journal  f1=0.848989 p=0.844637 r=0.853386 (tp=1071 fp=197 fn=184 true=1255 pred=1268)

location f1=0.927813 p=0.906639 r=0.950000 (tp=437 fp=45 fn=23 true=460 pred=482)

note     f1=0.739726 p=0.675000 r=0.818182 (tp=54 fp=26 fn=12 true=66 pred=80)

other    f1=0.901756 p=0.860644 r=0.946993 (tp=9165 fp=1484 fn=513 true=9678 pred=10649)

pages    f1=0.961957 p=0.960796 r=0.963120 (tp=1593 fp=65 fn=61 true=1654 pred=1658)

publisher f1=0.934046 p=0.939068 r=0.929078 (tp=262 fp=17 fn=20 true=282 pred=279)

pubnum   f1=1.000000 p=1.000000 r=1.000000 (tp=3 fp=0 fn=0 true=3 pred=3)

tech     f1=0.966887 p=0.973333 r=0.960526 (tp=73 fp=2 fn=3 true=76 pred=75)

title    f1=0.917233 p=0.913327 r=0.921172 (tp=1823 fp=173 fn=156 true=1979 pred=1996)

volume   f1=0.976872 p=0.967391 r=0.986540 (tp=1246 fp=42 fn=17 true=1263 pred=1288)

web      f1=0.636364 p=0.636364 r=0.636364 (tp=7 fp=4 fn=4 true=11 pred=11)

