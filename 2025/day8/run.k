/ It doesn't get more ugly than this
if:"input.txt"
i:(`I$'","\)'0:if
sq:{x*x}
d:{[a;b] %+/sq'a-b}
ds:(+i)d/:i
ut:{[l] +{[l;x] (x#1),(l-x)#0}[l;]' 1+!l}

c:$[if~"input.txt";1000;10]

ids:+&(-=#ds)++ut[#ds]
sids:ids@<(ds.)'ids

ones:{[c;i] (=c) .[;;|;1]/i}
s:s|+s:ones[#ds;c#sids]

mm:(+/*)\:
cmm:{{1&mm[x;x]}/x}

*/3#|x@<x:#'.=cmm[s]

set:{[c;xy]; (x;y):xy; c .[;;|;1]/((x;y);(y;x))}

bin:{[t;f;mi;mx]; v:f@x:_mi+((mx-mi)%2); $[v=t;x;v<t;o[t;f;x+1;mx];o[t;f;mi;x-1]]}

f:{[i] .=cmm@(=#ds) set/i#sids}
step:{[a]; (c;r):a; (cmm@set[c;*r]; 1_r)}

/ Find any point that leads to two groups
ti:10000-(bin[2;{#f@10000-x};100;9000])
grps:f[ti]

/ Find the first occurrance of "the" remaining, lonely thing assumes
/ that the setup will be 1:999 at the end Could easily be adapted to
/ iterate until we hit 1 group, but the matrix multiplication is
/ really, really slow here.
last:*grps@*&1=#'grps
final:*ti+&|/last=+ti_sids

*/(i@sids@final)[;0]
