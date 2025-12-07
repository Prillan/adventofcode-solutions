i:0:"input.txt"
mi:{x?|/x} / first index of maximum
s:{[a] `I$(a[x]; `c$|/(1+x:mi[-1_a])_a)}

/ not great, but finally got something working
it:{(i; j; a):x; (1+i+c?|/c:(1+-j)_(1+i)_a; j+-1; a)}
s2:{[a] r:{0<x[1]} it\(-1; 12; a); `I$a@1_r[;0]}

+/s'i
+/s2'i
