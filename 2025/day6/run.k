i:0:"input.txt"

s:{(" "\x)^,""}
solve:{f:.*|x; f/`I$'-1_x}
+/solve'+s'i

n:{`I$x^" "}'+-1_i
g:{x@&~^x}'(0,&^n)_n
ops:,/" "\*|i
+/{(a;b):x;(.b)/a}'+(g;ops)
