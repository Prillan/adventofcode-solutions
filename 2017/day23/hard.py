"""
This is the assembler translated to python, it was easier to
translate directly into an imperative language rather than
a functional one.
"""

a = b = c = d = e = f = g = h = 0
a = 1
b = 84
c = b
if a == 0:
  b = 84
  c = 84
else:
  b = 108400
  c = 108400 + 17000

def check():
  for d in range(2, b):
    if b % d == 0 and b // d >= 2:
        return True
  return False

while True:
  if check():
    h += 1

  if b == c:
    break

  b += 17
