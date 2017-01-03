import sys

for line in sys.stdin:
    b, fa, fb = line.split(":")[0].replace("(", "").replace(")", "").replace("\n", "").split(",")
    print(b, fa, fb)
