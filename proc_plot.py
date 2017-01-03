import sys

import matplotlib
import numpy

matplotlib.use('Agg')
import matplotlib.pyplot as plt


datatype = [('index', numpy.float32), ('floati', numpy.float32),
            ('floatq', numpy.float32)]
filename = 'bigdata.bin'


def main():
    plt.yscale('log')
    plt.xscale('log')
    c = 0
    for line in sys.stdin:
        c += 1
        if c < 10:
            continue
        c = 0
        b, fa, fb = line.split(":")[0].replace("(", "").replace(")", "").replace("\n", "").split(",")
        fa = float(fa)
        fb = float(fb)
        #print(b, fa, fb)
        if b == "True":
            plt.plot(fa, fb, 'b,')
        else:
            plt.plot(fa, fb, 'r,')

    plt.grid(True)
    plt.title("Signal-Diagram")
    plt.xlabel("Sample")
    plt.ylabel("In-Phase")
    plt.savefig('foo2.png')


if __name__ == "__main__":
    main()
