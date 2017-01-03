import os
import subprocess

import sys

out = subprocess.Popen([os.getenv('APPDATA') + r"\npm\elm-test.cmd"], stdout=subprocess.PIPE)

for l in out.stdout:
    line = l.decode("utf-8")
    if "Successfully generated" in line:
        x = line[len("Successfully generated"):].strip()
        sys.stderr.write(x + "\r\n")
        out.kill()
        sys.stderr.write("dead\r\n")
        break

sys.stderr.write("restarting\r\n")
nout = subprocess.Popen([r"C:\Program Files\nodejs\node", "--max-old-space-size=32768", x], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
for line in nout.stdout:
    if line.startswith(b"("):
            sys.stdout.buffer.write(line)
    elif line.startswith(b"Exit code: 0"):
        sys.stderr.write("done\r\n")
