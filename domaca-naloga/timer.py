import subprocess
import re
import argparse

def get_time(output):
    ptrn = re.compile(r"(\d+\.\d+) s\.")
    return float(ptrn.search(output).groups()[0])


def time(command):
    out = subprocess.check_output(command)
    return str(out)
    

def repeat(command, num):
    times = [get_time(time(command)) for _ in range(num)]
    return sum(times) / num, min(times), max(times)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("file")
    parser.add_argument("-n", "--repeats", type=int, default=100)
    args = parser.parse_args()
    print(f"Solving {args.file} over {args.repeats} repeats (avg, best, worst): {repeat(['sudoku.exe', args.file], args.repeats)}")