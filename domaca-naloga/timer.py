import subprocess
import re

def get_time(output):
    ptrn = re.compile(r"(\d+\.\d+) s\.")
    return float(ptrn.search(output).groups()[0])


def time(command):
    out = subprocess.check_output(command)
    return str(out)
    

def repeat(command, num):
    return sum(get_time(time(command)) for _ in range(num)) / num

if __name__ == "__main__":
    # sudoku = r"sudokuji\newman\whats_in_the_box.sdk"
    sudoku = r"sudokuji\obicajni-*.sdk"
    print(repeat(["sudoku.exe", sudoku], 10))