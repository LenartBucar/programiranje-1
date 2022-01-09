import subprocess
import re

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
    # sudoku = r"sudokuji\newman\whats_in_the_box.sdk"
    sudokus = [r"sudokuji\obicajni-*.sdk"]
    # sudokus = [r"sudokuji\horjak\obicajni-*.sdk", r"sudokuji\horjak\puscice-*.sdk", r"sudokuji\horjak\termometri-*.sdk", r"sudokuji\horjak\*.sdk"]
    # sudokus = [r"sudokuji\horjak\puscice-*.sdk"]
    for sudoku in sudokus:
        print(sudoku, repeat(["sudoku.exe", sudoku], 100))