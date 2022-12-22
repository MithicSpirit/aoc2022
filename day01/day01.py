#!/usr/bin/env python

def main():
    with open("day01/input", "r") as fin:
        contents = fin.read()
    print(get_max_sum(contents))
    print(get3_max_sum(contents))


def get_max_sum(data):
    return max(map(sum, parse(data)))

def get3_max_sum(data):
    vals = map(sum, parse(data))
    first = second = third = 0
    for v in vals:
        if v <= third:
            continue
        third = v
        if third <= second:
            continue
        second, third = third, second
        if second <= first:
            continue
        first, second = second, first
    return first + second + third


def parse(data):
    lines = data.split("\n")
    groups = []
    current = []
    for line in lines:
        if line == "":
            groups.append(current)
            current = []
        else:
            current.append(int(line))
    return groups


if __name__ == "__main__":
    main()
