import sys
import json
from functools import cache

def parse_input(contents: str):
  lines = contents.splitlines()
  lsd = (line.split(' ') for line in lines)
  ls2 = [(template, tuple([int(n) for n in ns.split(',')])) for (template, ns) in lsd]
  return ls2

def flatten(xss):
    return [x for xs in xss for x in xs]

def satisfies(test, actual):
  if len(test) == 0 and len(actual) == 0:
      return True
  t, *est = test
  a, *ctual = actual
  if t == '?':
      return satisfies(est, ctual)
  if t == a:
      return satisfies(est, ctual)
  return False

def sz(ns):
  return json.dumps(ns)

@cache
def generate(template, ns):
  if len(ns) == 1:
      n = ns[0]
      b = ('.'*u + '#'*n + (len(template) -  n - u)*'.' for u in range(0,len(template) - n + 1))
      c = [a for a in b if satisfies(template, a)]
      return c
  else:
      first, *rest = ns
      rest_max_length = len(template) - first - 1
      rest_min_length = sum(rest) + len(rest) - 1
      first_max_length = len(template) - rest_min_length - 1
      first_min_length = len(template) - rest_max_length - 1
      first_options = flatten([generate(template[:l], tuple([first])) for l in range(first_min_length, first_max_length + 1)])
      rest_options = [(fo, generate(template[len(fo)+1:], tuple(rest))) for fo in first_options]
      all_options = flatten([[fo + '.' + ro for ro in ros] for (fo, ros) in rest_options])
      return set([option for option in all_options if satisfies(template, option)])

def expand(inp):
    return [('?'.join([template]*5), tuple(ns*5)) for (template, ns) in inp]

def get_answer_a(contents):
    inp = parse_input(contents)
    return sum(len(generate(template, ns)) for (template, ns) in inp)

def get_answer_b(contents):
    inp = parse_input(contents)
    return sum(len(generate(template, ns)) for (template, ns) in expand(inp))

def read_file(file_name):
    try:
        with open(file_name, 'r') as file:
            contents = file.read()
            return contents
    except FileNotFoundError:
        print("File not found.")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide the file name as a command line argument.")
    else:
        file_name = sys.argv[1]
        contents = read_file(file_name)
        print('A ', get_answer_a(contents))
        print('B: ', get_answer_b(contents))