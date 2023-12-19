import sys
import ipdb
from heapq import heappop, heappush 

def read_file(file_name):
    try:
        with open(file_name, 'r') as file:
            contents = file.read()
            return contents
    except FileNotFoundError:
        print("File not found.")

def parse(contents):
    return [[int(x) for x in line] for line in contents.splitlines()]

def in_bounds(array, a):
    (y,x) = a
    return y < len(array) and x < len(array[0]) and y >= 0 and x >= 0

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please provide the file name as a command line argument.")
    else:
        file_name = sys.argv[1]
        arr = parse(read_file(file_name))
        seen = set()
        queue = [(0,0,0,0,0,0)]
        while queue:
            heat, row, column, drows, dcolumns, steps = heappop(queue)
            if row == len(arr)-1 and column == len(arr[0]) -1:
                print(heat)
                break
            if not in_bounds(arr, (row, column)):
                continue
            if (row, column, drows, dcolumns, steps) in seen:
                continue
            seen.add((row, column, drows, dcolumns, steps))
            if steps < 3 and (drows, dcolumns) != (0,0):
                next_row = row + drows
                next_column = column + dcolumns
                if in_bounds(arr, (next_row, next_column)):
                    heappush(queue, (heat + arr[next_row][next_column], next_row, next_column, drows, dcolumns, steps + 1))
            for next_dr, next_dc in [(0,1),(0,-1),(1,0),(-1,0)]:
                if (next_dr, next_dc) != (drows, dcolumns) and (next_dr, next_dc) != (-drows,-dcolumns):
                    next_row = row + next_dr
                    next_column = column + next_dc
                    if in_bounds(arr, (next_row, next_column)):
                        heappush(queue, (heat + arr[next_row][next_column], next_row, next_column, next_dr, next_dc, 1))

  

# class Vertex():
#     def __init__(self, y, x, dy, dx, h):
#       self.x = x
#       self.y = y
#       self.dx = dx
#       self.dy = dy
#       self.h = h
    
#     def key(self):
#        return (self.y, self.x, self.dy, self.dy, self.h)

#     def __str__(self):
#         return f'|C:({self.y},{self.x}) D:({self.dy},{self.dx}) H:{self.h}|'

#     def __repr__(self):
#         return f'|C:({self.y},{self.x}) D:({self.dy},{self.dx}) H:{self.h}|'
    
#     def __lt__(self, other):
#        return self.key() < other.key()

# def parse(contents):
#     return [[int(x) for x in line] for line in contents.splitlines()]

# def next_steps(a, v: Vertex):
#     (y,x) = (v.y, v.x)
#     choices = []
#     (yd, xd) = (v.dy, v.dx)
#     if in_bounds(a, (y+1, x)) and yd != 3 and yd >= 0:
#       choices.append((1,0))
#     if in_bounds(a, (y-1, x)) and yd != -3 and yd <= 0:
#       choices.append((-1,0))
#     if in_bounds(a, (y, x+1)) and xd != 3 and xd >= 0:
#       choices.append((0,1))
#     if in_bounds(a, (y, x-1)) and xd != -3 and xd <= 0:
#       choices.append((0,-1))
#     return choices
  
# def in_bounds(array, a):
#     (y,x) = a
#     return y < len(array) and x < len(array[0]) and y >= 0 and x >= 0

# if __name__ == "__main__":
#     if len(sys.argv) < 2:
#         print("Please provide the file name as a command line argument.")
#     else:
#         file_name = sys.argv[1]
#         contents = read_file(file_name)
#         que = []
#         v = Vertex(0,0,0,0,0)
#         heappush(que, (0, v))
#         visited = { }
#         visited[v.key()] = v
#         a = parse(contents)
#         end = (len(a) - 1, len(a[0])-1)
#         print('end', end)
#         while que:
#             (distance, v) = heappop(que)
#             visited[v.key()] = v
#             l = len(visited)
#             print(distance)
#             if (v.y, v.x) == end:
#                 print(distance, v)
#                 break
#             n = next_steps(a, v)
#             new_vs = [Vertex(y=v.y+dy, x=v.x+dx, dy=v.dy+dy if dy else 0, dx=v.dx+dx if dx else 0, h=a[v.y+dy][v.x+dx]) for (dy, dx) in n]
#             new_vs = [new_v for new_v in new_vs if not visited.get(new_v.key(), None)]
#             for new_v in new_vs:
#                 heappush(que, (distance+new_v.h, new_v))

