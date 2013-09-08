""" Naive example of solving the 8 puzzle.
NOTE: NOT optimized!!!
"""
import sys, heapq

# Co-ordinates of the board. Faster lookup. Made global used in heuristic
board = { 0:(0,2), 1:(1,2), 2:(2,2),
		  3:(0,1), 4:(1,1), 5:(2,1),
		  6:(0,0), 7:(1,0), 8:(2,0)
		}	

# Print the playing board.
def printBoard(s):
    
	for i in range(0, len(s)):
		sys.stdout.write(str(s[i]) + " ") 
		if ((i+1) %3) == 0:
			print
	print        

# Calculate all possible moves
def moves(s):
	
	star = s.index('*')
	success = []

	if star > 2:
		swap = star - 3
		success.append(s[0:swap] + '*' + s[swap+1:star] + s[swap] + s[star+1:])

	if star < 6:
		swap = star + 3
		success.append(s[0:star] + s[swap] + s[star+1:swap] + '*' + s[swap+1:])

	if star % 3 > 0:
		swap = star -1
		success.append(s[0:swap] + '*' + s[swap] + s[star+1:])

	if star % 3 < 2:
		swap = star + 1
		success.append(s[0:star] + s[swap] + '*' + s[swap+1:])
	
	return success

# Calculate the manhattanDistance
def manhattanDistance(s):

	sum = 0										# ManhattanDistance

	# Go through each number on board
	for i in s:
		if i != '*':
			(x1, y1) = board[int(i)]			# Where the point should be
			(x2, y2) = board[s.index(i)]		# Where the point is
			sum += abs(x2 - x1) + abs(y2 - y1)
	
	return sum

# Runs Greedy Best First Search.
# states are stored as list: [heuristic, solutionList]
def BFS(start):
	
	fringe = []									# Fringe of exploration
	closedList = []								# Already expanded nodes
	goal = "*12345678"							# goal state

	# Push the start state on the heap.
	heapq.heappush(fringe, [manhattanDistance(start), [start]])

	# Run BFS
	while len(fringe) != 0: 
		
		# pop first node
		currNode = heapq.heappop(fringe)		
		currBoard = currNode[1][-1]
		parentList = currNode[1]		

		# Check goal state
		if currBoard == goal:
			return currNode 
		
		closedList.append(currBoard)
		L = moves(currBoard)

		for x in L:
			if x == goal:
				return [0, parentList + [x]]
			if not x in closedList:
				heapq.heappush(fringe, [manhattanDistance(x), parentList + [x]])	

	return False								# No solution

def main():

	if len(sys.argv) == 2:
		start = sys.argv[1]
	else:
		print "INPUT: python 8puzzle.py *12345678"
		exit(1)
	
	ans = BFS(start)							# Run best first search
	
	if ans != False:
		
		solution = ans[1]						# How to get to final board
		
		for board in range(len(solution)):
			printBoard(solution[board])	
		
		print "Solved in " + str(len(solution)) + " steps."

	else:

		print "No Solution. Please Format Hard drive and Install Unix"

if __name__ == '__main__':
	main()
