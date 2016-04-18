# Homework 1
# Author: Tien Ho
# Date: 04 February 2015
#
# This program finds the indices of the correct sudoku solutions
# by checking all the 50 solutions contained in a 9x9x50 array
# in the provided sudoku dataset and making sure that all the
# rows, the columns, and 3x3 squares contain unique numbers from
# 1-9.


# This method checks that the input row contains unique numbers.
checkForRow = function (row) {
	# create an empty vector to hold the numbers that the row contains
	usedNumbers = c()
	for (i in 1:length(row)) {
		number = row[i]
		# only insert the number into the vector if it does not belong to
		# that vector (in other words, if it is unique among all the used numbers);
		# if the number already exists in the vector, return false because the
		# row uses repeated numbers.
		if (!any(number == usedNumbers)) {
			usedNumbers = c(usedNumbers, number)
		} else {
			return (FALSE)
		}
	}

	return (TRUE)
}

# This method checks that the input column contains unique numbers.
checkForColumn = function (column) {
	# create an empty vector to hold the numbers that the column contains
	usedNumbers = c()
	for (i in 1:length(column)) {
		number = column[i]
		# only insert the number into the vector if it does not belong to
		# that vector (in other words, if it is unique among all the used numbers);
		# if the number already exists in the vector, return false because the
		# column uses repeated numbers.
		if (!any(number == usedNumbers)) {
			usedNumbers = c(usedNumbers, number)
		} else {
			return (FALSE)
		}
	}

	return (TRUE)
}

# This method checks that the input 3x3 square contains unique numbers.
checkForSquare = function (square) {
	# create an empty vector to hold the numbers that the square contains
	usedNumbers = c()
	for (i in 1:nrow(square)) {
		for (j in 1:ncol(square)) {
			number = square[i, j]
			# only insert the number into the vector if it does not belong to
			# that vector (in other words, if it is unique among all the used numbers);
			# if the number already exists in the vector, return false because the
			# square uses repeated numbers.
			if (!any(number == usedNumbers)) {
				usedNumbers = c(usedNumbers, number)
			} else {
				return (FALSE)
			}
		}
	}

	return (TRUE)
}

# This method checks that the input sudoku solution contained
# in a 9x9 array is correct by checking all its rows, columns,
# and 3x3 squares for unique numbers.
isCorrect = function (sudokuSolution) {
	row = c(1:nrow(sudokuSolution))
	column = c(1:ncol(sudokuSolution))

  # loop through all the rows and check for unique numbers
	for (i in row) {
		row = sudokuSolution[i, ]
		if (!checkForRow(row)) {
			return (FALSE)
		}
	}

  # loop through all the columns and check for unique numbers
	for (i in column) {
		column = sudokuSolution[, i]
		if (!checkForColumn(column)) {
			return (FALSE)
		}
	}

  # loop through all the 3x3 squares and check for unique numbers
	for (i in row[row%%3 == 1]) {
		for (j in column[column%%3 == 1]) {
			square = sudokuSolution[i:(i+2), j:(j+2)]
			if (!checkForSquare(square)) {
				return (FALSE)
			}
		}
	}

	return (TRUE)
}

# This method returns the indices of the correct sudoku solutions.
findCorrectSudokuSolutions = function (sudokuSolutions) {
	correctIndices = c()
	# find the number of solutions in the dataset
	numberOfSolutions = dim(sudokuSolutions)[3]
	for (solution in 1:numberOfSolutions) {
		if (isCorrect(sudokuSolutions[, , solution]) == TRUE) {
			correctIndices = c(correctIndices, solution)
		}
	}

	return (correctIndices)
}

# Load the provided sudoku dataset
load("IntermediateR.RData")

# Find the indices of the correct solutions in the dataset stored
# in sudokus
findCorrectSudokuSolutions(sudokus)
