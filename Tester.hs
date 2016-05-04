import Data.List
import System.Random
import Data.List.Split


clockwiseArmList = [[0,7,15,22,30,37,45],
                    [1,8,16,23,31,38,46],
                    [2,9,17,24,32,39,47],
                    [3,10,18,25,33,40,48],
                    [4,11,19,26,34,41,42],
                    [5,12,20,27,28,35,43],
                    [6,13,14,21,29,36,44]]::[[Int]]

cntrclockwiseArmList = [[0,13,20,26,33,39,46],
                        [1,7,14,27,34,40,47],
                        [2,8,15,21,28,41,48],
                        [3,9,16,22,29,35,42],
                        [4,10,17,23,30,36,43],
                        [5,11,18,24,31,37,44],
                        [6,12,19,25,32,38,45]]::[[Int]]


ringList = [[0..6],
            [7..13],
            [14..20],
      	    [21..27],
      	    [28..34],
      	    [35..41],
      	    [42..48]]::[[Int]]




sudokuBoard= [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]

determineRing:: Int -> Int
determineRing boardIndex
  | boardIndex > 48 = -1
	| ((boardIndex >= 0)&&(boardIndex <7)) = 0
	| ((boardIndex >= 7)&&(boardIndex <14)) = 1
	| ((boardIndex >= 14)&&(boardIndex < 21)) = 2
	| ((boardIndex >= 21)&&(boardIndex < 28)) = 3
	| ((boardIndex >= 28)&&(boardIndex < 35)) = 4
	| ((boardIndex >= 35)&&(boardIndex < 42)) = 5
	| ((boardIndex >= 42)&&(boardIndex < 49)) = 6
  | otherwise = -1

--determines which clockwise arm an index on the lotus board falls into
determineClockwise::Int->Int
determineClockwise boardIndex
  | boardIndex > 48 = -1
  | (boardIndex == 0) || (boardIndex == 7) || (boardIndex == 15) || (boardIndex == 22) || (boardIndex == 30) || (boardIndex == 37) || (boardIndex == 45) = 0
  | (boardIndex == 1) || (boardIndex == 8) || (boardIndex == 16) || (boardIndex == 23) || (boardIndex == 31) || (boardIndex == 38) || (boardIndex == 46) = 1
  | (boardIndex == 2) || (boardIndex == 9) || (boardIndex == 17) || (boardIndex == 24) || (boardIndex == 32) || (boardIndex == 39) || (boardIndex == 47) = 2
  | (boardIndex == 3) || (boardIndex == 10)|| (boardIndex == 18) || (boardIndex == 25) || (boardIndex == 33) || (boardIndex == 40) || (boardIndex == 48) = 3
  | (boardIndex == 4) || (boardIndex == 11)|| (boardIndex == 19) || (boardIndex == 26) || (boardIndex == 34) || (boardIndex == 41) || (boardIndex == 42) = 4
  | (boardIndex == 5) || (boardIndex == 12)|| (boardIndex == 20) || (boardIndex == 27) || (boardIndex == 28) || (boardIndex == 35) || (boardIndex == 43) = 5
  | (boardIndex == 6) || (boardIndex == 13)|| (boardIndex == 14) || (boardIndex == 21) || (boardIndex == 29) || (boardIndex == 36) || (boardIndex == 44) = 6
  | otherwise = -1

--determines which counter clockwise arm an index on the lotus board falls into
determineCntClockwise::Int -> Int
determineCntClockwise boardIndex
  | boardIndex > 48 = -1
 	| ((boardIndex ==0)|| (boardIndex==13)|| (boardIndex==20) || (boardIndex==26) || (boardIndex==33) || (boardIndex==39) || (boardIndex==46)) = 0
 	| ((boardIndex ==1)|| (boardIndex==7) || (boardIndex==14) || (boardIndex==27) || (boardIndex==34) || (boardIndex==40) || (boardIndex==47)) = 1
	| ((boardIndex ==2)|| (boardIndex==8) || (boardIndex==15) || (boardIndex==21) || (boardIndex==28) || (boardIndex==41) || (boardIndex==48)) = 2
  | ((boardIndex ==3)|| (boardIndex==9) || (boardIndex==16) || (boardIndex==22) || (boardIndex==29) || (boardIndex==35) || (boardIndex==42)) = 3
	| ((boardIndex ==4)|| (boardIndex==10)|| (boardIndex==17) || (boardIndex==23) || (boardIndex==30) || (boardIndex==36) || (boardIndex==43)) = 4
	| ((boardIndex ==5)|| (boardIndex==11)|| (boardIndex==18) || (boardIndex==24) || (boardIndex==31) || (boardIndex==37) || (boardIndex==44)) = 5
	| ((boardIndex ==6)|| (boardIndex==12)|| (boardIndex==19) || (boardIndex==25) || (boardIndex==32) || (boardIndex==38) || (boardIndex==45)) = 6
  | otherwise = -1

copyCheckArm::[[Int]] -> [Int] -> Int -> Int -> Int -> Bool
copyCheckArm armList sudokuBoard armIndex inArm index
  | (armIndex > 6) = False
  | inArm > 6 = True
  | index > 48 = True
  | (armList!!armIndex)!!inArm == index = copyCheckArm armList sudokuBoard armIndex (inArm+1) index
  | (sudokuBoard!!((armList!!armIndex)!!inArm)) == sudokuBoard!!index = False
  | (sudokuBoard!!((armList!!armIndex)!!inArm)) /= (sudokuBoard!!index) = copyCheckArm armList sudokuBoard armIndex (inArm+1) index
  | otherwise = True


checkBoard::[Int]->Int->Bool
checkBoard board index
  | index > 48 = True
  | board!!index == 0 = checkBoard board (index+1)
  | (copyCheckRing ringList board (determineRing index) 0 index)
		&& (copyCheckArm clockwiseArmList board (determineClockwise index) 0 index)
		&& (copyCheckArm cntrclockwiseArmList board (determineCntClockwise index) 0 index) = checkBoard board (index+1)
  | otherwise = False

copyCheckRing::[[Int]] -> [Int] -> Int -> Int -> Int -> Bool
copyCheckRing ringList sudokuBoard ringIndex inRing index
  | (ringIndex > 6) = False
  | inRing > 6 = True
  | index > 48 = True
  | (((ringList!!ringIndex)!!inRing) == index) = copyCheckRing ringList sudokuBoard ringIndex (inRing+1) index
  | (sudokuBoard!!((ringList!!ringIndex)!!inRing)) == sudokuBoard!!index = False
  | (sudokuBoard!!((ringList!!ringIndex)!!inRing)) /= (sudokuBoard!!index) = copyCheckRing ringList sudokuBoard ringIndex (inRing+1) index
  | otherwise = True



--source=https://gist.github.com/umairsd/cdcb397941762fe02d05#file-list-index-ops-hs-L17
--inserts a value at a given position in a list
insertAt :: a -> [a] -> Int -> [a]
insertAt x list indexInsertingAt = foldr insertHelper [] $ zip [0..] list
	where
		insertHelper (i,y) acc = if i == indexInsertingAt
			then x : y : acc
			else y : acc


--source=https://gist.github.com/umairsd/cdcb397941762fe02d05#file-list-index-ops-hs-L17
--deletes a value from the list at given index, decreases size of the list
deleteAt :: [a] -> Int -> [a]
deleteAt ys n = foldr deleteHelper [] $ zip [0..] ys
	where
		deleteHelper (i,y) acc = if i == n then acc else y : acc



lotusSolver::[Int]->[Int]
lotusSolver sudokuBoard = fillBoard sudokuBoard 0 1

fillBoard::[Int]->Int->Int->[Int]
fillBoard board index n
  | (board!!index /= 0) = fillBoard board (index+1) n
  | index > 48 = board
  | (n==1) && (checkBoard a1 index) = a1
  | (n==2) && (checkBoard a2 index) = a2
  | (n==3) && (checkBoard a3 index) = a3
  | (n==4) && (checkBoard a4 index) = a4
  | (n==5) && (checkBoard a5 index) = a5
  | (n==6) && (checkBoard a6 index) = a6
  | (n==7) && (checkBoard a7 index) = a7
  | otherwise = []
  where
    a1 = fillBoard (insertNumber board 1 index) (index+1) 1
    a2 = fillBoard (insertNumber board 2 index) (index+1) 1
    a3 = fillBoard (insertNumber board 3 index) (index+1) 1
    a4 = fillBoard (insertNumber board 4 index) (index+1) 1
    a5 = fillBoard (insertNumber board 5 index) (index+1) 1
    a6 = fillBoard (insertNumber board 6 index) (index+1) 1
    a7 = fillBoard (insertNumber board 7 index) (index+1) 1



insertNumber::[Int]->Int->Int->[Int]
insertNumber board myNum index
  | index > 48 = board
  | index < 49 = deleteAt (insertAt myNum board (index)) (index+1)
