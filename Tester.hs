import Data.List
import System.Random
import Data.List.Split

testList = [1,2..49]::[Int]


--returns a random number from inclusively from 1 to 7

--clockwiseArmList = [[]]

--returns a random number from inclusively from 1 to 7
randomNumber::IO Int
randomNumber = getStdRandom (randomR (1,7))


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

testArmCheck::String
  | copyCheckArm ringList sudokuBoard


testBoard::[Int] -> String
testBoard list
  | length list == 0 = ""
  | length list /= 0 = show (chunksOf 7 list)


solveRing :: [Int] -> Int -> Int-> [Int]
solveRing board ringNum startIndex = do
	let missingRingNums = (getRingNumList board [] ringNum 0)
	missingRingNums


main = putStrLn (show $ lotusSolver sudokuBoard)
  --main = putStrLn $ show (lotusSolver sudokuBoard)

list=[1,2,3,4]


sudokuBoard= [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]

determineRing:: Int -> Int
determineRing boardIndex
	| ((boardIndex >= 0)&&(boardIndex <7)) = 0
	| ((boardIndex >= 7)&&(boardIndex <14)) = 1
	| ((boardIndex >= 14)&&(boardIndex < 21)) = 2
	| ((boardIndex >= 21)&&(boardIndex < 28)) = 3
	| ((boardIndex >= 28)&&(boardIndex < 35)) = 4
	| ((boardIndex >= 35)&&(boardIndex < 42)) = 5
	| ((boardIndex >= 42)&&(boardIndex < 49)) = 6


--determines which clockwise arm an index on the lotus board falls into
determineClockwise::Int->Int
determineClockwise boardIndex
  | (boardIndex == 0) || (boardIndex == 7) || (boardIndex == 15) || (boardIndex == 22) || (boardIndex == 30) || (boardIndex == 37) || (boardIndex == 45) = 0
  | (boardIndex == 1) || (boardIndex == 8) || (boardIndex == 16) || (boardIndex == 23) || (boardIndex == 31) || (boardIndex == 38) || (boardIndex == 46) = 1
  | (boardIndex == 2) || (boardIndex == 9) || (boardIndex == 17) || (boardIndex == 24) || (boardIndex == 32) || (boardIndex == 39) || (boardIndex == 47) = 2
  | (boardIndex == 3) || (boardIndex == 10)|| (boardIndex == 18) || (boardIndex == 25) || (boardIndex == 33) || (boardIndex == 40) || (boardIndex == 48) = 3
  | (boardIndex == 4) || (boardIndex == 11)|| (boardIndex == 19) || (boardIndex == 26) || (boardIndex == 34) || (boardIndex == 41) || (boardIndex == 42) = 4
  | (boardIndex == 5) || (boardIndex == 12)|| (boardIndex == 20) || (boardIndex == 27) || (boardIndex == 28) || (boardIndex == 35) || (boardIndex == 43) = 5
  | (boardIndex == 6) || (boardIndex == 13)|| (boardIndex == 14) || (boardIndex == 21) || (boardIndex == 29) || (boardIndex == 36) || (boardIndex == 44) = 6

--determines which counter clockwise arm an index on the lotus board falls into
determineCntClockwise::Int -> Int
determineCntClockwise boardIndex
 	| ((boardIndex ==0)|| (boardIndex==13)|| (boardIndex==20) || (boardIndex==26) || (boardIndex==33) || (boardIndex==39) || (boardIndex==46)) = 0
 	| ((boardIndex ==1)|| (boardIndex==7) || (boardIndex==14) || (boardIndex==27) || (boardIndex==34) || (boardIndex==40) || (boardIndex==47)) = 1
	| ((boardIndex ==2)|| (boardIndex==8) || (boardIndex==15) || (boardIndex==21) || (boardIndex==28) || (boardIndex==41) || (boardIndex==48)) = 2
  | ((boardIndex ==3)|| (boardIndex==9) || (boardIndex==16) || (boardIndex==22) || (boardIndex==29) || (boardIndex==35) || (boardIndex==42)) = 3
	| ((boardIndex ==4)|| (boardIndex==10)|| (boardIndex==17) || (boardIndex==23) || (boardIndex==30) || (boardIndex==36) || (boardIndex==43)) = 4
	| ((boardIndex ==5)|| (boardIndex==11)|| (boardIndex==18) || (boardIndex==24) || (boardIndex==31) || (boardIndex==37) || (boardIndex==44)) = 5
	| ((boardIndex ==6)|| (boardIndex==12)|| (boardIndex==19) || (boardIndex==25) || (boardIndex==32) || (boardIndex==38) || (boardIndex==45)) = 6


--checks to see if a number can validly be placed on the arm
checkArm::[[Int]] -> [Int] -> Int -> Int -> Int -> Bool
checkArm armList sudokuBoard armIndex inArm checkVal
	| (armIndex > 6) = False
  | inArm > 6 = True
  | (sudokuBoard!!((armList!!armIndex)!!inArm) == checkVal) = False
	| (sudokuBoard!!((armList!!armIndex)!!inArm) /= checkVal) = checkArm armList sudokuBoard armIndex (inArm+1) checkVal
	| otherwise = True

copyCheckArm::[[Int]] -> [Int] -> Int -> Int -> Int -> Bool
copyCheckArm armList sudokuBoard armIndex inArm index
  | (armIndex > 6) = False
  | inArm > 6 = True
  | (armList!!armIndex)!!inArm == index = copyCheckArm armList sudokuBoard armIndex (inArm+1) index
  | (sudokuBoard!!((armList!!armIndex)!!inArm)) == sudokuBoard!!index = False
  | (sudokuBoard!!((armList!!armIndex)!!inArm)) /= (sudokuBoard!!index) = copyCheckArm armList sudokuBoard armIndex (inArm+1) index
  | otherwise = True

--checks to see if the value passed in can validly be placed at the given index on the board
checkPlacement:: [Int]->Int->Bool
checkPlacement board boardIndex
	| (copyCheckRing ringList board (determineRing boardIndex) 0 boardIndex)
		&& (copyCheckArm clockwiseArmList board (determineClockwise boardIndex) 0 boardIndex)
		&& (copyCheckArm cntrclockwiseArmList board (determineCntClockwise boardIndex) 0 boardIndex) = True
	| otherwise = False

checkBoard::[Int]->Int->Bool
checkBoard board index
  | index == 49 = True
  | checkPlacement board index = checkBoard board (index+1)
  | otherwise = False

copyCheckRing::[[Int]] -> [Int] -> Int -> Int -> Int -> Bool
copyCheckRing ringList sudokuBoard ringIndex inRing index
  | (ringIndex > 6) = False
  | inRing > 6 = True
  | (((ringList!!ringIndex)!!inRing) == index) = copyCheckRing ringList sudokuBoard ringIndex (inRing+1) index
  | (sudokuBoard!!((ringList!!ringIndex)!!inRing)) == sudokuBoard!!index = False
  | (sudokuBoard!!((ringList!!ringIndex)!!inRing)) /= (sudokuBoard!!index) = copyCheckRing ringList sudokuBoard ringIndex (inRing+1) index
  | otherwise = True

--main = putStrLn (show $ checkArm clockwiseArmList 0 0)
checkCurRing::[Int] -> Int -> Int -> Bool
checkCurRing board curIndex valueInserted
	| (curIndex<7 && curIndex>(-1)) =
		((board!!0 /= valueInserted)
		&& (board!!1 /= valueInserted)
		&& (board!!2 /= valueInserted)
		&& (board!!3 /= valueInserted)
		&& (board!!4 /= valueInserted)
		&& (board!!5 /= valueInserted)
		&& (board!!6 /= valueInserted))
	| (curIndex <14 && curIndex> 6) =
		((board!!7 /= valueInserted)
		&& (board!!8 /= valueInserted)
		&& (board!!9 /= valueInserted)
		&& (board!!10 /= valueInserted)
		&& (board!!11 /= valueInserted)
		&& (board!!12 /= valueInserted)
		&& (board!!13 /= valueInserted))
	| (curIndex<21 && curIndex>13) =
		((board!!14 /= valueInserted)
		&& (board!!15 /= valueInserted)
		&& (board!!16 /= valueInserted)
		&& (board!!17 /= valueInserted)
		&& (board!!18 /= valueInserted)
		&& (board!!19 /= valueInserted)
		&& (board!!20 /= valueInserted))
	| (curIndex<28 && curIndex>20) =
		((board!!21 /= valueInserted)
		&& (board!!22 /= valueInserted)
		&& (board!!23 /= valueInserted)
		&& (board!!24 /= valueInserted)
		&& (board!!25 /= valueInserted)
		&& (board!!26 /= valueInserted)
		&& (board!!27 /= valueInserted))
	| (curIndex<35 && curIndex>27) =
		((board!!28 /= valueInserted)
		&& (board!!29 /= valueInserted)
		&& (board!!30 /= valueInserted)
		&& (board!!31 /= valueInserted)
		&& (board!!32 /= valueInserted)
		&& (board!!33 /= valueInserted)
		&& (board!!34 /= valueInserted))
	| (curIndex<42 && curIndex>34) =
		((board!!35 /= valueInserted)
		&& (board!!36 /= valueInserted)
		&& (board!!37 /= valueInserted)
		&& (board!!38 /= valueInserted)
		&& (board!!39 /= valueInserted)
		&& (board!!40 /= valueInserted)
		&& (board!!41 /= valueInserted))
	| (curIndex<49 && curIndex>41) =
		((board!!42 /= valueInserted)
		&& (board!!43 /= valueInserted)
		&& (board!!44 /= valueInserted)
		&& (board!!45 /= valueInserted)
		&& (board!!46 /= valueInserted)
		&& (board!!47 /= valueInserted)
		&& (board!!48 /= valueInserted))



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


--This function will assist the getRingNumList function by checking if a given number is located in the ring.
--If the number is not in the ring, it is appended to the list.
getMissingRingNums::[Int]->[Int]->Int->Int->[Int]
getMissingRingNums board valueList ringNumber numToCheck
  | (checkCurRing board ringNumber numToCheck) == True = numToCheck:valueList
  | otherwise = []


--Function that will be called recursively in order to collect all of the numbers that may not be in a ring.
getRingNumList::[Int]->[Int]->Int->Int->[Int]
getRingNumList board valueList ringNumber numToCheck
  | numToCheck < 8 = (getMissingRingNums board valueList ringNumber numToCheck) ++ (getRingNumList board valueList ringNumber (numToCheck+1))
  | otherwise = []


lotusSolver::[Int]->[Int]
lotusSolver sudokuBoard = fillBoard sudokuBoard 0 1

fillBoard::[Int]->Int->Int->[Int]
fillBoard board index n
  | index == 1 = board
  | (board!!index /= 0) = fillBoard board (index+1) 1
  | (n==1) && (checkPlacement a1 index) = a1
  | (n==2) && (checkPlacement a2 index) = a2
  | (n==3) && (checkPlacement a3 index) = a3
  | (n==4) && (checkPlacement a4 index) = a4
  | (n==5) && (checkPlacement a5 index) = a5
  | (n==6) && (checkPlacement a6 index) = a6
  | (n==7) && (checkPlacement a7 index) = a7
  | otherwise = []
  where
    a1 = fillBoard (insertNumber board 1 index) (index+1) 1
    a2 = fillBoard (insertNumber board 2 index) (index+1) 1
    a3 = fillBoard (insertNumber board 3 index) (index+1) 1
    a4 = fillBoard (insertNumber board 4 index) (index+1) 1
    a5 = fillBoard (insertNumber board 5 index) (index+1) 1
    a6 = fillBoard (insertNumber board 6 index) (index+1) 1
    a7 = fillBoard (insertNumber board 7 index) (index+1) 1


-- | (checkPlacement board index n) == False = fillBoard board index (n+1)
-- | (n==7) && (checkPlacement board index n) == False = []


tryNewNum::[Int]->Int->Int->[Int]
tryNewNum board myNum index
  | ((checkPlacement board index) == False) || board!!index == myNum = tryNewNum board (myNum+1) index
  | (checkPlacement board index) = deleteAt (insertAt myNum board (index)) (index+1)
  | otherwise = board


insertNumber::[Int]->Int->Int->[Int]
insertNumber board myNum index = deleteAt (insertAt myNum board (index)) (index+1)


--sudokuBoard=[x*0|x<-[0,1..48]]
