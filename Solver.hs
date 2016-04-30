import Data.List
import System.Random
import Data.List.Split

testList = [1,2..49]::[Int]

clockwiseArmList = [[]]

randomNumber::IO Int
randomNumber = getStdRandom (randomR (1,7))


testBoard::[Int] -> String
testBoard list
  | length list == 0 = ""
  | length list /= 0 = show (chunksOf 7 list)

determineClockwise::Int->Int
determineClockwise boardIndex
  | (boardIndex == 0) || (boardIndex == 7) || (boardIndex == 15) || (boardIndex == 22) || (boardIndex == 30) || (boardIndex == 37) || (boardIndex == 45) = 0
  | (boardIndex == 1) || (boardIndex == 8) || (boardIndex == 16) || (boardIndex == 23) || (boardIndex == 31) || (boardIndex == 38) || (boardIndex == 46) = 1
  | (boardIndex == 2) || (boardIndex == 9) || (boardIndex == 17) || (boardIndex == 24) || (boardIndex == 32) || (boardIndex == 39) || (boardIndex == 47) = 2
  | (boardIndex == 3) || (boardIndex == 10) || (boardIndex == 18) || (boardIndex == 25) || (boardIndex == 33) || (boardIndex == 40) || (boardIndex == 48) = 3
  | (boardIndex == 4) || (boardIndex == 11) || (boardIndex == 19) || (boardIndex == 26) || (boardIndex == 34) || (boardIndex == 41) || (boardIndex == 42) = 4
  | (boardIndex == 5) || (boardIndex == 12) || (boardIndex == 20) || (boardIndex == 27) || (boardIndex == 28) || (boardIndex == 35) || (boardIndex == 43) = 5
  | (boardIndex == 6) || (boardIndex == 13) || (boardIndex == 14) || (boardIndex == 21) || (boardIndex == 29) || (boardIndex == 36) || (boardIndex == 44) = 6

--checkClockwise::[[Int]]->[Int]->Int->Bool
--checkClockwise indexBoard board index


checkArm::[Int] -> Int -> Int -> Bool
checkArm arm index armIndex
  | armIndex > 6 = False
  | (arm!!armIndex) == index = True
  | otherwise = checkArm arm index (armIndex+1)


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
insertAt :: a -> [a] -> Int -> [a]
insertAt x list indexInsertingAt = foldr insertHelper [] $ zip [0..] list
	where
		insertHelper (i,y) acc = if i == indexInsertingAt
			then x : y : acc
			else y : acc


--source=https://gist.github.com/umairsd/cdcb397941762fe02d05#file-list-index-ops-hs-L17
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


sudokuBoard=[x*1|x<-[0,1..48]]



main = do
	putStrLn (show sudokuBoard)
	putStrLn (show (deleteAt sudokuBoard 3))
