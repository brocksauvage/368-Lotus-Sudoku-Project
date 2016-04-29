import Data.List
import System.Random
import Data.List.Split

testList = [0,1..48]::[Int]
sudokuBoard=[x*0|x<-[0,1..48]]

randomNumber::IO Int
randomNumber = getStdRandom (randomR (1,7))


testBoard::[Int] -> String
testBoard list
  | length list == 0 = ""
  | length list /= 0 = show (chunksOf 7 list)



--checkCurRing::[Int]->Int->Int->Int->Bool
--checkCurRing board curIndex x valueInserted
--  | (curIndex < 7 && curIndex >= 0) = ((board !! x) /= valueInserted) && (if (x>6) then (1==1) else (checkCurRing board curIndex (x+1) valueInserted))

--checkClockwise::[[Int]]->[Int]->Int->Bool
--checkClockwise indexBoard board index

--getArm::[[Int]]->Int->[Int]
--getArm indexBoard curIndex



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



--src=https://gist.github.com/umairsd/cdcb397941762fe02d05#file-list-index-ops-hs-L17
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys n = foldr insertHelper [] $ zip [0..] ys
	where 
		insertHelper (i,y) acc = if i == n
			then x : y : acc
			else y : acc



















































	
