import Data.List
import System.Random
import Data.List.Split

testList = [0,1..48]::[Int]

randomNumber::IO Int
randomNumber = getStdRandom (randomR (1,7))


testBoard::[Int] -> String
testBoard list
  | length list == 0 = ""
  | length list /= 0 = show (chunksOf 7 list)


checkCurRing::[Int]->Int->Int->Int->Bool
checkCurRing board curIndex x valueInserted
  | (curIndex < 7 && curIndex >= 0) = ((board !! x) /= valueInserted) && (if (x>6) then (1==1) else (checkCurRing board curIndex (x+1) valueInserted))

--checkClockwise::[[Int]]->[Int]->Int->Bool
--checkClockwise indexBoard board index

--getArm::[[Int]]->Int->[Int]
--getArm indexBoard curIndex


checkArm::[Int] -> Int -> Int -> Bool
checkArm arm index armIndex
  | armIndex > 6 = False
  | (arm!!armIndex) == index = True
  | otherwise = checkArm arm index (armIndex+1)
