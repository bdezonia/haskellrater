{-
  Note - rating relative to home field advantage is unnecessary when
  each team has same number of home and road games. It is added and
  subtracted equally and cancels out. Could just calc from games and
  print it but don't use it in ratings. Simpler. But when unbalanced
  number of home/road games you lose a little info.
-}

{- IMPORTS -}

import Data.List(nub, sortBy, sort, mapAccumL)
import Data.Ord(comparing)
import Text.Printf(printf)

{- TYPES -}

type Name = String

type Points = Int

type Rating = Double

type WonLossRecord = (Int, Int, Int)

type TeamInfo = (Name, Rating, WonLossRecord)

type GameResult = (Name, Points, Name, Points, Bool)

type RatingsList = [TeamInfo]

type ModelRunResult = Maybe (Double, RatingsList)

{- MAIN -}

main = do
  putStrLn "Welcome to the haskell sports rater!"

  fileText <- readFile "games.txt"

  -- This let is key. an IO String (fileText) is passed as pure String
  -- to parseGames.  
  let games = parseGames fileText
  
  printResults $ modelRun games

{- CONSTANTS -}

maxIterations = 10000

convergenceFactor = 0.0001

rateThisTeam :: [Double] -> Double
rateThisTeam = arithmeticMean
--rateThisTeam = midpoint
-- doesn't converge using these methods:
--rateThisTeam = median
--rateThisTeam = minimum
--rateThisTeam = maximum
-- todo: harmonic, geometric, contraharmonic, trimmed

{- IMPURE CODE -}

printResults :: (Int, ModelRunResult) -> IO ()
printResults (i, Nothing) = do
  putStrLn ("Model did not converge after " ++ (show i) ++ " iterations.")
printResults r = do
  printHomeFieldAdvantage r
  printSortedList r
  
printHomeFieldAdvantage :: (Int, ModelRunResult) -> IO ()
printHomeFieldAdvantage (i, (Just a)) = do
  putStr  $ "Model converged after " ++ (show i) ++
              " iterations. Home field advantage = "
  printf "%6.2f\n" (fst a)

printSortedList :: (Int, ModelRunResult) -> IO ()
printSortedList (num, (Just a)) = printSortedList' $ sortedRatings (snd a)

printSortedList' :: RatingsList -> IO ()
printSortedList' [] = return ()
printSortedList' (team : moreTeams) = do
  printf "%6.2f %3s %d-%d-%d\n" tRate tName tWins tLosses tTies
  printSortedList' moreTeams
    where
      tRate = rating team
      tName = name team
      tWins = wins $ record team
      tLosses = losses $ record team
      tTies = ties $ record team

{- PURE CODE -}

sortedRatings :: RatingsList -> RatingsList
sortedRatings ratings = sortBy (comparing rating) ratings

{-
toStrings :: RatingsList -> [String]
toStrings [] = []
toStrings (team:teams) = summary team : toStrings teams

summary :: TeamInfo -> String
summary team = (name team ++ " " ++ (show $ rating team) ++ " " ++
   show (wins (record team)) ++ "-" ++ show(losses (record team)) ++
   "-" ++ show(ties (record team)))
-}

wins :: WonLossRecord -> Int
wins (w, l, t) = w

losses :: WonLossRecord -> Int
losses (w, l, t) = l

ties :: WonLossRecord -> Int
ties (w, l, t) = t

name :: TeamInfo -> Name
name (n, _, _) = n

rating :: TeamInfo -> Rating
rating (_, r, _) = r

record :: TeamInfo -> WonLossRecord
record (_, _, r) = r

modelRun :: [GameResult] -> (Int, ModelRunResult)
modelRun games = testResult $ rateTeams games
  where
    testResult (iterations, Nothing) = (iterations, Nothing)
    testResult (iterations, Just ratingsList) =
	  (iterations, Just (homeFieldAdvantage games, ratingsList))

rateTeams :: [GameResult] -> (Int, Maybe RatingsList)
rateTeams games = solveIteratively 0 games Nothing (initialRatings games)

initialRatings :: [GameResult] -> RatingsList
initialRatings games = initTeams $ teamList games

initTeams :: [Name] -> RatingsList
initTeams nameList = map basicTeam nameList

basicTeam :: Name -> TeamInfo
basicTeam name = (name, 0, (0,0,0))

teamList :: [GameResult] -> [Name]
teamList games = teamNames games

teamNames :: [GameResult] -> [Name]
teamNames games = nub $ allTeams [] games
  where
    allTeams accum [] = accum
    allTeams accum ((a,_,b,_,_) : games) = allTeams (a : b : accum) games

solveIteratively :: Int -> [GameResult] -> Maybe RatingsList -> RatingsList -> (Int, Maybe RatingsList)
solveIteratively num games prevRatings ratings =
  if (num >= maxIterations)
    then (num, Nothing)
    else if converged prevRatings ratings
      then (num, (Just ratings))
      else solveIteratively (num+1) games (Just ratings) (updateRatings games ratings)
       
updateRatings :: [GameResult] -> RatingsList -> RatingsList
updateRatings games ratings = updateTeamRanks (teamList games) games ratings

updateTeamRanks :: [Name] -> [GameResult] -> RatingsList -> RatingsList
updateTeamRanks [] games ratings = []
updateTeamRanks (team:teams) games ratings =
  (updateTeamRank team games ratings) : (updateTeamRanks teams games ratings)

updateTeamRank :: Name -> [GameResult] -> RatingsList -> TeamInfo
updateTeamRank name games ratings = (name, newRating, record)
  where
    newRating = teamRatingCalc name games ratings
    record = teamRecord name games

teamRatingCalc :: Name -> [GameResult] -> RatingsList -> Rating
teamRatingCalc name games ratings = rateThisTeam $ newRatings name games ratings

newRatings :: Name -> [GameResult] -> RatingsList -> [Double]
newRatings name games ratings = newRatings' [] name games ratings
  where
    newRatings' accum name [] ratings = elimNaNs accum
    newRatings' accum name (game : moreGames) ratings =
      newRatings' (rating : accum) name moreGames ratings
      where rating = gameRate name game games ratings

gameRate :: Name -> GameResult -> [GameResult] -> RatingsList -> Double
gameRate name (hname, hs, vname, vs, neutral) games ratings =
  if name == hname then
    if neutral
      then (teamRating vname ratings) + fromIntegral(hs-vs)
      else (teamRating vname ratings) + fromIntegral(hs-vs) - (homeFieldAdvantage games)
  else if name == vname then
    if neutral
      then (teamRating hname ratings) + fromIntegral(vs-hs)
      else (teamRating hname ratings) + fromIntegral(vs-hs) + (homeFieldAdvantage games)
  else 0.0 / 0.0

elimNaNs :: [Double] -> [Double]
elimNaNs games = filter (not . isNaN) games

teamRating :: Name -> RatingsList -> Rating
teamRating teamname [] = -1.0 / 0.0
teamRating teamname (info : infos) =
  if (teamname == (name info))
    then rating info
    else teamRating teamname infos
  
teamRecord :: Name -> [GameResult] -> WonLossRecord
teamRecord name games = buildRecord (0,0,0) name games

buildRecord :: WonLossRecord -> Name -> [GameResult] -> WonLossRecord
buildRecord (w,l,t) name [] = (w,l,t)
buildRecord (w,l,t) name ((homeTeam, hs, visitTeam, vs, neutral):games) =
  if (name == homeTeam)
	then buildRecord (w + hwins, l + hlosses, t + hties) name games
	else if (name == visitTeam)
	  then buildRecord (w + vwins, l + vlosses, t + vties) name games
	  else buildRecord (w,l,t) name games
  where
	hwins   = if hs >  vs then 1 else 0    
	hlosses = if hs <  vs then 1 else 0    
	hties   = if hs == vs then 1 else 0    
	vwins   = if vs >  hs then 1 else 0    
	vlosses = if vs <  hs then 1 else 0    
	vties   = if vs == hs then 1 else 0    

converged :: Maybe RatingsList -> RatingsList -> Bool
converged Nothing _ = False
converged (Just prevRatings) ratings = isSmallDeviation prevRatings ratings

-- old compatible way
-- testing I see results don't match ruby version
--   range of values slightly expanded
--   order slightly different
{-
isSmallDeviation :: RatingsList -> RatingsList -> Bool
isSmallDeviation [] [] = True
isSmallDeviation (a1:as) (b1:bs) =
  if abs (rating a1 - rating b1) >= 0.01
    then False
    else isSmallDeviation as bs
-}

-- new way
isSmallDeviation :: RatingsList -> RatingsList -> Bool
isSmallDeviation rating1 rating2 = (sum (square deviations)) < convergenceFactor
  where
    deviations = zipWith ratingDiff rating1 rating2
    square list = zipWith (*) list list
    ratingDiff (_,r1,_) (_,r2,_) = r1 - r2

homeFieldAdvantage :: [GameResult] -> Double
homeFieldAdvantage gameList = arithmeticMean $ scoreDeltas gameList

scoreDeltas :: [GameResult] -> [Double]
scoreDeltas [] = []
scoreDeltas ((_,_,_,_,True) : games) = scoreDeltas games
scoreDeltas ((_, score1, _, score2, False) : games) =
  doubleDiff : scoreDeltas games
  where
    doubleDiff = fromIntegral(score2) - fromIntegral(score1)

arithmeticMean :: [Double] -> Double
arithmeticMean list = (sum list) / fromIntegral (length list)

trimmedMean :: Int -> [Double] -> Double
trimmedMean _ _ = 0

geometricMean :: [Double] -> Double
geometricMean _ = 0

harmonicMean :: [Double] -> Double
harmonicMean _ = 0

contraharmonicMean :: [Double] -> Double
contraharmonicMean _ = 0

median :: [Double] -> Double
median [] = 0.0 / 0.0
median list =
  if ((mod numElements 2) == 1)
    then values !! middle
    else ((values !! (middle - 1)) + (values !! middle)) / 2.0
  where
    numElements = length list
    middle = div numElements 2
    values = sort list

midpoint :: [Double] -> Double
midpoint list = midpoint' list (1.0/0.0 , -1.0/0.0)

midpoint' [] minMaxTuple = ((fst minMaxTuple) + (snd minMaxTuple)) / 2.0
midpoint' (x:xs) (mn,mx) = midpoint' xs (smallest,largest)
  where
    smallest = min x mn
    largest = max x mx
    
{-
-- no need to define this
-- min == minimum
min :: [Double] -> Double
min list = min' (1.0/0.0) list
  where
    min' mn [] = mxn
    min' mn (v : vs) =
      if (v < mn)
        then min' v vs
        else min' mx vs
-}

{-
-- no need to define this
-- max == maximum
max :: [Double] -> Double
max list = max' (-1.0/0.0) list
  where
    max' mx [] = mx
    max' mx (v : vs) =
      if (v > mx)
        then max' v vs
        else max' mx vs
-}

{-
numNonNeutralGames :: [GameResult] -> Int
numNonNeutralGames x = numNonNeutral' 0 x
  where
    numNonNeutral' i [] = i
    numNonNeutral' i ((_,_,_,_,neutral) : games) =
      if (neutral)
        then numNonNeutral' i games
        else numNonNeutral' (i+1) games
-}

parseGames :: String -> [GameResult]
parseGames contents = toGames maybeGames
  where
    strings = lines contents
    maybeGames = parse strings

parse :: [String] -> [Maybe GameResult]
parse strings = parse' [] strings
  where
    parse' accum [] = accum
    parse' accum (string : moreStrings) =
      parse' (parseString string : accum) moreStrings

parseString :: String -> Maybe GameResult
parseString [] = Nothing
parseString string =
  if (string == "")
    then Nothing
    else if ((string !! 0) == '[' ) then Nothing
    else parseGame string

parseGame :: String -> Maybe GameResult
parseGame string =
  if (((length tokens) - offset) < 5)
    then Nothing
    else Just (homeTeam, homeScore, visitTeam, visitScore, neutral)
  where
    tokens = words string
    neutral = ((tokens !! 0) == "neutral")
    offset = if neutral then 1 else 0
    homeTeam = tokens !! (offset+0)
    homeScoreString = tokens !! (offset+1)
    homeScore = read homeScoreString
    visitTeam = tokens !! (offset+3)
    visitScoreString = tokens !! (offset+4)
    visitScore = read visitScoreString

toGames :: [Maybe GameResult] -> [GameResult]
toGames maybeResults = toGames' [] maybeResults
  where
    toGames' accum [] = accum
    toGames' accum ((Just result) : moreResults) = toGames' (result:accum) moreResults
    toGames' accum ((Nothing):moreResults) = toGames' accum moreResults
 
