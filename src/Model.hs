module Model where

import Data.List (find, zipWith4)
import Data.Maybe (catMaybes)
import System.Random

data Experiment = Experiment
  { bank           :: Bank
  , newRequests    :: RequestsQueue
  , parameters     :: Parameters
  , statistic      :: Statistic
  , isInitialized  :: Bool
  , isPaused       :: Bool
  , isEnded        :: Bool
  , lastUsedNumber :: Int
  } deriving (Eq)

initExperiment :: Experiment
initExperiment = Experiment
  { bank           = initBank
  , newRequests    = initNewRequests
  , parameters     = initParameters
  , statistic      = initStatistic
  , isInitialized  = False
  , isPaused       = True
  , isEnded        = False
  , lastUsedNumber = 0
  }

changeParameter :: ParametersField -> ChangeAction -> Experiment -> Experiment
changeParameter ClerksNum action ex = ex
  { parameters = (parameters ex)
      { clerksNum = case action of
          Sub -> clerksNum (parameters ex) - 1
          Add -> clerksNum (parameters ex) + 1
      }
  , bank = case action of
      Sub -> delClerk (bank ex)
      Add -> addClerk (bank ex)
  }
changeParameter field action ex = ex
  { parameters = case field of
      TimeStep         -> (parameters ex)
        { timeStep = case action of
            Sub -> timeStep (parameters ex) - 10
            Add -> timeStep (parameters ex) + 10
        }
      SimulationPeriod -> (parameters ex)
        { simulationPeriod = case action of
            Sub -> simulationPeriod (parameters ex) - 7
            Add -> simulationPeriod (parameters ex) + 7
        }
      ClerksNum        -> (parameters ex)
        { clerksNum = case action of
            Sub -> clerksNum (parameters ex) - 1
            Add -> clerksNum (parameters ex) + 1
        }
      QueueLenLimit    -> (parameters ex)
        { queueLenLimit = case action of
            Sub -> queueLenLimit (parameters ex) - 5
            Add -> queueLenLimit (parameters ex) + 5
        }
      ServiceMinTime   -> (parameters ex)
        { serviceMinTime = case action of
            Sub -> serviceMinTime (parameters ex) - 4
            Add -> serviceMinTime (parameters ex) + 4
        }
      ServiceMaxTime   -> (parameters ex)
        { serviceMaxTime = case action of
            Sub -> serviceMaxTime (parameters ex) - 4
            Add -> serviceMaxTime (parameters ex) + 4
        }
      ComingMinTime    -> (parameters ex)
        { comingMinTime = case action of
            Sub -> comingMinTime (parameters ex) - 2
            Add -> comingMinTime (parameters ex) + 2
        }
      ComingMaxTime    -> (parameters ex)
        { comingMaxTime = case action of
            Sub -> comingMaxTime (parameters ex) - 2
            Add -> comingMaxTime (parameters ex) + 2
        }
  }

startExperiment :: StdGen -> Experiment -> Experiment
startExperiment g ex = ex
  { newRequests   = genNewRequests g (parameters ex)
  , isInitialized = True
  }

playPauseExperiment :: Experiment -> Experiment
playPauseExperiment ex = ex
  { isPaused  = not (isPaused ex)
  }

resetExperiment :: Experiment
resetExperiment = initExperiment

finishExperiment :: Experiment -> Experiment
finishExperiment ex = addTime (snd defaultTimeSteps) ex

addToQueue :: [Request] -> Experiment -> Experiment
addToQueue reqs ex = newExperiment
  { statistic = (statistic newExperiment)
    { leftClientsNum = newLeftClientsNum
    }
  }
  where
    newExperiment       = addClients comingClients ex
    comingClients       = filter inTime reqs
    notComingClientsNum = length reqs - length comingClients
    newLeftClientsNum   = leftClientsNum (statistic newExperiment) + notComingClientsNum
    inTime (Request _ _ _ dur_) = isWorkingTime
      (spentDays (statistic ex)) (dur_ + currentTime (statistic ex))

leftFromQueue :: Experiment -> Experiment
leftFromQueue ex = ex 
  { bank = (bank ex)
    { queue = newQueue
    }
  , statistic = (statistic ex)
    { leftClientsNum = newLeftClientsNum
    }
  }
  where
    newQueue          = filter notLeft (queue (bank ex))
    leftClients       = filter isLeft (queue (bank ex))
    notLeft client    = not (client `elem` leftClients)
    newLeftClientsNum = leftClientsNum (statistic ex) + length leftClients
    isLeft (Client (Request _ _ _ dur_) _ _) = not $ isWorkingTime
      (spentDays (statistic ex)) (dur_ + currentTime (statistic ex))

setWorkToClerks :: Experiment -> Experiment
setWorkToClerks ex = ex
  { bank = (bank ex)
    { queue     = newQueue
    , clerks    = newClerks
    , infoTable = newInfoTable
    }
  , statistic = (statistic ex)
    { servicedClientsNum = servicedClientsNum (statistic ex) + length readyClients
    }
  }
  where
    freeClerks    = filter withoutWork (clerks (bank ex))
    readyClients  = take (length freeClerks) (queue (bank ex))
    changedClerks = zipWith takeClientToClerk readyClients freeClerks
    newClerks     = setWork <$> clerks (bank ex)
    setWork clerk =
      case find (\clerk_ -> name clerk_ == name clerk) changedClerks of
        Nothing  -> clerk
        Just new -> new
    newInfoTable  = updateInfoTable changedClerks (infoTable (bank ex))
    newQueue      = drop (length readyClients) (queue (bank ex))

updateClients :: Experiment -> Experiment
updateClients ex = newExperiment
  { newRequests = drop amount (newRequests ex)
  , bank        = (bank newExperiment)
    { queue = addWaitTime <$> queue (bank newExperiment)
    }
  }
  where
    newExperiment = addToQueue newClients $ leftFromQueue $ setWorkToClerks ex
    newClients    = takeWhile isComing (newRequests ex)
    amount        = length newClients
    isComing (Request _ day_ time_ _) =
      day_ <= spentDays (statistic ex) && time_ <= currentTime (statistic ex)

updateClerks :: Experiment -> Experiment
updateClerks ex = ex
  { bank = (bank ex)
    { clerks    = serviceComplete <$> newClerks
    , infoTable = newInfoTable
    }
  , statistic = (statistic ex)
    { bankProfit = bankProfit (statistic ex) + newProfit
    }
  }
  where
    newClerks     = serviceTime <$> clerks (bank ex)
    newProfit     = sum $ serviceProfit <$> newClerks
    changedClerks = filter isServiced newClerks
    newInfoTable  = updateInfoTable (serviceComplete <$> changedClerks)
      (infoTable (bank ex))

addTime :: Minutes -> Experiment -> Experiment
addTime t ex
  | not (isWorkingTime (spentDays (statistic ex)) (currentTime (statistic ex)))
    && t == 0 && not (isEnded ex) = addTime 1 ex
  | t == 0    = ex
  | otherwise = addTime (t - 1) newExperiment
    { statistic = (statistic newExperiment)
      { spentDays   = newSpentDays
      , currentDay  = newDay
      , currentTime = if simulationIsEnded
        then 0
        else newTime
      , bankProfit  = newBankProfit
      }
    , isEnded        = simulationIsEnded
    , lastUsedNumber = if daysChanges > 0
      then 0
      else lastUsedNumber newExperiment
    }
  where
    newExperiment  = updateClients $ updateClerks ex
    daysChanges    = updatedTime `div` day
    updatedTime    = currentTime (statistic ex) + 1
    newTime        = updatedTime `mod` day
    newSpentDays   = spentDays (statistic ex) + daysChanges
    clerksSalaries
      | currentDay (statistic ex) `elem` fulltimeDays
        || currentDay (statistic ex) `elem` parttimeDays =
          daysChanges * sum (salary <$> (clerks (bank ex)))
      | otherwise = 0
    newBankProfit  = bankProfit (statistic newExperiment) - clerksSalaries
    newDay
      | daysChanges > 0 = addDays daysChanges (currentDay (statistic ex))
      | otherwise       = currentDay (statistic ex)
    simulationIsEnded = newSpentDays >= simulationPeriod (parameters ex)

initClerks :: [Clerk]
initClerks = take (fst defaultClerksNums) defaultClerks

initInfoTable :: [TableLine]
initInfoTable = initTableLine <$> initClerks

initNewRequests :: RequestsQueue
initNewRequests = []

genNewRequests :: StdGen -> Parameters -> RequestsQueue
genNewRequests g params =
  filter requestWillEnd $ zipWith4 mkRequest profits days times durations
  where
    (g1, g')      = split g
    (g2, g3)      = split g'
    profits       = randomRs defaultProfitInterval g1
    durations     = randomRs (serviceMinTime params, serviceMaxTime params) g2
    (days, times) = unzip $ filter (\(d,t) -> isWorkingTime d t) $
      takeWhile (\(d,_) -> d < simulationPeriod params) $ genNewTime g3 params (0,0)
    requestWillEnd (Request _ day_ time_ dur_) = isWorkingTime
      (day_ + (time_ + dur_) `div` day) ((time_ + dur_) `mod` day)

genNewTime :: StdGen -> Parameters -> (Int, Minutes) -> [(Int, Minutes)]
genNewTime g params (prevDay, prevTime) =
  newTime : genNewTime g' params newTime
  where
    (genTime, g') = randomR (comingMinTime params, comingMaxTime params) g
    currDay       = prevDay + (prevTime + genTime) `div` day
    currTime      = (prevTime + genTime) `mod` day
    newTime       = (currDay, currTime)

addClients :: [Request] -> Experiment -> Experiment
addClients [] ex = ex
addClients (req : reqs) ex
  | queueLen < limit = addClients reqs ex
    { bank = (bank ex)
      { queue = queue (bank ex) ++ [client]
      }
    , lastUsedNumber = newClientNumber
    }
  | otherwise = ex
    { statistic = (statistic ex)
      { leftClientsNum = newLeftClientsNum
      }
    }
  where
    queueLen = length (queue (bank ex))
    limit    = queueLenLimit (parameters ex)
    client   = Client
      { request  = req
      , waitTime = 0
      , number   = newClientNumber
      }
    newClientNumber = lastUsedNumber ex + 1
    newLeftClientsNum = leftClientsNum (statistic ex) + length (req : reqs)


data Bank = Bank
  { queue     :: ClientsQueue
  , infoTable :: [TableLine]
  , clerks    :: [Clerk]
  } deriving (Eq)

initBank :: Bank
initBank = Bank
  { queue     = []
  , infoTable = initInfoTable
  , clerks    = initClerks
  }

delClient :: Client -> Bank -> Bank
delClient client bank_ = bank_
  { queue = filter (/= client) (queue bank_)
  }

addClerk :: Bank -> Bank
addClerk bank_
  | amount == snd defaultClerksNums = bank_
  | otherwise                             = bank_
    { clerks    = clerks bank_ ++ [defaultClerks !! amount]
    , infoTable = initTableLine <$> newClerks
    }
  where
    amount    = length (clerks bank_)
    newClerks = clerks bank_ ++ [defaultClerks !! amount]

delClerk :: Bank -> Bank
delClerk bank_
  | amount == fst defaultClerksNums = bank_
  | otherwise                             = bank_
    { clerks    = take (amount - 1) (clerks bank_)
    , infoTable = delTableLine deletedClerk (infoTable bank_)
    }
  where
    amount       = length (clerks bank_)
    deletedClerk = last   (clerks bank_)

servicedClientNumbers :: Bank -> [Int]
servicedClientNumbers bank_ =
  number <$> catMaybes (work <$> busyClerks)
  where
    busyClerks = filter (\clerk_ -> work clerk_ /= Nothing) (clerks bank_)

data Client = Client
  { request      :: Request
  , waitTime     :: Minutes
  , number       :: Int
  } deriving (Eq)

subtractDuration :: Client -> Client
subtractDuration client = client
  { request = (request client)
    { duration = duration (request client) - 1
    }
  }

completeService :: Maybe Client -> Maybe Client
completeService (Just client)
  | duration (request client) == 0 = Nothing
  | otherwise                      = (Just client)
completeService work_ = work_

addWaitTime :: Client -> Client
addWaitTime client = client
  { waitTime = waitTime client + 1
  }

data Request = Request
  { profit       :: Money
  , dayToComing  :: Int
  , timeToComing :: Minutes
  , duration     :: Minutes
  } deriving (Eq)

mkRequest :: Money -> Int -> Minutes -> Minutes -> Request
mkRequest profit_ day_ time_ duration_ = Request
  { profit       = profit_
  , dayToComing  = day_
  , timeToComing = time_
  , duration     = duration_
  }

data TableLine = TableLine
  { tableClerk  :: String
  , tableClient :: Maybe Int
  } deriving (Eq)

delTableLine :: Clerk -> [TableLine] -> [TableLine]
delTableLine clerk table =
  filter (\line -> tableClerk line /= name clerk) table

addTableLine :: Clerk -> [TableLine] -> [TableLine]
addTableLine clerk table = initTableLine clerk : table

initTableLine :: Clerk -> TableLine
initTableLine clerk = TableLine
  { tableClerk  = name clerk
  , tableClient = number <$> work clerk
  }

updateInfoTable :: [Clerk] -> [TableLine] -> [TableLine]
updateInfoTable [] table = table
updateInfoTable (clerk : cs) table =
   addTableLine clerk $ delTableLine clerk $ updateInfoTable cs table

data Clerk = Clerk
  { name   :: String
  , salary :: Money
  , work   :: Maybe Client
  } deriving (Eq)

takeClientToClerk :: Client -> Clerk -> Clerk
takeClientToClerk client clerk = clerk
  { work = Just client
  }

serviceTime :: Clerk -> Clerk
serviceTime clerk = clerk
  { work = subtractDuration <$> work clerk
  }

serviceComplete :: Clerk -> Clerk
serviceComplete clerk = clerk
  { work = completeService (work clerk)
  }

isServiced :: Clerk -> Bool
isServiced clerk = case work clerk of
  Nothing      -> False
  Just client_ -> duration (request client_) == 0

serviceProfit :: Clerk -> Money
serviceProfit clerk = case work clerk of
  Nothing -> 0
  Just client
    | duration (request client) == 0 -> profit (request client)
    | otherwise                      -> 0

withoutWork :: Clerk -> Bool
withoutWork clerk = work clerk == Nothing

data Parameters = Parameters
  { timeStep         :: Minutes
  , simulationPeriod :: Int
  , clerksNum        :: Int
  , queueLenLimit    :: Int
  , serviceMinTime   :: Minutes
  , serviceMaxTime   :: Minutes
  , comingMinTime    :: Minutes
  , comingMaxTime    :: Minutes
  } deriving (Eq)

initParameters :: Parameters
initParameters = Parameters
  { timeStep         = fst defaultTimeSteps
  , simulationPeriod = fst defaultSimulationPeriods
  , clerksNum        = fst defaultClerksNums
  , queueLenLimit    = fst defaultQueueLenLimits
  , serviceMinTime   = fst defaultServiceTimes
  , serviceMaxTime   = snd defaultServiceTimes
  , comingMinTime    = fst defaultComingTimes
  , comingMaxTime    = snd defaultComingTimes
  }

data Statistic = Statistic
  { servicedClientsNum   :: Int
  , leftClientsNum       :: Int
  , bankProfit           :: Money
  , spentDays            :: Int
  , currentDay           :: Day
  , currentTime          :: Minutes
  } deriving (Eq)

initStatistic :: Statistic
initStatistic = Statistic
  { servicedClientsNum   = 0
  , leftClientsNum       = 0
  , bankProfit           = 0
  , spentDays            = 0
  , currentDay           = head fulltimeDays
  , currentTime          = fst fulltimeWorkShedule
  }

type Minutes       = Int
type Money         = Int
type ClientsQueue  = [Client]
type RequestsQueue = [Request]

data Day
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq)

isWorkingTime :: Int -> Minutes -> Bool
isWorkingTime n time =
  (day_ `elem` fulltimeDays) && (inFulltimeShedule time)
  || (day_ `elem` parttimeDays) && (inParttimeShedule time)
  where
    day_ = case n `mod` 7 of
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      _ -> Sunday
    inFulltimeShedule t =
      (t >= fst fulltimeWorkShedule) && (t <= snd fulltimeWorkShedule)
      && ((t <= fst fulltimeDinnerShedule) || (t >= snd fulltimeDinnerShedule))
    inParttimeShedule t =
      (t >= fst parttimeWorkShedule) && (t <= snd parttimeWorkShedule)
      && ((t <= fst parttimeDinnerShedule) || (t >= snd parttimeDinnerShedule))

addDays :: Int -> Day -> Day
addDays 0 day_ = day_
addDays n day_ = addDays (n - 1) nextDay
  where
    nextDay = case day_ of
      Monday    -> Tuesday
      Tuesday   -> Wednesday
      Wednesday -> Thursday
      Thursday  -> Friday
      Friday    -> Saturday
      Saturday  -> Sunday
      Sunday    -> Monday

data Action
  = NoOp
  | StartExperiment
  | PlayPauseExperiment
  | ResetExperiment
  | ChangeParameter ParametersField ChangeAction
  | AddTime Minutes
  | FinishExperiment
  deriving (Eq)

data ParametersField
  = TimeStep
  | SimulationPeriod
  | ClerksNum
  | QueueLenLimit
  | ServiceMinTime
  | ServiceMaxTime
  | ComingMinTime
  | ComingMaxTime
  deriving (Eq)

data ChangeAction = Sub | Add
  deriving (Eq)

defaultTimeSteps :: (Minutes, Minutes)
defaultTimeSteps = (10, 60)

defaultClerksNums :: (Int, Int)
defaultClerksNums = (2, 7)

defaultClerks :: [Clerk]
defaultClerks = mkClerk <$> defaultClerksNames

mkClerk :: String -> Clerk
mkClerk s = Clerk
  { name   = s
  , salary = defaultClerkSalary
  , work   = Nothing
  }

defaultClerkSalary :: Money
defaultClerkSalary = 2

defaultClerksNames :: [String]
defaultClerksNames = (\num -> "Clerk " ++ show num) <$> [1,2..]

defaultQueueLenLimits :: (Int, Int)
defaultQueueLenLimits = (10, 25)

defaultServiceTimes :: (Minutes, Minutes)
defaultServiceTimes = (2, 30)

defaultComingTimes :: (Minutes, Minutes)
defaultComingTimes = (0, 10)

defaultSimulationPeriods :: (Int, Int)
defaultSimulationPeriods = (7, 28)

hour :: Minutes
hour = 60

day :: Minutes
day = 24 * hour

fulltimeDays :: [Day]
fulltimeDays = [Monday, Tuesday, Wednesday, Thursday, Friday]

parttimeDays :: [Day]
parttimeDays = [Saturday]

fulltimeWorkShedule :: (Minutes, Minutes)
fulltimeWorkShedule = (10 * hour, 19 * hour)

fulltimeDinnerShedule :: (Minutes, Minutes)
fulltimeDinnerShedule = (14 * hour, 15 * hour)

parttimeWorkShedule :: (Minutes, Minutes)
parttimeWorkShedule = (10 * hour, 17 * hour)

parttimeDinnerShedule :: (Minutes, Minutes)
parttimeDinnerShedule = (13 * hour, 14 * hour)

defaultProfitInterval :: (Money, Money)
defaultProfitInterval = (3, 50)
