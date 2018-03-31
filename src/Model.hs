module Model where

import Data.List (find, zipWith4)
import Data.Maybe (catMaybes)
import System.Random

import Constants

data Experiment = Experiment
  { bank           :: Bank
  , newRequests    :: [Request]
  , parameters     :: Parameters
  , statistic      :: Statistic
  , isInitialized  :: Bool
  , isPaused       :: Bool
  , isEnded        :: Bool
  , lastUsedNumber :: Int
  } deriving (Eq)

class ExperimentClass experiment where
  initExperiment      :: experiment
  changeParameter     :: ParametersField -> ChangeAction -> experiment -> experiment
  startExperiment     :: StdGen -> experiment -> experiment
  playPauseExperiment :: experiment -> experiment
  resetExperiment     :: experiment
  finishExperiment    :: experiment -> experiment
  addToQueue          :: [Request] -> experiment -> experiment
  leftFromQueue       :: experiment -> experiment
  setWorkToClerks     :: experiment -> experiment
  updateClients       :: experiment -> experiment
  updateClerks        :: experiment -> experiment
  addClients          :: [Request] -> experiment -> experiment
  addTime             :: Minutes -> experiment -> experiment

instance ExperimentClass Experiment where

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

  startExperiment g ex = ex
    { newRequests   = genNewRequests g (parameters ex)
    , isInitialized = True
    }

  playPauseExperiment ex = ex
    { isPaused  = not (isPaused ex)
    }

  resetExperiment = initExperiment

  finishExperiment ex = addTime (snd defaultTimeSteps) ex

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
      isLeft (Client (Request _ _ _ dur_)  _) = not $ isWorkingTime
        (spentDays (statistic ex)) (dur_ + currentTime (statistic ex))

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

  updateClients ex = newExperiment
    { newRequests = drop amount (newRequests ex)
    , bank        = bank newExperiment
    }
    where
      newExperiment = addToQueue newClients $ leftFromQueue $ setWorkToClerks ex
      newClients    = takeWhile isComing (newRequests ex)
      amount        = length newClients
      isComing (Request _ day_ time_ _) =
        day_ <= spentDays (statistic ex) && time_ <= currentTime (statistic ex)

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
        , number   = newClientNumber
        }
      newClientNumber = lastUsedNumber ex + 1
      newLeftClientsNum = leftClientsNum (statistic ex) + length (req : reqs)

data Bank = Bank
  { queue     :: [Client]
  , infoTable :: [TableLine]
  , clerks    :: [Clerk]
  } deriving (Eq)

class BankClass bank_ where
  initBank              :: bank_
  delClient             :: Client -> bank_ -> bank_
  addClerk              :: bank_ -> bank_
  delClerk              :: bank_ -> bank_
  servicedClientNumbers :: bank_ -> [Int]  

instance BankClass Bank where

  initBank = Bank
    { queue     = []
    , infoTable = initInfoTable
    , clerks    = initClerks
    }

  delClient client bank_ = bank_
    { queue = filter (/= client) (queue bank_)
    }

  addClerk bank_
    | amount == snd defaultClerksNums = bank_
    | otherwise                             = bank_
      { clerks    = clerks bank_ ++ [defaultClerks !! amount]
      , infoTable = initTableLine <$> newClerks
      }
    where
      amount    = length (clerks bank_)
      newClerks = clerks bank_ ++ [defaultClerks !! amount]

  delClerk bank_
    | amount == fst defaultClerksNums = bank_
    | otherwise                             = bank_
      { clerks    = take (amount - 1) (clerks bank_)
      , infoTable = delTableLine deletedClerk (infoTable bank_)
      }
    where
      amount       = length (clerks bank_)
      deletedClerk = last   (clerks bank_)

  servicedClientNumbers bank_ =
    number <$> catMaybes (work <$> busyClerks)
    where
      busyClerks = filter (\clerk_ -> work clerk_ /= Nothing) (clerks bank_)

data Client = Client
  { request      :: Request
  , number       :: Int
  } deriving (Eq)

class ClientClass client where
  subtractDuration :: client -> client
  completeService  :: Maybe client -> Maybe client

instance ClientClass Client where

  subtractDuration client = client
    { request = (request client)
      { duration = duration (request client) - 1
      }
    }

  completeService (Just client)
    | duration (request client) == 0 = Nothing
    | otherwise                      = (Just client)
  completeService work_ = work_

data Request = Request
  { profit       :: Money
  , dayToComing  :: Int
  , timeToComing :: Minutes
  , duration     :: Minutes
  } deriving (Eq)

class RequestClass request_ where
  initNewRequests :: [request_]  
  genNewRequests  :: StdGen -> Parameters -> [request_]
  mkRequest       :: Money -> Int -> Minutes -> Minutes -> request_

instance RequestClass Request where

  initNewRequests = []

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

class TableLineClass tableLine where
  initInfoTable   :: [tableLine]
  delTableLine    :: Clerk -> [tableLine] -> [tableLine]
  addTableLine    :: Clerk -> [tableLine] -> [tableLine]
  initTableLine   :: Clerk -> tableLine
  updateInfoTable :: [Clerk] -> [tableLine] -> [tableLine]

instance TableLineClass TableLine where

  initInfoTable = initTableLine <$> initClerks

  delTableLine clerk table =
    filter (\line -> tableClerk line /= name clerk) table

  addTableLine clerk table = initTableLine clerk : table

  initTableLine clerk = TableLine
    { tableClerk  = name clerk
    , tableClient = number <$> work clerk
    }

  updateInfoTable [] table = table
  updateInfoTable (clerk : cs) table =
     addTableLine clerk $ delTableLine clerk $ updateInfoTable cs table

data Clerk = Clerk
  { name   :: String
  , salary :: Money
  , work   :: Maybe Client
  } deriving (Eq)

class ClerkClass clerk where
  initClerks        :: [clerk]
  takeClientToClerk :: Client -> clerk -> clerk
  serviceTime       :: clerk -> clerk
  serviceComplete   :: clerk -> clerk
  isServiced        :: clerk -> Bool
  serviceProfit     :: clerk -> Money
  withoutWork       :: clerk -> Bool
  defaultClerks     :: [clerk]
  mkClerk           :: String -> clerk

instance ClerkClass Clerk where

  initClerks = take (fst defaultClerksNums) defaultClerks

  takeClientToClerk client clerk = clerk
    { work = Just client
    }

  serviceTime clerk = clerk
    { work = subtractDuration <$> work clerk
    }

  serviceComplete clerk = clerk
    { work = completeService (work clerk)
    }

  isServiced clerk = case work clerk of
    Nothing      -> False
    Just client_ -> duration (request client_) == 0

  serviceProfit clerk = case work clerk of
    Nothing -> 0
    Just client
      | duration (request client) == 0 -> profit (request client)
      | otherwise                      -> 0

  withoutWork clerk = work clerk == Nothing

  defaultClerks = mkClerk <$> defaultClerksNames

  mkClerk s = Clerk
    { name   = s
    , salary = defaultClerkSalary
    , work   = Nothing
    }

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

class ParametersClass parameters_ where
  initParameters :: parameters_

instance ParametersClass Parameters where

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

class StatisticClass statistic_ where
  initStatistic :: statistic_

instance StatisticClass Statistic where

  initStatistic = Statistic
    { servicedClientsNum   = 0
    , leftClientsNum       = 0
    , bankProfit           = 0
    , spentDays            = 0
    , currentDay           = head fulltimeDays
    , currentTime          = fst fulltimeWorkShedule
    }

genNewTime :: StdGen -> Parameters -> (Int, Minutes) -> [(Int, Minutes)]
genNewTime g params (prevDay, prevTime) =
  newTime : genNewTime g' params newTime
  where
    (genTime, g') = randomR (comingMinTime params, comingMaxTime params) g
    currDay       = prevDay + (prevTime + genTime) `div` day
    currTime      = (prevTime + genTime) `mod` day
    newTime       = (currDay, currTime)

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
