module Constants where

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

data Day
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq)

type Minutes       = Int
type Money         = Int

defaultTimeSteps :: (Minutes, Minutes)
defaultTimeSteps = (10, 60)

defaultClerksNums :: (Int, Int)
defaultClerksNums = (2, 7)

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
