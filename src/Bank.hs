{-# LANGUAGE OverloadedStrings #-}
module Bank where

import           Miso
import           Miso.String (MisoString, ms, (<>))
import qualified Data.Map as Map (fromList)
import           Data.List.Extra (takeEnd)
import           Control.Monad (forever)
import           Control.Concurrent
import           System.Random

import           Model

run :: IO ()
run = do
  g <- newStdGen
  startApp App
    { initialAction = NoOp
    , model         = initExperiment
    , update        = updateExperiment g
    , view          = viewExperiment
    , events        = defaultEvents
    , subs          = [ timeSub ]
    , mountPoint    = Nothing
    }

timeSub :: Sub Action Experiment
timeSub experiment sink = do
  _ <- forkIO $ timeUpdate experiment sink
  return ()

timeUpdate :: Sub Action Experiment
timeUpdate experiment sink = forever $ do
  threadDelay 1000000
  ex <- experiment
  if isInitialized ex && not (isPaused ex) && not (isEnded ex)
    then sink (AddTime 1) >> return ()
    else return ()

updateExperiment :: StdGen -> Action -> Experiment -> Effect Action Experiment
updateExperiment _ NoOp ex = noEff ex
updateExperiment _ (ChangeParameter field action) ex =
  noEff (changeParameter field action ex)
updateExperiment g StartExperiment     ex = noEff (startExperiment g ex)
updateExperiment _ PlayPauseExperiment ex = noEff (playPauseExperiment ex)
updateExperiment _ ResetExperiment     _  = noEff resetExperiment
updateExperiment _ FinishExperiment    ex
  | isEnded ex || not (isInitialized ex) = noEff ex
  | otherwise                            = (finishExperiment ex) <# do
    threadDelay 10000
    return FinishExperiment
updateExperiment _ (AddTime m)         ex = noEff (addTime m ex)

showMS :: Show a => a -> MisoString
showMS = ms . show

showMSTime :: Minutes -> MisoString
showMSTime m = ms $ hours ++ " : " ++ minutes
  where
    hours   = takeEnd 2 $ '0' : show (m `div` hour)
    minutes = takeEnd 2 $ '0' : show (m `mod` hour)

showMSDay :: Day -> MisoString
showMSDay d = case d of
  Monday    -> "Пн."
  Tuesday   -> "Вт."
  Wednesday -> "Ср."
  Thursday  -> "Чт."
  Friday    -> "Пт."
  Saturday  -> "Сб."
  Sunday    -> "Вс."

showMSMoney :: Money -> MisoString
showMSMoney m = showMS m <> " тыс. руб."

showMSInterval :: Show a => (a,a) -> MisoString
showMSInterval (x,y) = showMS x <> " - " <> showMS y

viewExperiment :: Experiment -> View Action
viewExperiment ex =
  div_
  [ lineBlockStyle ]
  [ div_
    [ dataStyle ]
    [ parametersView
    , statisticView
    ]
  , bankView
  ]
  where

    bankView =
      div_
      [ activeStyle
      , bankStyle ]
      [ clerksView
      , div_
        [ lineBlockStyle ]
        [ queueView
        ]
      , div_
        [ lineBlockStyle ]
        [ sheduleView
        , tableView
        ]
      ]

    clerksView =
      div_
      [ clerksBlockStyle
      , lineBlockStyle ]
      (clerkToDiv_ <$> clerks (bank ex))

    queueView =
      div_
      [ queueStyle
      , centerStyle ]
      (clientToDiv_ <$> queue (bank ex))

    sheduleView =
      div_
      [ centerStyle
      , sheduleStyle ]
      ((fulltimeDayToDiv_ <$> fulltimeDays)
      ++ (parttimeDayToDiv_ <$> parttimeDays))

    tableView =
      div_
      [ centerStyle
      , tableStyle ]
      (tableLineToDiv_ <$> infoTable (bank ex))

    parametersView =
      div_
      []
      [ clerkParameterView
      , timeStepParameterView
      , simulationPeriodParameterView
      , maxQueueLenParameterView
      , serviceTimeParameterView
      , comingTimeParameterView
      , parametersButtonsView
      ]

    statisticView =
      div_
      [ statisticStyle ]
      [ currentTimeView
      , spentDaysView
      , currentDayView
      , servicedClientsNumView
      , leftClientsNumView
      , bankProfitView
      ]

    currentTimeView =
      div_
      [ lineBlockStyle
      , textLineStyle ]
      [ div_
        [ labelStyle ]
        [ text "Текущее время: " ]
      , div_
        []
        [ text $ showMSTime (currentTime $ statistic ex) ]
      ]

    spentDaysView =
      div_
      [ lineBlockStyle
      , textLineStyle ]
      [ div_
        [ labelStyle ]
        [ text "Дней прошло: " ]
      , div_
        []
        [ text $ showMS (spentDays $ statistic ex) ]
      ]

    currentDayView =
      div_
      [ lineBlockStyle
      , textLineStyle ]
      [ div_
        [ labelStyle ]
        [ text "Текущий день недели: " ]
      , div_
        []
        [ text $ showMSDay (currentDay $ statistic ex) ]
      ]

    servicedClientsNumView =
      div_
      [ lineBlockStyle
      , textLineStyle ]
      [ div_
        [ labelStyle ]
        [ text "Число обслужанных клиентов: " ]
      , div_
        []
        [ text $ showMS (servicedClientsNum $ statistic ex) ]
      ]

    leftClientsNumView =
      div_
      [ lineBlockStyle
      , textLineStyle ]
      [ div_
        [ labelStyle ]
        [ text "Число потерянных клиентов: " ]
      , div_
        []
        [ text $ showMS (leftClientsNum $ statistic ex) ]
      ]

    bankProfitView =
      div_
      [ lineBlockStyle
      , textLineStyle ]
      [ div_
        [ labelStyle ]
        [ text "Прибыль банка: " ]
      , div_
        []
        [ text $ showMSMoney (bankProfit $ statistic ex) ]
      ]

    timeStepParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Шаг моделирования (" <> showMSInterval defaultTimeSteps <> "):" ]
      , div_
        [ lineBlockStyle
        , settingStyle ]
        [ button_
          [ timeStepParameterSubStyle
          , subButtonStatus
          , onClick $ ChangeParameter TimeStep Sub ]
          [ "-" ]
        , div_
          [ parameterValView ]
          [ text $ (showMS $ timeStep (parameters ex)) <> " мин." ]
        , button_
          [ timeStepParameterAddStyle
          , addButtonStatus
          , onClick $ ChangeParameter TimeStep Add ]
          [ "+" ]
        ]
      ]

    simulationPeriodParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Период моделирования (" <> showMSInterval defaultSimulationPeriods <> "):" ]
      , div_
        [ lineBlockStyle
        , settingStyle ]
        [ button_
          [ simulationPeriodParameterSubStyle
          , subButtonStatus
          , onClick $ ChangeParameter SimulationPeriod Sub ]
          [ "-" ]
        , div_
          [ parameterValView ]
          [ text $ (showMS $ simulationPeriod (parameters ex)) <> " дней" ]
        , button_
          [ simulationPeriodParameterAddStyle
          , addButtonStatus
          , onClick $ ChangeParameter SimulationPeriod Add ]
          [ "+" ]
        ]
      ]

    maxQueueLenParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Максимальный размер очереди (" <> showMSInterval defaultQueueLenLimits <> "):" ]
      , div_
        [ lineBlockStyle
        , settingStyle ]
        [ button_
          [ maxQueueLenParameterSubStyle
          , subButtonStatus
          , onClick $ ChangeParameter QueueLenLimit Sub ]
          [ "-" ]
        , div_
          [ parameterValView ]
          [ text $ showMS $ queueLenLimit (parameters ex) ]
        , button_
          [ maxQueueLenParameterAddStyle
          , addButtonStatus
          , onClick $ ChangeParameter QueueLenLimit Add ]
          [ "+" ]
        ]
      ]

    serviceTimeParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Время обслуживания (" <> showMSInterval defaultServiceTimes <> "):" ]
      , div_
        [ settingStyle ]
        [ div_
          [ lineBlockStyle ]
          [ div_
            [ labelStyle ]
            [ text "От:" ]
          , button_
            [ serviceMinTimeParameterSubStyle
            , subButtonStatus
            , onClick $ ChangeParameter ServiceMinTime Sub ]
            [ "-" ]
          , div_
            [ parameterValView ]
            [ text $ (showMS $ serviceMinTime (parameters ex)) <> " мин." ]
          , button_
            [ serviceMinTimeParameterAddStyle
            , addButtonStatus
            , onClick $ ChangeParameter ServiceMinTime Add ]
            [ "+" ]
          ]
        , div_
          [ lineBlockStyle ]
          [ div_
            [ labelStyle ]
            [ text "До:" ]
          , button_
            [ serviceMaxTimeParameterSubStyle
            , subButtonStatus
            , onClick $ ChangeParameter ServiceMaxTime Sub ]
            [ "-" ]
          , div_
            [ parameterValView ]
            [ text $ (showMS $ serviceMaxTime (parameters ex)) <> " мин." ]
          , button_
            [ serviceMaxTimeParameterAddStyle
            , addButtonStatus
            , onClick $ ChangeParameter ServiceMaxTime Add ]
            [ "+" ]
          ]
        ]
      ]

    comingTimeParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Время между приходом клиентов (" <> showMSInterval defaultComingTimes <> "):" ]
      , div_
        [ settingStyle ]
        [ div_
          [ lineBlockStyle ]
          [ div_
            [ labelStyle ]
            [ text "От:" ]
          , button_
            [ comingMinTimeParameterSubStyle
            , subButtonStatus
            , onClick $ ChangeParameter ComingMinTime Sub ]
            [ "-" ]
          , div_
            [ parameterValView ]
            [ text $ (showMS $ comingMinTime (parameters ex)) <> " мин." ]
          , button_
            [ comingMinTimeParameterAddStyle
            , addButtonStatus
            , onClick $ ChangeParameter ComingMinTime Add ]
            [ "+" ]
          ]
        , div_
          [ lineBlockStyle ]
          [ div_
            [ labelStyle ]
            [ text "До:" ]
          , button_
            [ comingMaxTimeParameterSubStyle
            , subButtonStatus
            , onClick $ ChangeParameter ComingMaxTime Sub ]
            [ "-" ]
          , div_
            [ parameterValView ]
            [ text $ (showMS $ comingMaxTime (parameters ex)) <> " мин." ]
          , button_
            [ comingMaxTimeParameterAddStyle
            , addButtonStatus
            , onClick $ ChangeParameter ComingMaxTime Add ]
            [ "+" ]
          ]
        ]
      ]

    clerkParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Число клерков (" <> showMSInterval defaultClerksNums <> "):" ]
      , div_
        [ lineBlockStyle
        , settingStyle ]
        [ button_
          [ clerkParameterSubStyle
          , subButtonStatus
          , onClick $ ChangeParameter ClerksNum Sub ]
          [ "-" ]
        , div_
          [ parameterValView ]
          [ text $ showMS $ clerksNum (parameters ex) ]
        , button_
          [ clerkParameterAddStyle
          , addButtonStatus
          , onClick $ ChangeParameter ClerksNum Add ]
          [ "+" ]
        ]
      ]

    parametersButtonsView =
      div_
      [ blockStyle ]
      [ div_
        [ lineBlockStyle
        , buttonsBlockStyle ]
        [ startButton
        , playPauseButton
        , resetButton
        ]
      , div_
        [ lineBlockStyle
        , buttonsBlockStyle ]
        [ nextStepButton
        , finishButton
        ]
      ]

    startButton =
      button_
      [ startButtonStatus
      , buttonStyle
      , onClick StartExperiment ]
      [ "Инициализировать" ]

    playPauseButton =
      button_
      [ playPauseButtonStatus
      , buttonStyle
      , onClick PlayPauseExperiment ]
      [ playPauseLabel ]

    playPauseLabel
      | isPaused ex = "Продолжить"
      | otherwise   = "Приостановить"

    resetButton =
      button_
      [ buttonStyle
      , onClick ResetExperiment ]
      [ "Сброс" ]

    nextStepButton =
      button_
      [ nextStepButtonStatus
      , buttonStyle
      , onClick (AddTime (timeStep (parameters ex))) ]
      [ "Сделать шаг" ]

    finishButton =
      button_
      [ finishButtonStatus
      , buttonStyle
      , onClick FinishExperiment ]
      [ "Завершить эксперимент" ]

-- =================================================================
-- |                        Elements styles                        |
-- =================================================================

    centerStyle = style_ $ Map.fromList
      [ ("margin-left",  "auto")
      , ("margin-right", "auto") ]

    dataStyle = style_ $ Map.fromList
      [ ("width",       "350px")
      , ("margin-left", "20px")
      , ("margin-top",  "20px") ]

    textLineStyle = style_ $ Map.fromList
      [ ("margin-bottom", "5px") ]

    settingStyle = style_ $ Map.fromList
      [ ("margin-left", "20px") ]

    clerksBlockStyle = style_ $ Map.fromList
      [ ("margin-bottom",   "50px")
      , ("justify-content", "space-around") ]

    statisticStyle = style_ $ Map.fromList
      [ ("display",         "flex")
      , ("border-style",    "solid")
      , ("border-color",    "gray")
      , ("border-radius",   "2px") 
      , ("border-width",    "2px")
      , ("flex-direction",  "column")
      , ("justify-content", "space-around")
      , ("padding",         "20px")
      , ("width",           "300px") ]

    queueStyle = style_ $ Map.fromList
      [ ("display",          "flex")
      , ("background-color", "linen")
      , ("height",           "200px")
      , ("border-style",     "solid")
      , ("border-color",     "gray")
      , ("border-radius",    "2px") 
      , ("border-width",     "2px")
      , ("flex-wrap",        "wrap")
      , ("align-items",      "center")
      , ("width",            "700px") ]

    sheduleStyle = style_ $ Map.fromList
      [ ("display",          "flex")
      , ("padding-top",      "10px")
      , ("background-color", "lightcyan")
      , ("border-style",     "solid")
      , ("border-color",     "gray")
      , ("border-radius",    "2px") 
      , ("border-width",     "2px")
      , ("margin-top",       "20px")
      , ("height",           "200px")
      , ("width",            "335px")
      , ("flex-direction",   "column")
      , ("justify-content",  "space-around") ]

    tableStyle = style_ $ Map.fromList
      [ ("display",          "flex")
      , ("background-color", "lightgoldenrodyellow")
      , ("padding-top",      "10px")
      , ("border-style",     "solid")
      , ("border-color",     "gray")
      , ("border-radius",    "2px") 
      , ("border-width",     "2px")
      , ("margin-top",       "20px")
      , ("height",           "200px")
      , ("width",            "170px")
      , ("flex-direction",   "column")
      , ("justify-content",  "space-around") ]

    buttonStyle = style_ $ Map.fromList
      [ ("margin-right", "10px") ]

    activeStyle
      | isInitialized ex =
        style_ $ Map.fromList [ ("opacity", "1") ]
      | otherwise = style_ $ Map.fromList [ ("opacity", "0.3") ]

    bankStyle = style_ $ Map.fromList
      [ ("width",      "900px")
      , ("margin-top", "20px") ]

    startButtonStatus
      | isInitialized ex = disabled_ True
      | otherwise        = disabled_ False

    playPauseButtonStatus
      | isEnded ex || not (isInitialized ex) = disabled_ True
      | otherwise                            = disabled_ False

    nextStepButtonStatus
      | isInitialized ex && isPaused ex && not (isEnded ex) = disabled_ False
      | otherwise                                           = disabled_ True

    finishButtonStatus
      | isInitialized ex && isPaused ex && not (isEnded ex) = disabled_ False
      | otherwise                                           = disabled_ True

    blockStyle = style_ $ Map.fromList
      [ ("margin-bottom", "20px") ]

    buttonsBlockStyle = style_ $ Map.fromList
      [ ("margin-bottom",   "10px")
      , ("justify-content", "space-around") ]

    parameterValView = style_ $ Map.fromList
      [ ("margin-top", "2px") ]

    subButtonStatus = style_ $ Map.fromList
      [ ("margin-right", "10px") ]

    addButtonStatus = style_ $ Map.fromList
      [ ("margin-left", "10px") ]

    labelStyle = style_ $ Map.fromList
      [ ("margin-right", "10px") ]

    clerkParameterSubStyle
      | clerksNum (parameters ex) == fst defaultClerksNums
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    clerkParameterAddStyle
      | clerksNum (parameters ex) == snd defaultClerksNums
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    timeStepParameterSubStyle
      | timeStep (parameters ex) == fst defaultTimeSteps = disabled_ True
      | otherwise                                        = disabled_ False

    timeStepParameterAddStyle
      | timeStep (parameters ex) == snd defaultTimeSteps = disabled_ True
      | otherwise                                        = disabled_ False

    simulationPeriodParameterSubStyle
      | simulationPeriod (parameters ex) == fst defaultSimulationPeriods
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    simulationPeriodParameterAddStyle
      | simulationPeriod (parameters ex) == snd defaultSimulationPeriods
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    maxQueueLenParameterSubStyle
      | queueLenLimit (parameters ex) == fst defaultQueueLenLimits
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    maxQueueLenParameterAddStyle
      | queueLenLimit (parameters ex) == snd defaultQueueLenLimits
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    serviceMinTimeParameterSubStyle
      | serviceMinTime (parameters ex) == fst defaultServiceTimes
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    serviceMinTimeParameterAddStyle
      | serviceMinTime (parameters ex) == snd defaultServiceTimes
        || serviceMinTime (parameters ex) >= serviceMaxTime (parameters ex)
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    serviceMaxTimeParameterSubStyle
      | serviceMaxTime (parameters ex) == fst defaultServiceTimes
        || serviceMaxTime (parameters ex) <= serviceMinTime (parameters ex)
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    serviceMaxTimeParameterAddStyle
      | serviceMaxTime (parameters ex) == snd defaultServiceTimes
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    comingMinTimeParameterSubStyle
      | comingMinTime (parameters ex) == fst defaultComingTimes
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    comingMinTimeParameterAddStyle
      | comingMinTime (parameters ex) == snd defaultComingTimes
        || comingMinTime (parameters ex) >= comingMaxTime (parameters ex)
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    comingMaxTimeParameterSubStyle
      | comingMaxTime (parameters ex) == fst defaultComingTimes
        || comingMaxTime (parameters ex) <= comingMinTime (parameters ex)
        || comingMaxTime (parameters ex) <= 2
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    comingMaxTimeParameterAddStyle
      | comingMaxTime (parameters ex) == snd defaultComingTimes
        || isInitialized ex = disabled_ True
      | otherwise           = disabled_ False

    lineBlockStyle = style_ $ Map.fromList [ ("display", "flex") ]

clerkToDiv_ :: Clerk -> View Action
clerkToDiv_ clerk =
  div_
  []
  [ div_
    [ nameStyle ]
    [ text $ ms $ name clerk ]
  , div_
    [ clerkBodyStyle
    , clerkBodyColor ]
    []
  , clientPlaceView
  ]
  where

    clientPlaceView =
      div_
      [ clientPlaceStyle ]
      [ div_
        [ numberStyle ]
        [ text servicedClientLabel ]
      , div_
        [ servicedClientBodyStyle
        , servicedClientBodyVisibility ]
        []
      ]

    servicedClientLabel = case work clerk of
      Nothing -> "___"
      Just cl -> ms $ takeEnd 3 $ "00" ++ show (number cl)

    servicedClientBodyStyle = style_ $ Map.fromList
      [ ("width",            "30px")
      , ("height",           "30px")
      , ("background-color", "lightblue") ]

    servicedClientBodyVisibility
      | work clerk == Nothing =
        style_ $ Map.fromList [ ("visibility", "hidden") ]
      | otherwise = style_ $ Map.fromList [ ("visibility", "visible") ]

    nameStyle = style_ $ Map.fromList
      [ ("margin-bottom", "5px")
      , ("margin-left",   "3px") ]

    numberStyle = style_ $ Map.fromList
      [ ("margin-bottom", "5px")
      , ("text-align",    "center") ]

    clerkBodyStyle = style_ $ Map.fromList
      [ ("width",         "30px")
      , ("height",        "30px")
      , ("margin-left",   "12px")
      , ("margin-top",    "10px")
      , ("margin-bottom", "20px") ]

    clerkBodyColor
      | work clerk == Nothing = style_ $ Map.fromList
        [ ("background-color", "darkseagreen ") ]
      | otherwise             = style_ $ Map.fromList
        [ ("background-color", "lightcoral") ]

    clientPlaceStyle = style_ $ Map.fromList
      [ ("border-style",  "solid")
      , ("border-color",  "gray")
      , ("border-radius", "2px") 
      , ("border-width",  "2px")
      , ("padding",       "10px") ]

clientToDiv_ :: Client -> View Action
clientToDiv_ client =
  div_
  []
  [ div_
    [ numberStyle ]
    [ text clientLabel ]
  , div_
    [ clientBodyStyle ]
    []
  ]
  where

    numberStyle = style_ $ Map.fromList
      [ ("margin-bottom", "5px")
      , ("text-align",    "center") ]

    clientBodyStyle = style_ $ Map.fromList
      [ ("width",            "30px")
      , ("height",           "30px")
      , ("margin-left",      "20px")
      , ("margin-right",     "20px")
      , ("margin-bottom",    "10px")
      , ("background-color", "lightblue") ]

    clientLabel = ms $ takeEnd 3 $ "00" ++ show (number client)

fulltimeDayToDiv_ :: Day -> View Action
fulltimeDayToDiv_ day_ =
  div_
  [ sheduleLineStyle ]
  [ text $ showMSDay day_ <> ": "
  <> showMSTime (fst fulltimeWorkShedule) <> " - "
  <> showMSTime (snd fulltimeWorkShedule)
  <> " (обед: "
  <> showMSTime (fst fulltimeDinnerShedule) <> " - "
  <> showMSTime (snd fulltimeDinnerShedule) <> ")" ]
  where

    sheduleLineStyle = style_ $ Map.fromList
      [ ("display",       "flex")
      , ("margin-left",   "20px")
      , ("margin-bottom", "10px")
      , ("font-weight",   "bold") ]

parttimeDayToDiv_ :: Day -> View Action
parttimeDayToDiv_ day_ =
  div_
  [ sheduleLineStyle ]
  [ text $ showMSDay day_ <> ": "
  <> showMSTime (fst parttimeWorkShedule) <> " - "
  <> showMSTime (snd parttimeWorkShedule)
  <> " (обед: "
  <> showMSTime (fst parttimeDinnerShedule) <> " - "
  <> showMSTime (snd parttimeDinnerShedule) <> ")" ]
  where

    sheduleLineStyle = style_ $ Map.fromList
      [ ("display",       "flex")
      , ("margin-left",   "20px")
      , ("margin-bottom", "10px")
      , ("font-weight",   "bold") ]

tableLineToDiv_ :: TableLine -> View Action
tableLineToDiv_ tl =
  div_
  [ tableLineStyle
  , tableLineStatusColor ]
  [ div_
    [ tableClerkLabel ]
    [ text $ ms $ tableClerk tl ++ ":" ]
  , div_
    []
    [ text tableRequest ]
  ]
  where

    tableRequest = case tableClient tl of
      Nothing  -> "свободен"
      Just num -> ms $ takeEnd 3 $ "00" ++ show num

    tableLineStyle = style_ $ Map.fromList
      [ ("display",       "flex")
      , ("margin-left",   "20px")
      , ("margin-bottom", "10px")
      , ("font-weight",   "bold") ]

    tableLineStatusColor = case tableClient tl of
      Nothing -> style_ $ Map.fromList [ ("color", "darkseagreen") ]
      Just _  -> style_ $ Map.fromList [ ("color", "lightcoral") ]

    tableClerkLabel = style_ $ Map.fromList
      [ ("margin-right", "10px") ]
