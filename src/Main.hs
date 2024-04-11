module Main where
import qualified Data.Map as Data
import Data.Map (mapWithKey, (!), fromList)
import Control.Exception (Exception)
import qualified Data.List

newtype SimulationError = InvalidTransition String

instance Show SimulationError where
  show (InvalidTransition toState) = "No transition from state" ++ toState

instance Exception SimulationError

newtype Context = Context { agents :: [Agent] } deriving (Show)

data AgentField = Integer Integer | Double Double deriving (Eq, Show)
type Transition = Agent -> Context -> [Agent]

data Agent = Agent
  { classname :: String
  , fields :: Data.Map String AgentField
  , state :: String
  , states :: Data.Map String Transition
  }

update :: Transition
update a = (states a ! state a) a

stepSimulation :: Context -> Context
stepSimulation ctx = Context { agents = concatMap (`update` ctx) $ agents ctx }

instance Show Agent where
  show a = Data.List.intercalate ", " $ map showField [("Class", classname), ("Fields", joinFields), ("State", state)]
    where
      showField :: Show s => (String, Agent -> s) -> String
      showField (name, field) = name ++ ": " ++ show (field a)
      joinFields :: Agent -> String
      joinFields a' = Data.List.intercalate ", " $ map (\(k, v) -> show k ++ "': " ++ show v) $ Data.assocs $ fields a'


testAgent :: Agent
testAgent = Agent {classname= "TestAgent" , fields=fromList [("a", Integer 1)], state="S", states=fromList [("S", incrementA)]}

incrementA :: Agent -> Context -> [Agent]
incrementA a _ = [a {
  fields = mapWithKey (\k v -> if k == "a" then
    case v of
      Integer x -> Integer $ x + 1
      Double x -> Double $ x + 1
  else v
  ) $ fields a
}]

main :: IO ()
main = do
  mapM_ print (take 10 $ map agents $ iterate stepSimulation $ Context [testAgent])
