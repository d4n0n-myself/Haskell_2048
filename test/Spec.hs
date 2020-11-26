import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Prelude hiding (Left, Right)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "checkIfGameWon tests" checkIfGameWonTests
  , testGroup "generateRandomTileTests tests" generateRandomTileTests
  , testGroup "updateGrid tests" updateGridTests
  , testGroup "performMove tests" performMoveTests
  , testGroup "getEmptyCells tests" getEmptyCellsTests
  , testGroup "checkIfGameLost tests" checkIfGameLostTests
  ]

checkIfGameWonTests :: [TestTree]
checkIfGameWonTests =
  [ testCase "some start state is not a win" $
    checkIfGameWon ([[2,0,0,0],[0,0,0,0],[2,0,0,0],[0,0,0,0]]) @?= False
  , testCase "2048 is a win" $
    checkIfGameWon ([[2,2,2,2],[2,2,2,2],[2,2,2,2],[2,2,2,2048]]) @?= True
  ]


generateRandomTileTests :: [TestTree]
generateRandomTileTests =
  [ testCase "tile was generated" $ do
      let initVals = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
      res <- generateRandomTile initVals
      assertBool "generate failed" (res /= initVals)
  ]

updateGridTests :: [TestTree]
updateGridTests =
  [ testCase "simple update" $ do
      let vals = [[2,2,2,2],[2,2,2,2],[2,2,2,2],[2,2,2,2]]
      let newGrid = updateGrid vals (0,0) 4
      let errorMsg = "init value = " ++ (show $ vals!!0!!0) ++ ", new value = " ++ (show $ newGrid!!0!!0)
      assertBool errorMsg ((vals!!0!!0) /= newGrid!!0!!0)
  ]

performMoveTests :: [TestTree]
performMoveTests =
  [ testCase "2 2 0 0 + left = 4 0 0 0" $ do
        let vals = [[2,2,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
        let res = performMove Left vals
        assertBool "" ((res!!0!!0) == 4)
    , testCase "2 0 2 0 + left = 4 0 0 0" $ do
        let vals = [[2,0,2,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
        let res = performMove Left vals
        assertBool "" ((res!!0!!0) == 4)
    , testCase "2 0 0 2 + left = 4 0 0 0" $ do
        let vals = [[2,0,0,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
        let res = performMove Left vals
        assertBool "" ((res!!0!!0) == 4)
    , testCase "0 2 0 2 + left = 4 0 0 0" $ do
        let vals = [[0,2,0,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
        let res = performMove Left vals
        assertBool "" ((res!!0!!0) == 4)
    , testCase "2 2 2 2 + left = 4 4 0 0" $ do
        let vals = [[2,2,2,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
        let res = performMove Left vals
        assertBool "" ((res!!0!!0) == 4)
        assertBool "" ((res!!0!!1) == 4)
    , testCase "4 2 2 2 + left = 4 4 2 0" $ do
        let vals = [[4,2,2,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
        let res = performMove Left vals
        assertBool "" ((res!!0!!0) == 4)
        assertBool "" ((res!!0!!1) == 4)
        assertBool "" ((res!!0!!2) == 2)
    , testCase "0 2 4 2 + left = 2 4 2 0" $ do
        let vals = [[0,2,4,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
        let res = performMove Left vals
        assertBool "first" ((res!!0!!0) == 2)
        assertBool "second" ((res!!0!!1) == 4)
        assertBool "third" ((res!!0!!2) == 2)
        assertBool "last" ((res!!0!!3) == 0)
    , testCase "2 4 4 2 + right = 0 2 8 2" $ do
        let vals = [[2,4,4,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
        let res = performMove Right vals
        assertBool "first" ((res!!0!!0) == 0)
        assertBool "second" ((res!!0!!1) == 2)
        assertBool "third" ((res!!0!!2) == 8)
        assertBool "last" ((res!!0!!3) == 2)
    , testCase "8 8 16 16 + up = 16 32 0 0" $ do
        let vals = [[8,0,0,0],[8,0,0,0],[16,0,0,0],[16,0,0,0]]
        let res = performMove Up vals
        assertBool "first" ((res!!0!!0) == 16)
        assertBool "second" ((res!!1!!0) == 32)
        assertBool "third" ((res!!2!!0) == 0)
        assertBool "last" ((res!!3!!0) == 0)
    , testCase "8 8 16 16 + down = 0 0 16 32" $ do
        let vals = [[8,0,0,0],[8,0,0,0],[16,0,0,0],[16,0,0,0]]
        let res = performMove Down vals
        assertBool "first" ((res!!0!!0) == 0)
        assertBool "second" ((res!!1!!0) == 0)
        assertBool "third" ((res!!2!!0) == 16)
        assertBool "last" ((res!!3!!0) == 32)
  ]

getEmptyCellsTests :: [TestTree]
getEmptyCellsTests =
  [ testCase "find one" $ do
      let res = getEmptyCells [[8,2,4,2],[8,32,4,16],[16,2,8,4],[16,64,256,0]]
      res @?= [(3,3)]
  , testCase "find none" $ do
      let res = getEmptyCells [[8,2,4,2],[8,32,4,16],[16,2,8,4],[16,64,256,128]]
      res @?= []
  ]


checkIfGameLostTests :: [TestTree]
checkIfGameLostTests =
  [ testCase "lost if no moves left" $ do
      let vals = [[8,2,4,2],[16,32,16,64],[32,2,8,4],[16,64,256,128]]
      checkIfGameLost vals @?= True
  ]