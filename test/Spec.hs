{-# LANGUAGE UnicodeSyntax #-}

import System.Random
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Angle

main = defaultMain tests

type A = Angle Double -- To make signatures shorter

createDegrees :: (Floating a) => a → Angle a
createDegrees x = degrees x

createRadians :: (Floating a) => a → Angle a
createRadians x = radians x

instance (Floating a, Random a) => Arbitrary (Angle a) where
  arbitrary = oneof
    [ (fmap createDegrees (choose (-720.0, 720.0)))
    , (fmap createRadians (choose (-4 * pi, 4 * pi)))
    ]

tests :: TestTree
tests = testGroup "All Angle Tests" [qcTests]

qcTests = testGroup "QuickCheck Tests" 
  [ QC.testProperty "Addition of angles is associative" $
      associativeTestFn (∠+∠)
  , QC.testProperty "Addition of angles is commutative" $
      commutativeTestFn (∠+∠)
  , QC.testProperty "Subtraction is inverse addition" $
      subtractionIsInverseAdditionTest
  , QC.testProperty "Multiplication is repeated addition" $
      multiplicationIsRepeatedAdditionTest
  , QC.testProperty "Division is inverse multiplication" $
      divisionIsInverseMultiplicationTest
  , QC.testProperty "Multiplying divisor with quotient gives dividend" $
      multiplyingDivisorWithQuotientGivesDividendTest
  , QC.testProperty "Arcsine is inverse sine" $
      inverseTestFn sine arcsine
  , QC.testProperty "Arccosine is inverse cosine" $
      inverseTestFn cosine arccosine
  , QC.testProperty "Arctangent is inverse tangent" $
      inverseTestFn tangent arctangent
  , QC.testProperty "Normalize angle is idempotent" $
      idempotenceTestFn normalizeAngle
  , QC.testProperty "Radians are degrees times pi divided by 180" $
      degreesReturnedCorrectly
  ]

threshold = 0.00000000001
α ~== β = abs ((getRadians α) - (getRadians β)) < threshold

associativeTestFn :: (A → A → A) → A → A → A → Bool
associativeTestFn f α β θ = f (f α β) θ ~== f α (f β θ)

commutativeTestFn :: (A → A → A) → A → A → Bool
commutativeTestFn f α β = f α β ~== f β α

inverseTestFn :: (A → Double) → (Double → A ) → A  → Bool
inverseTestFn f g α = (g . f . g . f) α ~== (g . f) α

idempotenceTestFn :: (A → A) → A → Bool
idempotenceTestFn f α = (f . f) α ~== f α

multiplicationIsRepeatedAdditionTest :: A → Positive Int → Bool
multiplicationIsRepeatedAdditionTest α n' =
    repeatedAdditionRes ~== multiplicationRes
  where
    addFn = (∠+∠) α
    n = getPositive n'
    repeatedAdditionRes = iterate addFn α !! n
    multiplicationRes = α ∠* (fromIntegral (n + 1))

divisionIsInverseMultiplicationTest :: A → NonZero Double → Bool
divisionIsInverseMultiplicationTest α n' = (α ∠* n) ∠/ n ~== α
  where
    n = getNonZero n'

subtractionIsInverseAdditionTest :: A → A → Bool
subtractionIsInverseAdditionTest α β = (α ∠+∠ β) ∠-∠ β ~== α

multiplyingDivisorWithQuotientGivesDividendTest :: A → A → Bool
multiplyingDivisorWithQuotientGivesDividendTest α β = β ∠* (α ∠/∠ β) ~== α

degreesReturnedCorrectly :: A → Bool
degreesReturnedCorrectly α =
  abs ((getRadians α) - ((getDegrees α) * (pi / 180))) < threshold
