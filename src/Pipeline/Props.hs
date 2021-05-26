{-# LANGUAGE OverloadedStrings #-}
module Pipeline.Props ( failProps
             , failTypes
             , runProps
             , compilePred
             , props
             ) where

import Cp
import List
import Turtle
import Prelude hiding (FilePath)
import Data.Maybe (isNothing, fromJust)
import Data.Text (strip)

data Prop = Prop { name    :: !Text
                 , typeSig :: !Text
                 , qc      :: !(Maybe Text)
                 } deriving (Eq, Show)

compilePred :: Text -> IO Bool
compilePred file = fmap (cond (==ExitSuccess) true false . p1) $ shellStrict command empty where
  command = "stack exec ghc " <> file <> " -- -e ':t id'"

runProp :: Text -> Prop -> IO Text
runProp file (Prop prop _ qcCommand) = let command = if isNothing qcCommand
                                                     then "stack exec ghc " <> file <> " --package QuickCheck -- -e 'quickCheck " <> prop <> "'"
                                                     else "stack exec ghc " <> file <> " --package QuickCheck -- -e '" <> (fromJust qcCommand) <> " " <> prop <> "'"
                                       in fmap p2 $ shellStrict command empty

typeProp :: Text -> Prop -> IO Text
typeProp file (Prop prop _ _) = fmap p2 $ shellStrict command empty where
  command = "stack exec ghc " <> file <> " -- -e ':t " <> prop <> "'"

failProps :: Text -> [Prop] -> IO [Text]
failProps file = cataList (either epsilon testProp) where
  epsilon = const (return [])
  testProp (p, ioPs) = runProp file p >>= cond hasPropFailed (const (((name p):) <$> ioPs)) (const ioPs)
  hasPropFailed = not . null . match (has "Failed!")

failTypes :: Text -> [Prop] -> IO [Text]
failTypes file = cataList (either epsilon testType) where
  epsilon = const (return [])
  testType (p, ioPs) = typeProp file p >>= cond (sameTypeFailed p) (const (((name p):) <$> ioPs)) (const ioPs)
  sameTypeFailed p = (/= ((strip . typeSig) p)) . strip

runProps :: Text -> [Prop] -> IO ()
runProps file = cataList (either return (uncurry (>>) . ((print <=< runProp file)  >< id )))

props :: [Prop]
props = map (uncurry (uncurry Prop)) $
  [ (("prop_in_out_idExpAr", "prop_in_out_idExpAr :: Eq a => ExpAr a -> Bool\n"), Nothing)
  , (("prop_out_in_idExpAr", "prop_out_in_idExpAr :: Eq a => OutExpAr a -> Bool\n"), Nothing)
  , (("prop_sum_idr", "prop_sum_idr :: (Floating a, Real a) => a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_sum_idl", "prop_sum_idl :: (Floating a, Real a) => a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_product_idr", "prop_product_idr :: (Floating a, Real a) => a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_product_idl", "prop_product_idl :: (Floating a, Real a) => a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_e_id", "prop_e_id :: (Floating a, Real a) => a -> Bool\n"), Nothing)
  , (("prop_negate_id", "prop_negate_id :: (Floating a, Real a) => a -> Bool\n"), Nothing)
  , (("prop_double_negate", "prop_double_negate :: (Floating a, Real a) => a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_optimize_respects_semantics", "prop_optimize_respects_semantics\n  :: (Floating a, Real a) => a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_const_rule", "prop_const_rule :: (Real a, Floating a) => a -> Bool\n"), Nothing)
  , (("prop_var_rule", "prop_var_rule :: Bool\n"), Nothing)
  , (("prop_sum_rule", "prop_sum_rule :: (Real a, Floating a) => ExpAr a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_product_rule",  "prop_product_rule\n  :: (Real a, Floating a) => ExpAr a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_e_rule", "prop_e_rule :: (Real a, Floating a) => ExpAr a -> Bool\n"), Nothing)
  , (("prop_negate_rule", "prop_negate_rule :: (Real a, Floating a) => ExpAr a -> Bool\n"), Nothing)
  , (("prop_congruent", "prop_congruent :: (Floating a, Real a) => a -> ExpAr a -> Bool\n"), Nothing)
  , (("prop_calcLine_def", "prop_calcLine_def :: NPoint -> NPoint -> Float -> Bool\n"), Nothing)
  , (("prop_bezier_sym", "prop_bezier_sym :: [[Rational]] -> Gen Bool\n"), Just "quickCheckWith (stdArgs {maxSize = 10, maxSuccess = 200})")
  , (("prop_avg", "prop_avg :: Ord a => [a] -> Property\n"), Nothing)
  , (("prop_cat", "prop_cat :: Integer -> Property\n"), Nothing)
  ]
