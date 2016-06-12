module Handler.TestUtils where

import TestImport
import CustomDBDataTypes

addPhysicians countN = do
  unless (countN < 2) $ addPhysicians (countN - 1)

  let physician = Physician name gender position remarks
      name = pack $ "Phys_" ++ (show countN)
      gender = if even countN then Male else Female
      position = pack $ "Pos_" ++ (show countN)
      remarks = Nothing
      -- Just $ Textarea $ pack $ "Remarks for Phys_" ++ (show count)

  runDB $ insert physician
  return ()

addPatients countN = do
  unless (countN < 2) $ addPatients (countN - 1)

  let patient = Patient name gender
        Nothing Nothing (Just email) Nothing Nothing
      name = pack $ "Pat_" ++ (show countN)
      gender = if even countN then Male else Female
      email = name ++ "@gmail.com"

  runDB $ insert patient
  return ()
