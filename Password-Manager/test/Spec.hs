module Main where

import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do

    let entries = [(Entry "test" "user" "pass")] -- used in testing
    let entries2 = [(Entry "newTest" "newUser" "newPass"), (Entry "test" "user" "pass")] -- used for editOptions test

    describe "Main" $ do
        describe "inDatabase" $ do
            it "returns True if Entry in database" $
                inDatabase entries "test" == True

            it "returns False if Entry not in database" $
                inDatabase entries "notin" == False
            
        describe "getPassword" $ do
            it "returns password if Entry in database" $
                getPassword entries "test" == "pass"
            
            it "returns message if Entry not in database" $
                getPassword entries "notin" == ""
            
        describe "addEntry" $ do
            it "adds specified Entry to database" $
                addEntry entries (Entry "newName" "newUser" "newPass") == [(Entry "newName" "newUser" "newPass"), (Entry "test" "user" "pass")]
            
        describe "removeEntry" $ do
            it "removes the specified Entry from database" $
                removeEntry entries "test" == []
        
        describe "editEntry" $ do
            it "edits Entry name to name specified by user" $
                editOptions entries "test" "n" "editTest" == [(Entry "editTest" "user" "pass")]
            
            it "edits Entry username to username specified by user" $
                editOptions entries "test" "u" "editUser" == [(Entry "test" "editUser" "pass")]

            it "edits Entry password to password specified by user" $
                editOptions entries "test" "p" "editPass" == [(Entry "test" "user" "editPass")]
            it "does not edit if the new Entry name is already associated with an existing Entry in the database." $
                editOptions entries2 "test" "n" "newTest" == entries2