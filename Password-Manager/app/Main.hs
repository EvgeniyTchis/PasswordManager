{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Char8 as BS
import Data.Either
import System.IO as IO
import Data.Maybe
import Jose.Jws
import Jose.Jwa
import Jose.Jwt
import Crypto.Password as PS

data Entry = Entry 
-- This represents an entry by the user
-- An entry consists of a name/label, username, and password
    { name     :: String
    , username :: String
    , password :: String
    } deriving (Show, Generic, ToJSON, FromJSON) -- The tags ToJSON and FromJSON allow the Aeson package to automate JSON encoding and decoding

---------------------------------------------------------- Password Manager ----------------------------------------------------------
-- The central functions of the application revolve around the "database" which contains all the
-- entries input by the user, in the application this is represented as a list of entries or [Entry].
-- As the user adds/removes/edits/etc. the entries it is this [Entry] that changes, additionally other information
-- such as the file name, file path, and master password are passed between function calls as well.
-- When the user saves their progress the [Entry] is then encoded into JSON, encrypted, and written to a file with
-- the file name the user specifices (this file is created in the directory the app is located). Likewise, when the
-- user loads an already existing file by providing its full path (or just the file name + extension if it's in the same directory)
-- the contents of the file are decrypted with the master password, and then decoded from JSON to [Entry].
--------------------------------------------------------------------------------------------------------------------------------------

main :: IO [Entry]
-- Initializes the application and handles the creation and loading of databases.
main =
    do 
        Prelude.putStrLn "Welcome to your password manager."
        Prelude.putStrLn "Type (N) to create a new database and (L) to load an existing database and (E) to exit."
        response <- IO.getLine
        if (response `Prelude.elem` ["N", "n", "(N)", "(n)"]) 
            then createNew
            else if (response `Prelude.elem` ["L", "l", "(L)", "(l)"]) 
                then loadDatabase
                else if (response `Prelude.elem` ["E", "e", "(E)", "(e)"]) 
                    then return []
                    else do
                        Prelude.putStrLn "Invalid response, please try again."
                        main


createNew :: IO [Entry]
-- Creates new database by taking in relevant information such as file/database name, and master password
-- also initializes the database (this is the empty list, called entries)
createNew = 
    do
        Prelude.putStrLn "What would you like to name your database?"
        databaseName <- IO.getLine
        Prelude.putStrLn "What would you like your master password to be?"
        masterPassword <- IO.getLine
        let databasePath = databaseName ++ ".json"                                               -- Make file extension .json can change to something else if needed
        let entries = [] :: [Entry]
        newEntries <- mainScreen databaseName databasePath masterPassword entries                -- Here we enter the main program
        return []                                                                                -- Have to return IO [Entry] because the main function retursn IO [Entry]

loadDatabase :: IO [Entry]
loadDatabase = 
    do
        Prelude.putStrLn "What is the name of your database file?"
        databaseName <- IO.getLine
        Prelude.putStrLn "Please enter the full path to your database file."
        databasePath <- IO.getLine
        Prelude.putStrLn "Please enter your master password."
        masterPassword <- IO.getLine

        file <- Prelude.readFile databasePath                                                    -- Read the contents of the encrypted file
        let tryDecrypt = hmacDecode (BS.pack masterPassword) (BS.pack file)                      -- This has type (Either JwtError Jws)
        if isRight tryDecrypt                                                                    -- If the type is Jws (ie. the decryption as successfull) then decode JSON
            then 
                do
                    let Right decryptedText = tryDecrypt
                    let loadedEntries = Data.Aeson.decode (BL.fromStrict (snd decryptedText)) :: Maybe [Entry]
                    let entries = fromJust loadedEntries
                    Prelude.putStrLn "====================================================="
                    Prelude.putStrLn "Entries Successfully loaded!"
                    Prelude.putStrLn "====================================================="
                    newEntries <- mainScreen databaseName databasePath masterPassword entries    -- Here we enter the main program
                    return []                                                                    -- Have to return IO [Entry] because the main function retursn IO [Entry]
            else                                                                                 
                do                                                                               -- Otherwise the type is JwtError, which indicates an incorrect password
                    Prelude.putStrLn "====================================================="
                    Prelude.putStrLn "Incorrect Password. Please try again."
                    Prelude.putStrLn "====================================================="
                    main                                                                         -- Return to main to either create new database or re-try with different password
                    return []

mainScreen :: String -> String -> String -> [Entry] -> IO [Entry]
-- This is the main-frame (or main-screen) of the application, here the user can manipulate their database
-- however they like. The user is able to retrieve passwords and list, add, remove, and edit entries. Additonally,
-- the user is able to save and encrypt their database of entries for use with the app at a later time. Every option
-- will return the user to this main-screen with the exception of the save option, which instead exits the application.
-- **NOTE- this is still really messy :) **
mainScreen name path mpass entries = 
    do
        Prelude.putStrLn "====================================================="
        Prelude.putStrLn "Welcome to your database, what would you like to do?"
        Prelude.putStrLn "Type [G] to get a password from an entry."
        Prelude.putStrLn "Type [L] to list your current entries."
        Prelude.putStrLn "Type [A] to add another entry."
        Prelude.putStrLn "Type [R] to remove an entry."
        Prelude.putStrLn "Type [E] to edit an entry."
        Prelude.putStrLn "Type [P] to generate a password."
        Prelude.putStrLn "Type [S] to save and exit."
        Prelude.putStrLn "====================================================="
        response <- IO.getLine
        if (response `Prelude.elem` ["G", "g", "(G)", "(g)"])
            then do                                                                         -- This block of code retrieves password of a specified entry
                Prelude.putStrLn "Enter the name of the entry whose password you want."
                entryName <- IO.getLine
                Prelude.putStrLn (getPassword entries entryName)
                mainScreen name path mpass entries                                          -- Return to main-screen after performing user-specified function
            else if (response `Prelude.elem` ["L", "l", "(L)", "(l)"])                      -- This returning occurs in every block except the save/exit block
                then do
                    listEntries entries                                                     -- This block prints the list of current entries to terminal
                    mainScreen name path mpass entries
                else if (response `Prelude.elem` ["A", "a", "(A)", "(a)"]) 
                    then do                                                                 -- This block takes in the information for a new Entry from the user
                        Prelude.putStrLn "What is the name of your new entry?"              -- Creates the entry and adds it to the database
                        newName <- IO.getLine
                        Prelude.putStrLn "What is the username of your new entry?"
                        newUser <- IO.getLine
                        Prelude.putStrLn "What is the password of your new entry?"
                        newPass <- IO.getLine
                        let newEntries = addEntry entries (Entry newName newUser newPass)
                        Prelude.putStrLn "Completed"                                         -- Not sure if these completed messages are useful, can remove
                        mainScreen name path mpass newEntries
                    else if (response `Prelude.elem` ["R", "r", "(R)", "(r)"]) 
                        then do                                                              -- This block removes an entry from the database
                            Prelude.putStrLn "What is the name of the entry you wish to remove?"
                            entryName <- IO.getLine
                            let newEntries = removeEntry entries entryName
                            Prelude.putStrLn "Completed"
                            mainScreen name path mpass newEntries
                        else if (response `Prelude.elem` ["E", "e", "(E)", "(e)"])
                            then do                                                           -- This block edits an entry rom the the database (more info in editOptions function)
                                Prelude.putStrLn "What is the name of the entry you wish to edit?"
                                entryName <- IO.getLine
                                Prelude.putStrLn "Would you like to edit the name [N], username [U], or password [P]?"
                                editParam <- IO.getLine
                                Prelude.putStrLn "Enter the new value of the parameter."
                                newParam <- IO.getLine
                                let newEntries = editOptions entries entryName editParam newParam
                                Prelude.putStrLn "Completed"
                                mainScreen name path mpass newEntries
                                else if (response `Prelude.elem` ["S", "s", "(S)", "(s)"])
                                    then do                                                      -- This block enccodes the database into JSON, encrypts it, and then writes to file
                                        let jstring = Data.Aeson.encode entries                  -- Here we encode our [Entry] (ie. our database) into JSON, the function returns a lazy ByteString
                                        let strictEntries = BL.toStrict jstring                  -- Convert the lazy ByteString to strict ByteString for use in encryption function
                                        let key = (BS.pack mpass)                                -- Convert master password into strict ByteString for use in encryption function
                                        let Right (Jwt jwt) = hmacEncode HS256 key strictEntries -- Encrypt function has type (Either JwtError Jwt), Right is successfull encryption returns type Jwt
                                        BS.writeFile path jwt                                    -- Write the encrypted JSON into a file and exit application.
                                        return []
                                    else if (response `Prelude.elem` ["P", "p", "(P)", "(p)"])
                                        then do
                                             Prelude.putStrLn "Your Generated Password is: "
                                             let up = Uppercase
                                             let lo = Lowercase
                                             let di = Digit
                                             let sy = Symbol
                                             let pfUp = Include up
                                             let pfLo = Include lo
                                             let pfDi = Include di
                                             let pfSy = Include sy
                                             let pfLe = Length 14
                                             genPass <- PS.generatePassword [pfUp, pfLo, pfDi, pfSy, pfLe]
                                             Prelude.putStrLn genPass
                                             mainScreen name path mpass entries
                                        else do
                                        Prelude.putStrLn "Invalid response, please try again."   -- If we recieve invalid input, simply print error message and return to main-screen
                                        mainScreen name path mpass entries
                    
inDatabase :: [Entry] -> String -> Bool
-- Return true if the entry with the given name is in the database, false otherwise
-- *** TODO - Need to make each entry name unique ***
inDatabase [] entryName = False
inDatabase ((Entry n _ _):t) entryName
    | n == entryName = True
    | otherwise = inDatabase t entryName

getPassword :: [Entry] -> String -> String
-- Retrieves and prints password from the entry with the given name, returns failure message
-- if entry is not found in database
getPassword [] entryName = "Entry not found."
getPassword ((Entry n _ p):t) entryName
    | n == entryName = p
    | otherwise = getPassword t entryName

listEntries :: [Entry] -> IO ()
-- Print a list of all current entries into the terminal
listEntries [] = Prelude.putStrLn " "
listEntries ((Entry n u p):t) = 
    do
        Prelude.putStrLn (n ++ " " ++ u ++ " " ++ "********")
        listEntries t

addEntry :: [Entry] -> Entry -> [Entry]
-- Adds a user-specified entry into the database
addEntry [] entry = [entry]
addEntry list entry = entry:list

removeEntry :: [Entry] -> String -> [Entry]
-- Removes an entry with the given name from the database
removeEntry [] _ = []
removeEntry ((Entry n u p):t) entryName
    | n == entryName = removeEntry t entryName
    | otherwise = removeEntry ((Entry n u p):t) entryName

editOptions :: [Entry] -> String -> String -> String -> [Entry]
-- Parses the user input and calls the appropriate edit function for the editParam
-- as specified by the user.
editOptions entries entryName editParam newParam
    | editParam `Prelude.elem` ["[n]", "[N]", "n", "N"] = editEntryName entries entryName newParam
    | editParam `Prelude.elem` ["[u]", "[U]", "u", "U"] = editEntryUser entries entryName newParam
    | editParam `Prelude.elem` ["[p]", "[P]", "p", "P"] = editEntryPass entries entryName newParam
    | otherwise = entries

editEntryName :: [Entry] -> String -> String -> [Entry]
-- Edits the name of the specified entry with the name given by the user
editEntryName [] _ _ = []
editEntryName ((Entry n u p):t) entryName newParam
    | inDatabase ((Entry n u p):t) newParam = ((Entry n u p):t)
    | n == entryName = (Entry newParam u p): t
    | otherwise = (Entry n u p) : editEntryName t entryName newParam

editEntryUser :: [Entry] -> String -> String -> [Entry]
-- Edits the username of the specified entry with the username given by the user
editEntryUser [] _ _ = []
editEntryUser ((Entry n u p):t) entryName newParam
    | n == entryName = (Entry n newParam p): t
    | otherwise = (Entry n u p) : editEntryUser t entryName newParam

editEntryPass :: [Entry] -> String -> String -> [Entry]
-- Edits the password of the specified entry with the password given by the user
editEntryPass [] _ _ = []
editEntryPass ((Entry n u p):t) entryName newParam
    | n == entryName = (Entry n u newParam): t
    | otherwise = (Entry n u p) : editEntryPass t entryName newParam
