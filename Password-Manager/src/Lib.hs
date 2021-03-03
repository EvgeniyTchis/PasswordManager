{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import Data.Either
import System.IO
import Data.Maybe
import Jose.Jws
import Jose.Jwa
import Jose.Jwt
import Crypto.Password as PS
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Console.ANSI
import System.Clipboard
import Control.Exception

data Entry = Entry 
-- This represents an entry by the user
-- An entry consists of a name/label, username, and password
    { name     :: String
    , username :: String
    , password :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON) -- The tags ToJSON and FromJSON allow the Aeson package to automate JSON encoding and decoding
    



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

start :: IO [Entry]
-- Initializes the application and handles the creation and loading of databases.
start =
    do 
        setTitle "Password Manager"
        flushScreen 100
        putStrLn "Welcome to your password manager."
        putStrLn "Type (N) to create a new database and (L) to load an existing database and (E) to exit."
        response <- getLine
        if (response `elem` ["N", "n", "(N)", "(n)"])
            then
            createNew
            else if (response `elem` ["L", "l", "(L)", "(l)"]) 
                then
                loadDatabase
                else if (response `elem` ["E", "e", "(E)", "(e)"]) 
                    then
                    return []
                    else do
                        putStrLn "Invalid response, please try again."
                        start

createNew :: IO [Entry]
-- Creates new database by taking in relevant information such as file/database name, and master password
-- also initializes the database (this is the empty list, called entries)
createNew = 
    do
        putStrLn "What would you like to name your database?"
        databaseName <- getLine
        putStrLn "What would you like your master password to be?"
        masterPassword <- getLine
        let databasePath = databaseName ++ ".json"                                               -- Make file extension .json can change to something else if needed
        let entries = [] :: [Entry]
        newEntries <- mainScreen databaseName databasePath masterPassword entries                -- Here we enter the main program
        return []                                                                                -- Have to return IO [Entry] because the main function retursn IO [Entry]

loadDatabase :: IO [Entry]
loadDatabase = 
    do
        putStrLn "What is the name of your database file?"
        databaseName <- getLine
        putStrLn "Please enter the full path to your database file."
        databasePath <- getLine
        putStrLn "Please enter your master password."
        masterPassword <- getLine

        file <- catch (readFile databasePath) (\(e::IOException) -> do                           -- Read the contents of the encrypted file
            putStrLn "Invalid file path."                                                        -- Throw IOException if path is invalid and return to main
            putStrLn "Press any key to return to the start screen."
            getResponse <- getLine
            start
            return [])                                                                               
        let tryDecrypt = hmacDecode (BS.pack masterPassword) (BS.pack file)                      -- This has type (Either JwtError Jws)
        if isRight tryDecrypt                                                                    -- If the type is Jws (ie. the decryption as successfull) then decode JSON
            then 
                do
                    let Right decryptedText = tryDecrypt
                    let loadedEntries = Data.Aeson.decode (BL.fromStrict (snd decryptedText)) :: Maybe [Entry]
                    let entries = fromJust loadedEntries
                    putStrLn "====================================================="
                    putStrLn "Entries Successfully loaded!"
                    putStrLn "====================================================="
                    newEntries <- mainScreen databaseName databasePath masterPassword entries    -- Here we enter the main program
                    return []                                                                    -- Have to return IO [Entry] because the main function retursn IO [Entry]
            else                                                                                 
                do                                                                               -- Otherwise the type is JwtError, which indicates an incorrect password
                    putStrLn "====================================================="
                    putStrLn "Incorrect Password. Please try again."
                    putStrLn "Press any key to return to the start screen."
                    putStrLn "====================================================="
                    getResponse <- getLine
                    start                                                                         -- Return to main to either create new database or re-try with different password
                    return []

mainScreen :: String -> String -> String -> [Entry] -> IO [Entry]
-- This is the main-frame (or main-screen) of the application, here the user can manipulate their database
-- however they like. The user is able to retrieve passwords and list, add, remove, and edit entries. Additonally,
-- the user is able to save and encrypt their database of entries for use with the app at a later time. Every option
-- will return the user to this main-screen with the exception of the save option, which instead exits the application.
mainScreen name path mpass entries = 
    do
        clearScreen
        putStrLn "====================================================="
        putStrLn "Welcome to your database, what would you like to do?"
        putStrLn "Type [G] to get a password from an entry."
        putStrLn "Type [L] to list your current entries."
        putStrLn "Type [A] to add another entry."
        putStrLn "Type [R] to remove an entry."
        putStrLn "Type [E] to edit an entry."
        putStrLn "Type [P] to generate a password."
        putStrLn "Type [S] to save and exit."
        putStrLn "====================================================="
        response <- getLine
        clearScreen
        if (response `elem` ["G", "g", "(G)", "(g)"])
            then handleGet name path mpass entries
            else if (response `elem` ["L", "l", "(L)", "(l)"])
                then handleList name path mpass entries
                else if (response `elem` ["A", "a", "(A)", "(a)"]) 
                    then handleAdd name path mpass entries
                    else if (response `elem` ["R", "r", "(R)", "(r)"]) 
                        then handleRemove name path mpass entries
                        else if (response `elem` ["E", "e", "(E)", "(e)"])
                            then handleEdit name path mpass entries
                                else if (response `elem` ["P", "p", "(P)", "(p)"])
                                    then handleGenPass name path mpass entries
                                    else if (response `elem` ["S", "s", "(S)", "(s)"])
                                        then handleSave name path mpass entries
                                        else do
                                            putStrLn "Invalid response, please try again."   -- If we recieve invalid input, simply print error message and return to main-screen
                                            mainScreen name path mpass entries


-- ###############################################################################################################
-- ############################ I/O command function to clean-up  mainScreen function ############################
-- ###############################################################################################################
handleGet :: String -> String -> String -> [Entry] -> IO [Entry]
-- Handles the I/O of the get password command. Gets specified password and copies to clipboard.
handleGet name path mpass entries = do
    clearScreen
    putStrLn "Enter the name of the entry whose password you want."
    entryName <- getLine
    if inDatabase entries entryName
        then do
            setClipboardString (getPassword entries entryName)
            putStrLn "Password has been copied to your clipboard"
            putStrLn ""
            putStrLn "Press any key to return to the main screen."
            getResponse <- getLine
            mainScreen name path mpass entries 
        else do
            putStrLn "Entry not found."
            putStrLn ""
            putStrLn "Press any key to return to the main screen."
            getResponse <- getLine
            mainScreen name path mpass entries

handleList :: String -> String -> String -> [Entry] -> IO [Entry]
-- Handles the I/O of the list entries command.
handleList name path mpass entries = do
    listEntries entries
    putStrLn ""
    putStrLn "Press any key to clear the list and return to the main screen."
    getResponse <- getLine
    mainScreen name path mpass entries

handleAdd :: String -> String -> String -> [Entry] -> IO [Entry]
-- Handles the I/O of the add entry command.
handleAdd name path mpass entries = do
    clearScreen
    putStrLn "What is the name of your new entry? Remember entry names must be unique."
    newName <- getLine
    if inDatabase entries newName
        then do
            putStrLn "Entry with that name already exists. Please try again with a different name or remove the existing entry."
            putStrLn ""
            putStrLn "Press any key to return to the main screen."
            getResponse <- getLine
            mainScreen name path mpass entries 
        else do
            putStrLn "What is the username of your new entry?"
            newUser <- getLine
            putStrLn "What is the password of your new entry?"
            newPass <- getLine
            let newEntries = addEntry entries (Entry newName newUser newPass)
            mainScreen name path mpass newEntries

handleRemove :: String -> String -> String -> [Entry] -> IO [Entry]
-- Handles the I/O of the remove entry command.
handleRemove name path mpass entries = do
    clearScreen
    putStrLn "What is the name of the entry you wish to remove?"
    entryName <- getLine
    if inDatabase entries entryName
        then do
            let newEntries = removeEntry entries entryName
            mainScreen name path mpass newEntries
        else do
            putStrLn "Entry not found."
            putStrLn ""
            putStrLn "Press any key to return to the main screen."
            getResponse <- getLine
            mainScreen name path mpass entries

handleEdit :: String -> String -> String -> [Entry] -> IO [Entry]
-- Handles the I/O of the edit entry command.
handleEdit name path mpass entries = do
    clearScreen
    putStrLn "What is the name of the entry you wish to edit?"
    entryName <- getLine
    if inDatabase entries entryName
        then do
            putStrLn "Would you like to edit the name [N], username [U], or password [P]?"
            editParam <- getLine
            putStrLn ""
            putStrLn "Enter the new value of the parameter."
            putStrLn "If editing entry names remember they must be unique, otherwise no change will occur."
            newParam <- getLine
            let newEntries = editOptions entries entryName editParam newParam
            mainScreen name path mpass newEntries
        else do
            putStrLn "Entry not found."
            putStrLn ""
            putStrLn "Press any key to return to the main screen."
            getResponse <- getLine
            mainScreen name path mpass entries

handleGenPass :: String -> String -> String -> [Entry] -> IO [Entry]
-- Handles the I/O of the generate password command.
handleGenPass name path mpass entries = do
    clearScreen
    putStrLn "Your Generated Password is: "
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
    putStrLn genPass
    putStrLn ""
    putStrLn "Press any key to clear the password and return to the main screen."
    getResponse <- getLine
    mainScreen name path mpass entries

handleSave :: String -> String -> String -> [Entry] -> IO [Entry]
-- Handles the I/O of the save command.
-- Specifically, this block encodes the database into JSON, encrypts it, and then writes to file
handleSave name path mpass entries = do
    clearScreen
    let jstring = Data.Aeson.encode entries                            -- Here we encode our [Entry] (ie. our database) into JSON, the function returns a lazy ByteString
    let strictEntries = BL.toStrict jstring                            -- Convert the lazy ByteString to strict ByteString for use in encryption function
    let key = (BS.pack mpass)                                          -- Convert master password into strict ByteString for use in encryption function
    let Right (Jwt jwt) = hmacEncode HS256 key strictEntries           -- Encrypt function has type (Either JwtError Jwt), Right is successfull encryption returns type Jwt
    catch (BS.writeFile path jwt) (\(e::IOException) -> do             -- Write the encrypted JSON into a file and exit application.
        putStrLn "Unable to save file, perhaps file name is invalid?"  -- Throw IOException if file name is invalid and exit out of application
        putStrLn "Press any key to exit."
        getResponse <- getLine
        putStrLn "Thank you :)")                                    
    return []

flushScreen :: Int -> IO ()
-- Cheeky function that just moves the terminal text from the top of the screen to the bottom
-- this is so that when we flush the screen later on the text doesn't seemingly magically
-- float down to the bottom of the terminal screen
flushScreen 0 = do
    putStrLn ""
flushScreen num = do
    putStrLn ""
    flushScreen (num - 1)

-- ###########################################################################################################################################
-- ############################ Database functions - these are purely functional (Except listEntries... Shhhh...) ############################
-- ###########################################################################################################################################

inDatabase :: [Entry] -> String -> Bool
-- Return true if the entry with the given name is in the database, false otherwise
inDatabase [] entryName = False
inDatabase ((Entry n _ _):t) entryName
    | n == entryName = True
    | otherwise = inDatabase t entryName

getPassword :: [Entry] -> String -> String
-- Retrieves and prints password from the entry with the given name, returns failure message
-- if entry is not found in database
getPassword [] entryName = ""
getPassword ((Entry n _ p):t) entryName
    | n == entryName = p
    | otherwise = getPassword t entryName

listEntries :: [Entry] -> IO ()
-- Print a list of all current entries into the terminal
listEntries [] = putStrLn " "
listEntries ((Entry n u p):t) = 
    do
        putStrLn (n ++ " " ++ u ++ " " ++ "********")
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
    | otherwise = (Entry n u p): removeEntry t entryName

editOptions :: [Entry] -> String -> String -> String -> [Entry]
-- Parses the user input and calls the appropriate edit function for the editParam
-- as specified by the user.
editOptions entries entryName editParam newParam
    | editParam `elem` ["[n]", "[N]", "n", "N"] = editEntryName entries entryName newParam
    | editParam `elem` ["[u]", "[U]", "u", "U"] = editEntryUser entries entryName newParam
    | editParam `elem` ["[p]", "[P]", "p", "P"] = editEntryPass entries entryName newParam
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
