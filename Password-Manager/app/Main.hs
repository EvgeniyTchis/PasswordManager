module Main where

import Lib

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
-- Starts the application.
main = start