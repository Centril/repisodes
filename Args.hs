{- repisodes, a program that renames files according to episode names.
 - Copyright, 2017, Mazdak Farrokhzad
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

-- | Parses command line arguments.
module Args where

--------------------------------------------------------------------------------
-- Imports:
--------------------------------------------------------------------------------

-- base:
import Control.Applicative
  ( (<|>) )

import qualified Data.Semigroup as S

-- optparse-applicative:
import Options.Applicative
  ( Parser, ParserInfo, execParser, info, progDescDoc, headerDoc, fullDesc
  , helper, strArgument, switch, flag', long, short, help, metavar )

-- ansi-wl-pprint:
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, text, colon, dot, comma, hardline
  , (<>), (</>), (<+>), (<$$>) )

--------------------------------------------------------------------------------
-- Help texts:
--------------------------------------------------------------------------------

argHeader :: Doc
argHeader = text "repisodes, a program for renaming files by episode name."
       <$$> text "Copyright, 2017, Mazdak Farrokhzad"
       <$$> text "Distributed under GPL2 or any later version."

argPDesc :: Doc
argPDesc = 
      hardline
   <> text "The program renames files in a certain RENAME_DIRECTORY with"
  <+> text "the names of" </> text "another SOURCE_DIRECTORY" <> dot
  <+> text "If SOURCE_DIRECTORY is a file" <> comma
  <+> text "then the contents of" </> text "the files"
  </> text "will be used by parsing each line as a file name" <> dot
  <+> text "If no" </> text "SOURCE_DIRECTORY is given" <> comma
  <+> text "the standard input will be used as if it were a"
  </> text "file" <> dot <+> text "If you wish, you may instead"
  <+> text "use --interactive" <> dot <+> text "With --interactive" <> comma
  </> text "you can specify every" </> text "file name on a line" <> dot
  <+> text "To stop, enter a blank line" <> dot
  </> hardline <> hardline
   <> text "Matching is done simply via the episode keys with the following"
  </> text "regular expression" <> colon
  <+> text "[sS](\\d\\d)[eE](\\d\\d)|(\\d\\d)x(\\d\\d)"

--------------------------------------------------------------------------------
-- Data types:
--------------------------------------------------------------------------------

-- | Information about how to retrieve the new filenames.
data DstInfo
  = File
    { dstFP :: FilePath -- ^ Path to a file with names or a directory to ls.
    }
  | STDIN               -- ^ The new names are in STDIN.
  | Interactive         -- ^ The user will provide names interactively.
  deriving (Eq, Ord, Show)

-- | The command line arguments.
data CLIArgs = CLIArgs
  { isDry   :: Bool     -- ^ Is this a dry run?
  , srcPath :: FilePath -- ^ The src directory.
  , dstPath :: DstInfo  -- ^ The dst directory.
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Argument parsers:
--------------------------------------------------------------------------------

-- | A more nicely named operator (3 symbols, balanced with others...).
(<.>) :: S.Semigroup a => a -> a -> a
(<.>) = (S.<>)

-- | Argument parser (switch) for dry runs.
argDry :: Parser Bool
argDry =  switch
       $  long  "dry"
      <.> short 'd'
      <.> help  "If used, a dry run will happen. No files are renamed."

-- | Positional argument parser for directory to rename files of.
argSrc :: Parser FilePath
argSrc =  strArgument
       $  metavar "RENAME_DIRECTORY"
      <.> help "The directory with files that will be renamed."

-- | Positional argument parser for directory to ls or file with new names.
argDstPos :: Parser DstInfo
argDstPos =  File
      <$> strArgument
       (  metavar "SOURCE_DIRECTORY"
      <.> help "The directory with new filenames to use for renaming."
       )

-- | Flag (-i / --interactive) for providing file names interactively.
argInter :: Parser DstInfo
argInter = flag' Interactive $
          long  "interactive"
      <.> short 'i'
      <.> help ( unwords $
               [ "Interactive mode, when used, enter each file name on a line,"
               , "or an empty line to stop." ] )

-- | Argument parser that always succeeds with STDIN as method for new names.
argStdin :: Parser DstInfo
argStdin = pure STDIN

-- | All the ways to parse ways to get new filenames.
argDst :: Parser DstInfo
argDst = argInter <|> argDstPos <|> argStdin

-- | Combined parser.
argAll :: Parser CLIArgs
argAll = CLIArgs <$> argDry <*> argSrc <*> argDst

-- | Combined parser, help message, etc.
argParser :: ParserInfo CLIArgs
argParser =  info (helper <*> argAll) $
             fullDesc
         <.> progDescDoc (Just argPDesc)
         <.> headerDoc   (Just argHeader)

--------------------------------------------------------------------------------
-- Interface for Main:
--------------------------------------------------------------------------------

-- | Fetches CLI arguments from passed arguments to program (getArgs).
compArgs :: IO CLIArgs
compArgs = execParser argParser