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

{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}

module Main where

--------------------------------------------------------------------------------
-- Imports:
--------------------------------------------------------------------------------

-- base:
import Data.Maybe (catMaybes)
import Control.Monad (void, filterM, unless)
import Control.Arrow ((<<<), (&&&), (***))

-- containers:
import Data.IntMap (IntMap, fromList, elems, intersectionWith)

-- regex-applicative
import Text.Regex.Applicative (RE, sym, psym, (<|>), findFirstInfix)
import Text.Regex.Applicative.Common (digit)

-- filepath:
import System.FilePath (takeBaseName, takeDirectory, takeFileName, (</>))

-- directory:
import System.Directory (listDirectory, renameFile, doesFileExist)

-- arguments:
import Args (CLIArgs (..), DstInfo (..), compArgs)

--------------------------------------------------------------------------------
-- General utilities:
--------------------------------------------------------------------------------

-- | Composition of a unary operator with a binary.
-- f .: g = λ x y. f(g(x, y))
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- | If as a function.
branch :: t -> t -> Bool -> t
branch a b c = if c then a else b

-- | Pack functor + snd into a functor of original value + snd.
gobbleSnd :: Functor f => (f a, b) -> f (a, b)
gobbleSnd (m, b) = (, b) <$> m

-- | Wraps a lazy /IO/ computation without arguments and forces its contents.
force :: IO sa -> IO sa
force mx = do !x <- mx; x `seq` return x
{-# INLINE force #-}

--------------------------------------------------------------------------------
-- Parsing episodes:
--------------------------------------------------------------------------------

-- | Packs season and episode into an Int.
asEpisode :: Int -> Int -> Int
asEpisode s e = s * 100 + e

-- | Parse two digits into a decimal number.
twoDigit :: RE Char Int
twoDigit = (\a -> (a * 10 +)) <$> digit <*> digit

-- | Parses: [sS](\d\d)[eE](\d\d)|(\d\d)x(\d\d)
pepisode :: RE Char Int
pepisode = let sym2 f s = psym $ \x -> x == f || x == s
           in  asEpisode <$> (sym2 's' 'S' *> twoDigit)
                         <*> (sym2 'e' 'E' *> twoDigit)
           <|> asEpisode <$> twoDigit <* sym 'x'
                         <*> twoDigit

-- | Extract the episode from a file path.
findEpisode :: FilePath -> Maybe Int
findEpisode = fmap (\(_, ep, _) -> ep) . findFirstInfix pepisode

--------------------------------------------------------------------------------
-- Matching files by episode:
--------------------------------------------------------------------------------

-- | Create a Map: Episode => Filepath from a list of filepaths.
episodeMap :: [FilePath] -> IntMap FilePath
episodeMap = fromList . catMaybes .
             fmap (gobbleSnd <<< findEpisode . takeBaseName &&& id)

-- | Intersect file paths with matching episodes.
pairPaths :: [FilePath] -> [FilePath] -> [(FilePath, FilePath)]
pairPaths = curry $ elems . uncurry (intersectionWith (,)) <<<
                    episodeMap *** episodeMap

-- | Replace snd by taking the directory from fst and file name from snd.
newName :: (FilePath, FilePath) -> (FilePath, FilePath)
newName (src, dst) = (src, takeDirectory src </> takeFileName dst)

--------------------------------------------------------------------------------
-- Terminal utilities:
--------------------------------------------------------------------------------

-- | Output a newline.
nl :: IO ()
nl = putStrLn ""

-- | Print out a header.
paragraph :: [String] -> IO ()
paragraph = (nl >>) . (>> (putStrLn $ replicate 80 '-')) . putStrLn . unwords

-- | Print out each string on its own line.
infos :: [String] -> IO ()
infos  = (>> nl) . mapM_ putStrLn

-- | Print out each pair as: fst => snd.
infoPaths :: [(FilePath, FilePath)] -> IO ()
infoPaths = infos . fmap (uncurry (++) <<< id *** (" => " ++))

-- | Print out that we found stuff in file.
infoFs :: String -> [String] -> IO [String]
infoFs file fs = do
  paragraph ["Found the following file(names) in:", file]
  infos fs
  pure fs

--------------------------------------------------------------------------------
-- Reading list of new filenames:
--------------------------------------------------------------------------------

-- | Read contents of the string and split into non-empty lines.
getFs :: IO String -> IO [FilePath]
getFs = fmap (filter (not . null) . lines) . force

-- | Reads a file and splits the lines in it.
readFs :: FilePath -> IO [FilePath]
readFs file = getFs (readFile file) >>= infoFs file

-- | List the files in the given file path.
listFs :: FilePath -> IO [FilePath]
listFs dir = fmap (dir </>) <$> listDirectory dir
         -- doesFileExist is suboptimal, but its the cross platform version...
         >>= filterM doesFileExist
         >>= infoFs dir

-- | Read list of new filenames from a file.
fileFs :: FilePath -> IO [FilePath]
fileFs file = branch readFs listFs <$> doesFileExist file >>= ($ file)

-- | Read list of new filenames from STDIN.
stdinFs :: IO [FilePath]
stdinFs = getFs getContents

-- | Read list of new filenames interactively.
interFs :: IO [FilePath]
interFs = do
  paragraph ["Reading interactively, enter blank line to finish."] >> nl
  takeWhile (not . null) . lines <$> getContents

-- | Compute the list of new names.
getNewNames :: DstInfo -> IO [FilePath]
getNewNames = \case
  File file   -> fileFs file
  STDIN       -> stdinFs
  Interactive -> interFs

--------------------------------------------------------------------------------
-- Pairing, new names, renaming:
--------------------------------------------------------------------------------

-- | Construct [(src, dst)] by intersecting the filepaths
-- with episodes in common.
pairsOf :: [FilePath] -> [FilePath] -> IO [(FilePath, FilePath)]
pairsOf srcFs dstFs = do
  let pairs = pairPaths srcFs dstFs
  if null pairs
  then nl >> putStrLn "No pairs found." >> nl
  else paragraph ["Found the following pairs:"] >> infoPaths pairs
  pure pairs

-- | Construct [from => to].
renames :: [(FilePath, FilePath)] -> IO [(FilePath, FilePath)]
renames pairs = do
  let rns = fmap newName pairs
  unless (null rns) $ paragraph ["Will rename as:"] >> infoPaths rns
  pure rns

-- | Setup for progDry by getting list of from => to filenames to rename.
setup :: FilePath -> DstInfo -> IO [(FilePath, FilePath)]
setup src dst = do
  srcFs     <- listFs src
  dstFs     <- getNewNames dst
  pairs     <- pairsOf srcFs dstFs
  renames pairs

-- | Rename [from => to].
renameFiles :: [(FilePath, FilePath)] -> IO ()
renameFiles renFs = unless (null renFs) $ do
  putStrLn "Renaming files!"
  mapM_ (uncurry renameFile) renFs
  putStrLn "Success!"

--------------------------------------------------------------------------------
-- Exposed programs:
--------------------------------------------------------------------------------

-- | Program without actual renaming,
-- will tell the user how renaming will be done.
progDry :: FilePath -> DstInfo -> IO ()
progDry = void .: setup

-- | The actual main program.
prog :: FilePath -> DstInfo -> IO ()
prog = (>>= renameFiles) .: setup

-- | The main program.
main :: IO ()
main = do
  CLIArgs dry src dst <- compArgs
  (if dry then progDry else prog) src dst