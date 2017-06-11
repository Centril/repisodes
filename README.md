# repisodes
Batch renaming of tv-episode filenames.

## installing

To install, you will need Haskell and stack installed on your machine.

Then, clone the repository from git, cd into it, and run `stack install` in the folder.

## Using

```
λ repisodes -h

repisodes, a program for renaming files by episode name.
Copyright, 2016, Mazdak Farrokhzad
Distributed under GPL2 or any later version.

Usage: repisodes.exe [-d|--dry] RENAME_DIRECTORY ([SOURCE_DIRECTORY] |
                     [-i|--interactive])

  The program renames files in a certain RENAME_DIRECTORY with the names of
  another SOURCE_DIRECTORY. If SOURCE_DIRECTORY is a file, then the contents of
  the files will be used by parsing each line as a file name. If no
  SOURCE_DIRECTORY is given, the standard input will be used as if it were a
  file. If you wish, you may instead use --interactive. With --interactive,
  you can specify every file name on a line. To stop, enter a blank line.

  Matching is done simply via the episode keys with the following
  regular expression: [sS](\d\d)[eE](\d\d)|(\d\d)x(\d\d)

Available options:
  -h,--help                Show this help text
  -d,--dry                 If used, a dry run will happen. No files are renamed.
  RENAME_DIRECTORY         The directory with files that will be renamed.
  SOURCE_DIRECTORY         The directory with new filenames to use for renaming.
  -i,--interactive         Interactive mode, when used, enter each file name on
                           a line, or an empty line to stop.
```
