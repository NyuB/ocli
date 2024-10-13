## Current
### Rebase
#### Features
- Enter files navigation mode with the **:f** command. In this mode, modified files tab takes most of the available space and you can move up & down in the file list. This allow to browse the full list, that is cropped if containing to many files to fit the screen.
### tty
- Column_divided component similar to Row_divided
- Column_sliding component showing a portion of an entry array arround a given index

## 0.5.0
### Rebase
#### Features
- Cli and Renaming are now actual 'line edition modes' with cursor navigation.
#### Bugfixes
- `newbase` now restore a correct terminal state without needing to call `reset`. The `RESET` env argument is thus now ignored by `newbase`.
### tty
- Introduced a (limited) component system to modularize individual widget rendering. Each component is responsible to create its own view within a box, and to reutrn the actual dimensions of this view.

## 0.4.0
### Rebase
#### Features
- Fixup: keep the fixuped commit message and discard of the previous commit message using **F** (uppercase) to fixup, translated to `fixup -C`. **f** (lowercase) still discards the fixuped commit message and keep the previous commit message.
- Use prettier glyphs (▲▼∟ ...). Old glyphs can be toggled on with the **:raw** command, and toggled back to prettier glyphs with the **:pretty** command.
- Display only relevant arrows (▲, ▼, or ▲▼) when moving a commit.
- Add **:abort** command to discard all changes and exit the program.
- Add **:inline** command to translate the current edition to actual git output.
#### Changes
- Exploded entries know keep the original message to improve readability of the exploded result
### ppx_cross_match
- Better error messages embedded as error nodes instead of hard-failing

## 0.3.0
### Rebase
#### Features
- Introduced a 'vim like' command line. Triggered when typing **:** from either move or navigate mode. Typing **Esc** goes back to navigate mode, typing **Enter** execute the command.
  + The only command recognized for now is **:q** to quit the editor
- Reduce rebase_edit binary size by ~4MB by extracting inline tests to a separate module
- "Explode" split a commit into multiple ones, one for each file modified in the original commit
- Rename with **r** or **R** instead of **Right -> Right**. **r** keeps the original message and let you edit it, **R** erase it to let you create a new one. 
#### Changes
- Canceling a rename is now done with the **Esc** key instead of the **Left** arrow key
- Quiting is now done by typing **:q** on the command line (see the *Features* section)
- Rename is now done with the **r** or **R** keys in Move or Navigation mode (instead of **Right** from the navigation mode).
#### Bugfixes
- Fixup entries are now correctly indented even in 'Move' mode
- Fixed entry list that would behave as a sliding window even when there is enough rows to display it fully

## 0.2.0
### Rebase
#### Features
- Allow to pass a number to newbase, this number will be interpreted as HEAD~number
- Forbid fixuping the first rebase entry
#### Bugfixes
- Crop commit messages and file names to try to fit terminal width (live adapt to terminal resize)
- Do not try to display more entries than available terminal rows, make a sliding window instead
- Do not try to display more files than available terminal rows, trim modified files list instead

## 0.1.0
### Rebase
#### Features
- Display modified files along commits