## Current
### Rebase
#### Features
- Introduced a 'vim like' command line. Triggered when typing **:** from either move or navigate mode. Typing **Esc** goes back to navigate mode, typing **Enter** execute the command.
  + The only command recognized for now is **:q** to quit the editor
- Reduce rebase_edit binary size by ~4MB by extracting inline tests to a separate module
- "Explode" split a commit into multiple ones, one for each file modified in the original commit
#### Changes
- Canceling a rename is now done with the **Esc** key instead of the **Left** arrow key
- Quiting is now done by typing **:q** on the command line (see the *Features* section)
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