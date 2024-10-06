## Current
### Rebase
#### Changes
- Canceling a rename is now done with the **Esc** key instead of the **Left** arrow key
#### Features
- Reduce rebase_edit binary size by ~4MB by extracting inline tests to a separate module
- "Explode" split a commit into multiple ones, one for each file modified in the original commit
#### Bugfixes
- Fixup entries are now correctly indented even in 'Move' mode

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