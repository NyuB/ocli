## Current
### Rebase
- Allow to pass a number to newbase, this number will be interpreted as HEAD~number
- Forbid fixuping the first rebase entry
- Bugfix: Crop commit messages and file names to try to fit terminal width (live adapt to terminal resize)
- Bugfix: Do not try to display more entries than available terminal rows, make a sliding window instead

## 0.1.0
### Rebase
- Display modified files along commits