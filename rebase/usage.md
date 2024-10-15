# General

`rebase_edit` is an editor to edit git interactive rebase files. `newbase` is a wrapper around git rebase -i using rebase_edit as the editor for the rebase file. Under the hood, it only returns back to git the rebase file with modifications corresponding to your actions in the editor, and thus has semantics very close to what `git rebase -i` offers. If you know how to `rebase`, you should `newbase` with ease ;)

You can use `newbase <REF>` as a drop-in replacement for `git-rebase -i <REF>`. If you pass a `number` as `REF`, it will be translated to `HEAD~number`

# Modal editor

`newbase` is a modal editor, with quite a few modes offering few actions each. You switch between `Commit navigation`, `Commit move`, `Commit renaming`, `CLI` and `File navigation` modes.

Remember that it is only a façade for git rebase,  all your actions are translated to `git rebase -i` todo entries applied **when you exit newbase**.

## Commit navigation mode
This is the main mode of newbase, and the active one when you enter the editor. In this mode, the selected commit is highlighted and any of the action is applied to this commit.
- **Up/Down** arrow keys -> navigate to previous/next commit
- **d** or **D** -> drop selected commit
- **f** -> fixup selected commit, discarding it's message
- **F** -> fixup selected commit, discarding its parent's message
- **r** -> enter [Commit renaming mode](#commit-renaming-mode), with the original message as editing base
- **R** -> enter [Commit renaming mode](#commit-renaming-mode), with an empty string as editing base
- **x** or **X** -> 'explode' selected commit, creating one commit for each modified file
- **Right arrow key** -> enter [Commit move mode](#commit-move-mode)
- **:** -> enter [CLI mode](#cli-mode)
- **TAB** enter [File navigation mode](#file-navigation-mode)


## Commit move mode
- **Up/Down** arrow keys -> move commit up/down
- **Esc** -> enter [Commit navigation mode](#commit-navigation-mode)
- **:** -> enter [CLI mode](#cli-mode)

## Commit renaming mode
In this mode, the current commit message becomes editable
- **Esc** -> discard changes and enter [Commit navigation mode](#commit-navigation-mode)
- **Enter** -> keep changes and enter [Commit navigation mode](#commit-navigation-mode)

## File navigation mode
In this mode, the modified files panel on the right takes more space and you can navigate between files.
- **Up/Down arrow keys** -> navigate to previous/next file
- **Esc** or **TAB** -> enter [Commit navigation mode](#commit-navigation-mode)

## CLI mode
- **Enter** -> apply current command if valid
- **Esc** -> enter [Commit navigation mode](#commit-navigation-mode)
### Available commands
- **:q** -> exit newbase and apply rebase entries
- **:abort** -> discard all edits and exit newbase
- **:pretty** -> use unicode characters for prettier display
- **:raw** -> display only ansi characters