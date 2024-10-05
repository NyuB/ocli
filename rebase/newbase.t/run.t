  $ chmod +x newbase
  $ newbase_test () { CUSTOM_EDITOR=rebase_pass RESET=false ./newbase $1 2>/dev/null; }
  $ git init -b main
  Initialized empty Git repository in $TESTCASE_ROOT/.git/
  $ git add .
  $ git commit -m "Root commit" > /dev/null
  $ git commit --allow-empty -m "Commit 1" > /dev/null
  $ git commit --allow-empty -m "Commit 2" > /dev/null
  $ git checkout -b work 2>1 > /dev/null
  $ git commit --allow-empty -m "Commit 3" > /dev/null
  $ git commit --allow-empty -m "Commit 4" > /dev/null
  $ ONLY_COMMITS="Commit [0-9]+.*"
Rebase on any ref
  $ newbase_test main | grep -Po "${ONLY_COMMITS}"
  Commit 3 # empty
  Commit 4 # empty
  $ newbase_test HEAD~4 | grep -Po "${ONLY_COMMITS}"
  Commit 1 # empty
  Commit 2 # empty
  Commit 3 # empty
  Commit 4 # empty
When passing a number <n>, it is interpreted as the ref HEAD~<n>
  $ newbase_test 2 | grep -Po "${ONLY_COMMITS}"
  Commit 3 # empty
  Commit 4 # empty
