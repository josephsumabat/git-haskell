# git-haskell

## About
This project is an implementation of a subset of `git` commands in Haskell for learning purposes.

### Planned implementation
  - Git index (`git add`)
  - Low level git commands
    - `git cat-file`
    - `git hash-object`
    - `git commit-tree`
  - `git checkout`
  - `git log`
  - `git status`

### (Planned) Differences from vanilla `git`
  - Errors/error messages may be handled differently/different
    - (Currently just throwing exceptions for parse errors/not git directory errors)
  - Not planning to handle every argument for implemented commands just major ones
  - No packfiles

### Progress
  - Parse `blob` files
  - Parse `tree` files
  - `cmd_catfile` and `cmd_hashfile` functions working (will refactor)

## Building
Build with stack:
```
stack build
```

## Testing
TODO
