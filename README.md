# tree-sitter-powershell

Powershell grammar for [tree-sitter](https://github.com/tree-sitter/tree-sitter)

## References

- [Powershell 7.3](https://learn.microsoft.com/en-us/powershell/scripting/lang-spec/chapter-15?view=powershell-7.3)

## Development

### Bump version

```sh
# Remove auto generated files that contains the version
rm package-lock.json Cargo.lock Makefile src/parser.c

# Bump the version in the following files
vim tree-sitter.json pyproject.toml package.json Cargo.toml

# Regenerate the auto files
npm run generate

# Show the diffs
git diff
```
