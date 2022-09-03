# Summary

GitHub Action for bumping and retrieving the package version in `.cabal` files for Haskell projects. It does not affect the formatting.

# Usage

## Get

```yaml
- uses: nikita-volkov/edit-cabal-version.github-action@v1.1.0
  with:
    mode: get
```

## Set

```yaml
- uses: nikita-volkov/edit-cabal-version.github-action@v1.1.0
  with:
    mode: set
    set-value: "2.0.0"
```

## Bump

```yaml
- uses: nikita-volkov/edit-cabal-version.github-action@v1.1.0
  with:
    mode: bump
    # Index of the bumped version place.
    # `2` points to `x` in `0.1.x`
    bump-place: 2
```

## Custom Work Directory

In all modes you can provide a path to the directory which contains the Cabal-file. Use the `work-dir` option for that. If it is not specified, path of `.` is implied.

# Outputs

- `before` - version before the edit
- `after` - version after the edit

In the `get` mode both outputs are the same.
