# Summary

GitHub Action for bumping and retrieving the package version in `.cabal` files for Haskell projects. It does not affect the formatting.

# Usage

## Bump

```yaml
- uses: nikita-volkov/edit-cabal-version.github-action@v1.1.0
  with:
    mode: bump
    # Index of the bumped version place.
    # `2` points to `x` in `0.1.x`
    bump-place: 2
```

Outputs:

- `before` - version prior to bumping
- `after` - version after bumping

## Retrieving

```yaml
- uses: nikita-volkov/edit-cabal-version.github-action@v1.1.0
  with:
    mode: read
```

Outputs:

- `before` - current version
- `after` - current version (no changes in this mode)

## Custom Work Directory

In all modes you can provide a path to the directory which contains the Cabal-file. Use the `work-dir` option for that. If it is not specified, path of `.` is implied.
