# Summary

GitHub Action for bumping and retrieving the package version in `.cabal` files for Haskell projects.

# Usage

## Bump

```yaml
- uses: nikita-volkov/edit-cabal-version.github-action@v1
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
- uses: nikita-volkov/edit-cabal-version.github-action@v1
  with:
    mode: read
```

Outputs:

- `before` - current version
- `after` - current version (no changes in this mode)
