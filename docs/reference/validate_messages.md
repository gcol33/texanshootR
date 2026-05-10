# Validate the on-disk message registry

Lints every YAML file under `inst/messages/` (and an additional path if
supplied), enforcing required fields, vocabulary membership, id
uniqueness, and combo-chain integrity. Used by the test suite and
available to contributors.

## Usage

``` r
validate_messages(path = NULL)
```

## Arguments

- path:

  Optional character path to a directory of YAML files. When `NULL`,
  validates the package's bundled messages.

## Value

A data.frame of the validated registry, returned invisibly.
