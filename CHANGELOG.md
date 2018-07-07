# Changelog

This project adheres to [Haskell PVP](https://pvp.haskell.org/).


## 0.2.0.0 [not released]

- [Breaking change] :
  The non-error constructor 'NoError' and 'GotData' were moved from 'PMError'
  to a new type 'PMSuccess', so that 'PMError' only contains real errors.
  To adhere to the 'Either' conventions where errors are returned in 'Left',
  successes in 'Right',
  functions that returned 'PmError' now return 'Either PmError PMSuccess' and
  functions that returned 'Either <Type-for-success> PmError' now return
  'Either PmError <Type-for-success>'.

## 0.1.6.1 (last version on hackage when the Changelog was created)
