# Changelog

This project adheres to [Haskell PVP](https://pvp.haskell.org/).

The format of this changelog is based on
[Keep a Changelog](http://keepachangelog.com/en/1.0.0/)


## 0.2.0.0
### Changed
- Error reporting (Breaking change).
  All functions now adhere to the `Either`
  [conventions](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Either.html):
    real errors are returned in a `Left`,
    successes are returned in a `Right`.
  The non-errors constructors of `PMError`
    (`NoError` and `GotData`) were moved to a new type `PMSuccess`.

### Added
- The `poll` function which binds to
[`Pm_Poll`](http://portmedia.sourceforge.net/portmidi/doxygen/group__grp__io.html#g344e6ba3edb14d560ccb074d66b56590).
- The `readEventsToBuffer` function, to read events in a user-supplied buffer.
- The `PMSuccess` type representing non-errors, with its associated
    functions `getText` and `getSuccessText`.


## 0.1.6.1

commit 3b92597d84113969371612fb5c590b9f8c77d53c
Author: Paul H. Liu <paul@thev.net>
Date:   Wed Jul 6 09:34:40 2016 -0700

    Fix case sensitivity, and bump version to 0.1.6.1 to correct version number

commit 2d5266dd325f2c248a9022679abfcebe9b317a95
Author: Paul H. Liu <paul@thev.net>
Date:   Tue Jul 5 21:37:24 2016 -0700

    Move to github, and bump version to 0.1.5.3

commit d7946afcfd7c3204c1ea999ed7fdb9c4e9ff20a7
Author: Paul Liu <paul@thev.net>
Date:   Sun Apr 3 00:46:02 2016 -0700

    PMEvent uses CLong as message type to better handle SysEx.

    Ignore-this: 367f146403228813c914a23bfc2a1248
    - This is a breaking change. Use encodeMsg or decodeMsg to work with PMMsg.

    darcs-hash:20160403074602-da8e5-05c03013660b8fc91f843fa22143ec2b7ea5d7b5

commit d5217416a5477f10a36727254425a572d18b7e4a
Author: Paul Liu <paul@thev.net>
Date:   Sun Apr 3 00:40:29 2016 -0700

    Use LANGUAGE pragma instead of OPTIONS_GHC

    Ignore-this: c65fb70875497b8e6c46f76ee9105dbe

    darcs-hash:20160403074029-da8e5-378ba491b94131265a94f115bf068971f869ea2b

commit 358452adfcf91c2436752f9b95ebcc5d5124db97
Author: Paul Liu <paul@thev.net>
Date:   Sun Apr 3 00:40:09 2016 -0700

    Detect freebsd OS

    Ignore-this: 9f819a332d364679acc2e005488f9375

    darcs-hash:20160403074009-da8e5-16571867d53492c3fb7dc84df8fb8a500a71312a

commit c64496ded49633100864e8e0e192a9333ef8caa5
Author: Paul Liu <paul@thev.net>
Date:   Mon Sep 21 22:17:09 2015 -0700

    Add CHANGELOG.txt to package, and bump version to 0.1.5.2

    Ignore-this: 465173236978f9557dbbc574b5d34a8f

    darcs-hash:20150922051709-da8e5-824c6e4c060e3c85ee3831cebde4a88fd3f8582b

commit fda70ecb49f85fd4dccc5daaecbdd9f96fe1651f
Author: Paul Liu <paul@thev.net>
Date:   Mon Sep 21 11:29:09 2015 -0700

    Update README and bump version to 0.1.5.1

    Ignore-this: 98500cbf8d27a5068949cb458648500e

    darcs-hash:20150921182909-da8e5-fc100f8c68c20229a726f31ce8fd7051e8c13163

commit 044b0f47f3fc3bc6ade39c2972fb4b40c16c4e05
Author: Paul Liu <paul@thev.net>
Date:   Mon Sep 21 11:17:37 2015 -0700

    Bump version to 0.1.5

    Ignore-this: 4fb2ae12ea9145bac0f0459ff3073169

    darcs-hash:20150921181737-da8e5-75d0be5001d877daf0c359687c66408afec0639b

commit a158c93a9d50c8bc56a3be908effd4020f9e7cc0
Author: Mark Lentczner <mark.lentczner@gmail.com>
Date:   Mon Sep 21 11:16:04 2015 -0700

    On some systems, Int is not the same size as CInt.

    Ignore-this: 7bd6231264f14d66dd4c0156cc3266ba

    - Added hidden module Sound.PortMidi.DeviceInfo, moving DeviceInfo and peekDeviceInfo there
     - this allowed it to be a hsc2hs module, enabling peekDeviceInfo to use C generated structure offsets and sizes
    - FFI declarations for C calls that take and return PmError and PmDeviceID have been changed to take and return CInt rather than Int
    - Haskell functions interfacing with C calls have had fromIntegral calls added as needed
    - toPMError convenience function added, which replaced many calls to toEnum
    - FFI declaration for Pm_CountDevices added, countDevices became a Haskell function
    - PMError's toEnum function produces a more useful error message when the "impossible" happens

    darcs-hash:20150921181604-63162-de113c7c4da4f42d274b8b6c0641563c8670f606

commit cfae744f937cf4dfafd9e03bce9e0088a5b375f5
Author: Mark Lentczner <mark.lentczner@gmail.com>
Date:   Mon Sep 21 11:15:00 2015 -0700

    Add strictness annotation when necessary

    Ignore-this: 9e5792f9ad04c797fcd51b244d7485fa

    darcs-hash:20150921181500-63162-dd522488d7f680cf536655d8cb1a878e489dcc98

commit 4fb6f505b2f495bad6ddd1072ff41b51001d9b44
Author: Paul Liu <paul@thev.net>
Date:   Wed May 6 00:33:40 2015 -0700

    Update cabal file, and bump version to 0.1.4

    Ignore-this: 8b1d17f401ebd925a4bc6ff7778b453d

    darcs-hash:20150506073340-da8e5-0cf190250915842e35fc5f3177e576d84fb82026

commit a164e464065c84f3f4e958c53271f930c097f2d3
Author: Paul Liu <paul@thev.net>
Date:   Wed May 6 00:33:10 2015 -0700

    Use withCAString for writeSysEx

    Ignore-this: 9499c201b5fb289cda27c090fdbf1541

    darcs-hash:20150506073310-da8e5-ecd2cf32c5f121cc95b97c1e9a495d189b0391cc

commit c3ad563bbce9fbc25c8470865ded7b070d0d030e
Author: Paul Liu <paul@thev.net>
Date:   Wed Sep 16 11:06:11 2009 -0700

    remove conflicts in cabal file

    Ignore-this: 3ef1f4b655182e4cc5eb861e8f2427f1

    darcs-hash:20090916180611-da8e5-47cff7e32f38c7d54155784dbf2ddee6f1053faf

commit 6b0963d50576c0572155e421a4e6d02c8c649d8c
Author: Paul Liu <paul@thev.net>
Date:   Wed Sep 16 11:01:59 2009 -0700

    fix compilation problem on OS X

    Ignore-this: a87d64d1c1a59bd161edecbde23932e3

    darcs-hash:20090916180159-da8e5-62407ea637a40e789f7e316a6fe181f56bc8fa1f

commit 13c1d569f8566968bd0a214903d11c08a1101a2a
Author: Paul Liu <paul@thev.net>
Date:   Thu Jul 30 14:32:59 2009 -0700

    make cabal work for ghc 6.10.* on OS X

    darcs-hash:20090730213259-da8e5-f5a2558120fba824bb05440a8d7e3a2985fab2ed

commit 8b7e2ea3c5ef6cb2b436e136964c2ee5814ffc02
Author: Paul Liu <paul@thev.net>
Date:   Thu Sep 4 18:59:02 2008 -0700

    PortMidi-0.1 initial release

    darcs-hash:20080905015902-da8e5-902a16abf5d28adda47ab45f6c0927a1042caf09
