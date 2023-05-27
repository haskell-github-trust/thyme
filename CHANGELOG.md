## unreleased

## 0.4

* Supported GHC 9 and older
* Changed the type of `mkUTCTime :: Day -> DiffTime -> UTCTime` to `mkUTCTime :: Year -> Month -> DayOfMonth -> Hour -> Minute -> Double -> UTCTime`. Use the `UTCTime` pattern synonym instead if needed.
* Miscellaneous API additions and refactors