# thyme

[![Status](https://travis-ci.org/liyang/thyme.png)](https://travis-ci.org/liyang/thyme)

<http://hackage.haskell.org/package/thyme>

A Haskell date and time library based on [time](http://hackage.haskell.org/package/time).

* Trades more speed for less precision.
* Improves type-system constraints on date time arithmetic.

## Building

### Library

```
stack build --flag thyme:lens
```

### Haddock

```
stack haddock --flag thyme:lens
```
