#define INSTANCES_USUAL     Eq, Ord, Data, Typeable, Generic
#define INSTANCES_NEWTYPE   INSTANCES_USUAL, Enum, Ix, Hashable, NFData
#ifdef QUICKCHECK
#define INSTANCES_MICRO     INSTANCES_NEWTYPE, Bounded, Random, Arbitrary, CoArbitrary
#else
#define INSTANCES_MICRO     INSTANCES_NEWTYPE, Bounded, Random
#endif
#define LensP Lens'
#define LENS(S,F,A) {-# INLINE _/**/F #-}; _/**/F :: LensP S A; _/**/F = lens F $ \ S {..} F/**/_ -> S {F = F/**/_, ..}

#define W_GREGORIAN <https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian>
