#define INSTANCES_USUAL     Eq, Ord, Data, Typeable, Generic
#define INSTANCES_NEWTYPE   INSTANCES_USUAL, Enum, Ix, NFData
#define INSTANCES_MICRO     INSTANCES_NEWTYPE, Bounded, Random, Arbitrary
