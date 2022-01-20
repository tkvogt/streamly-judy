# streamly-judy
Stream the key/value-pairs of a judy array

In the [judy](https://hackage.haskell.org/package/judy-0.4.1) library the functions

```bash
keys :: JudyImmutable a -> IO [Key]
elems :: JE a => JudyImmutable a -> IO [a]
toList :: JE a => JudyImmutable a -> IO [(Key, a)] 
```

return lists, which can use quite a lot of memory. This is problematic when the array is big.
It makes much more sense to stream a judy array into an arbitrary data structure.
Instead of adding these functions to [judy](https://hackage.haskell.org/package/judy-0.4.1/docs/Data-Judy.html) or [streamly](https://hackage.haskell.org/package/streamly-0.8.1),
we make a new library to not force new dependencies on either of the two.

