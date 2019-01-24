## Notes

1. fixed degree divideAndConquer
2. iterative combination
3. clustering
4. task queues

### Design

The main issue is that the append operation (++) in vector is O(n) but in concat lists it is amortized O(1).
So an opportunity might be to redefine certain skeletons like 

```haskell
divideAndConquer :: (Parallelizable t)
                 => (prob -> Bool) 
                 -> (prob -> t prob)
                 -> (t sol -> sol)
                 -> (prob -> sol)
                 -> prob
                 -> sol
```

and have 

```haskell
class Parallelizable t where
  parMap :: (a -> b) -> t a -> t b
  split :: t a -> t (t a) -- not sure `t(t a)` or `Vector (t a)`
  join  :: t (t a) -> t a
  merge :: t (a,a) -> t a -> t a
  partners :: t a -> t (a,a)
```

and then have

```haskell
instance Parallelizable Vector where
  -- now add all the parallel operation of vector
  
instance Parallelizable ConcList where
  -- parallel versin of conclist with O(1) concat
```

### RESEARCH QUESTIONS

Can linear haskell give O(1) array concat(using the pointer algo defined in Control.DivideAndConquer)?
