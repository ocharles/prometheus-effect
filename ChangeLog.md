# 1.1.0

- `Prometheus.GHC.ghcStats` now has the type `m (Maybe (IO ThreadId))`.
  
  This was changed to clarify what happens if GC stats aren't enabled, in which case you'll just get `Nothing` back.
 
  Don't forget to actually run the stats collector thread though! For example,
  
  ```haskell
  (startGhcStats, registry) <- buildRegistry ghcStats
  for_ startGhcStats id
  ```
	
	
# 1.0.0

Initial release.
