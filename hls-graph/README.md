# hls-graph - a limited reimplementation of Shake for in-memory build graphs

`ghcide` was originally built on top of [Shake](http://shakebuild.com), a Haskell build system. Nowadays Shake has been replaced by a special purpose implementation of a build graph called hls-graph, which drops all the persistency features in exchange for simplicity and performance.

Features:

* Dynamic dependencies
* User defined rules (there are no predefined File rules as in Shake)
* Build reports (a la Shake profiling)
* "Reactive" change tracking for minimal rebuilds (not available in Shake)
* Incremental topological ordering using the Pearce-Kelly algorithm for efficient dirty key propagation

What's missing:

* Persistence
* A default set of rules for file system builds
* A testsuite
* General purpose application - many design decisions make assumptions specific to ghcide

## Performance Optimizations

### Pearce-Kelly Topological Ordering

The build graph maintains an incremental topological ordering using the Pearce-Kelly algorithm. This optimization significantly speeds up dirty key propagation during rebuilds:

- **Before**: Computing transitive dirty keys required a DFS traversal followed by O(V log V) sorting
- **After**: The topological order is maintained incrementally, reducing sorting to O(V) filtering

The algorithm maintains an integer order for each key in the dependency graph. When dependencies change, only affected portions of the ordering are updated, ensuring minimal overhead during normal operation while providing fast lookups when computing dirty key propagation.
