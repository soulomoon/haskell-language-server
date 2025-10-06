From the hls paper, we read the following:

## Current status

The current build system of hls is compeletely pull based. Whenever a something changed, we issue an restart with some keys as the source of changed, it mark all of the reverse dependencies for these keys as dirty, and leave them to be rebuilt when they are needed.

This create some response delay problem if the rebuild takes a long time. So the current workaround hls take is kick action. For each file of interest, we kick some related keys to be rebuilt right after the restart, so that when the user actually need them, they are already built. It works pretty well in practice.

We do not have FRP framework that do upsweep in a multithreaded way, as seen in earlier attempts of matthew pickering.

I thought it would be interesting to alter the hls build system to behave like push-pull FRP, with "pull" to build the dependency graph, and "push" to upsweep the dirty nodes in parallel.

1. This could potentially improve the responsiveness of hls for action that won't be immediately needed after a restart. Example actions, hovering over a symbol, or jumping to definition. Thees actions are not immediately needed after a restart, but are likely to be needed after. With push model, these actions could be more responsive, since the dirty nodes are being rebuilt in the background.
2. On the other side, for action that would be immediately needed, we would have a slow down. Since push model always push all dirty nodes to be rebuilt, it might lead to some unnecessary work being done. This is especially true if the repo is large, an complete upsweep of dirty nodes might take quite a while. Example actions, code lenses, semantic highlighting. They are needed right after a restart.

I would like to see how this trade off plays out in practice. Even though, this would be a significant change to the build system, but I still manage to carry out this experiment. For codebase like hls, the push model seems to work pretty well, the extra work upsweep of nodes is not much of burden to the build system, the second kind of actions do slow down, but not obvious for human's eyes. However, for huge codebase like ghc, the push model seems to be a bit too aggressive, the push delay becomes significant after openning a lot of files which makes the dependency graph very large.

But in the process of carrying out the impelmentation, I found some other improvement to HLS, which I will discuss below.

1. We kills only what is actually changed, instead of every running threads as before. This is a significant improvement, since it allows us to keep some running threads alive, which could skip restart. These threads can categorized into two types:

* The threads that are building rule keys.
* The threads that are building one time actions.

## Benefits in theory

Aside from the responsiveness improvement, this change could also bring some other benefits:

1. Better CPU resource utilization:

  A downsweep build system as hls current has, thread number bound is not possible to implement. Ghc api required some IO hooks to be run. So we can not take a continuation based approach to halt the a key build whenever we wait for a another key as shake does. This means that if we have a lot of keys being built, we might end up with a lot of threads waiting for IO, which can lead to thread thrashing and poor resource utilization. Since haskell have green threads, this is not a big problem for IO Bound rules like parsing, interface reading, but for rules that are CPU bound like typechecking, too many threads is suboptimal.

  By upsweeping dirty nodes in parallel, we can limit the number of threads being used at any given time, which might lead to better CPU resource utilization.

## Conveat

There are some conveats to this approach:

1. Originally we only flush reverse dep to a key is computed and clean, now we need to track forward dep while a key is running.

1. Complexity: Implementing a push-pull FRP system is more complex than a pure pull-based system. This could lead to more bugs and maintenance overhead.
2. Overhead: There is some overhead associated with managing the upsweep process, which could negate some of the performance benefits.
3. Consistency: Ensuring that the upsweep process maintains consistency in the face of concurrent
