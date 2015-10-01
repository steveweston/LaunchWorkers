# LaunchWorkers
This is a Mathematica package for launching workers within a batch job.
In contrast with the ClusterIntegration package, LaunchWorkers uses
`LocalMachine` and `RemoteMachine` to start workers within the context
of a batch job.

Here's an example use of LaunchWorkers:
~~~
Needs["LaunchWorkers`"]
SetSystemOptions["ParallelOptions" -> "MathLinkTimeout" -> 100.];
LaunchWorkers[]
ParallelEvaluate[{$KernelID, $MachineName}]
CloseKernels[]
~~~
