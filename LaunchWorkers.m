(* :Name: Launch Workers *)

(* :Title: Alternative Cluster Integration using SSH *)

(* :Context: LaunchWorkers` *)

(* :Author: Steve Weston *)

(* :Summary:

    This package provides the function "LaunchWorkers" which
    launches/starts workers on cluster compute nodes that have been
    allocated via a batch queueing system.  This allows you to allocate
    cores on one or more nodes with a single batch queueing command and
    then start workers on those cores from your script. On many
    clusters, this is preferable to starting each worker in a separate
    job, as in the ClusterIntegration package.

*)

(* :Package Version: 1.0 *)

(* :Mathematica Version: 9.0 *)

(* :Limitations:

    Only supports PBS/Torque, LSF and SGE.

    May need to increase the "MathLinkTimeout" since ssh can be slow:
      SetSystemOptions["ParallelOptions" -> "MathLinkTimeout" -> 100.];

*)


(*****************************************************************************)


BeginPackage["LaunchWorkers`"]

LaunchWorkers::usage =
  "LaunchWorkers[] calls LaunchKernels for nodes allocated by LSF, PBS or SGE"
LaunchWorkers::noslots = "No allocated slots: unable to launch any workers"
LaunchWorkers::noexist = "File does not exist: `1`"

Begin["`Private`"]

Needs["SubKernels`LocalKernels`"]
Needs["SubKernels`RemoteKernels`"]

(*
    Modify $RemoteCommand to use full path to math/wolfram executable.
    The value of $RemoteCommand assumes that the Mathematica executables
    are in your PATH, but that isn't necessarily the case.
    Of course, by modifying $RemoteCommand in this way, we are assuming
    that Mathematica is installed in the same directory path on the
    local and remote machines.
*)
$sshcmd =
  Module[{kernelpath, repstr},
    kernelpath = FileNameJoin[{$TopDirectory, "Executables", "math"}];
    If[! FileExistsQ[kernelpath],
      Message[LaunchWorkers::noexist, kernelpath]
    ];
    (* Only replace the executable name if it is unqualified *)
    repstr = " " <> kernelpath <> " ";
    StringReplace[$RemoteCommand, {" wolfram " -> repstr, " math " -> repstr}]
  ]

(* Create a list of lists containing node name and worker counts *)
$nodecounts =
  Module[{nfile, extract, fields},
    (* Check for LSF *)
    nfile = Environment["LSB_DJOB_HOSTFILE"];
    If[nfile =!= $Failed && StringLength[nfile] > 0 && FileExistsQ[nfile],
      Tally[ReadList[nfile, String]],
      (* Else check for PBS/Torque *)
      nfile = Environment["PBS_NODEFILE"];
      If[nfile =!= $Failed && StringLength[nfile] > 0 && FileExistsQ[nfile],
        Tally[ReadList[nfile, String]],
        (* Else check for SGE *)
        nfile = Environment["PE_HOSTFILE"];
        If[nfile =!= $Failed && StringLength[nfile] > 0 && FileExistsQ[nfile],
          extract[fields_] := {fields[[1]], ToExpression[fields[[2]]]};
          extract /@ StringSplit[ReadList[nfile, Record]],
          (* Else display a warning message *)
          Message[LaunchWorkers::noslots];
          {}
        ]
      ]
    ]
  ]

(*
    Start a kernel on each allocated node.  If the node name matches
    your own machine name, use "LocalMachine", else use "RemoteMachine".
    This test could fail, depending on the configuration of your local
    machine and your batch queueing system, but that will only result
    in using "RemoteMachine" unnecessarily.
*)
LaunchWorkers[] :=
  Module[{n, i, node, num},
    n = Length[$nodecounts];
    If[n > 0,
      For[i = 1, i <= n, i++,
        {node, num} = $nodecounts[[i]];
        If[node === $MachineName,
          LaunchKernels[LocalMachine[num]],
          (* Else launch a remote worker *)
          LaunchKernels[RemoteMachine[node, $sshcmd, num]]
        ]
      ],
      (* Else display a warning message *)
      Message[LaunchWorkers::noslots]
    ]
  ]

End[]

Protect[LaunchWorkers]

EndPackage[]
