module Lambda.Main
open Lambda.Parser
open Lambda.Ast
open Lambda.Eval
open Lambda.Utils
open Lambda.Exception
open Lambda.Session

open System
open System.Drawing
open System.Drawing.Imaging
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks

open FSharp.Control.Reactive
open Microsoft.FSharp.Control
open Microsoft.FSharp.Collections

open Mono.Terminal
open ImageBuilder

let rec loop (session : Session) =
  let i = scan "> " in
  if (String.IsNullOrWhiteSpace i) then
    loop session
  else
    let session' = 
#if !DEBUG
      try
#endif
        match i with
          | Regex @"\s*:help\s*" _ ->
            cprintfn ConsoleColor.Blue "- :help      .. show this"
            cprintfn ConsoleColor.Blue "- :quit      .. quits the repl"
            cprintfn ConsoleColor.Blue "- :showvars  .. shows the defined meta variables"
            cprintfn ConsoleColor.Blue "- <varname> := <exp>"
            cprintfn ConsoleColor.Blue "             ... define a new meta variable"
            cprintfn ConsoleColor.Blue "- <varname> :b= <exp>"
            cprintfn ConsoleColor.Blue "             ... evaluate an expression and define the"
            cprintfn ConsoleColor.Blue "                 result as a new meta variable"
            cprintfn ConsoleColor.Blue "- <exp>      ... evaluate an expression"
            printfn ""
            session
          | Regex @"\s*:quit\s*" _ -> Environment.Exit 0; session
          | Regex @"\s*:showvars\s*" _ ->
            for (k, v) in session.dict |> Map.toSeq do
              cprintfn ConsoleColor.DarkGray "&%s := %s" k (to_s v)
            done
            session
          | Regex @"([^ ]+)\s*:b=\s*(.+)" [n; t] ->
            let (_, res) = session.parse t |> session.eval in
            cprintfn ConsoleColor.DarkGray "%s := %s" n (to_s res);
            match res with
              | Some x -> session.defMeta n x
              | None ->
                cprintfn ConsoleColor.Red "infinite reduction!"
                session
          | Regex @"([^ ]+)\s*:=\s*(.+)" [n; t] ->
            let e = session.parse t in
            cprintfn ConsoleColor.DarkGray "%s := %s" n (to_s e);
            session.defMeta n e
          | t ->
            let evl = session.parse t 
                   |> session.evalAsync
                   |> Async.withTimeout (TimeSpan.FromSeconds 5.0)
                   |> Async.run
            in
            match evl with
              | Some (_, res) ->
                match res with
                  | Some x ->
                    if sprintf "@cannorin %s [xxx]" (to_s x) |> TwitterText.isValid then
                      cprintfn ConsoleColor.DarkGray "%s" (to_s x)
                    else
                      cprintfn ConsoleColor.Red "the result is too big!" 
                  | None ->
                    cprintfn ConsoleColor.Red "infinite reduction!"
              | None -> cprintfn ConsoleColor.Red "computation timeout"
            session
#if !DEBUG
      with
        | e -> cprintfn ConsoleColor.Red "RUNTIME ERROR: %s" e.Message; session
#endif
    in
    printfn "";
    GC.Collect();
    loop session'

[<EntryPoint>]
let rec main argv =
  match (argv |> Array.toList) with
    | "--measure" ::_ ->
      let s = Session None in
      s.parse "&pow 2 10" |> s.evalAsync |> Async.run |> ignore;
      0
    | "--debug" :: _
    | "--repl" :: _ ->
      printfn "beta reduction repl"
      printfn "type :help for help"
      printfn ""
      loop (Session None); 0
    | _ -> 
      let lockfile = "/tmp/betareductionbot.lock"  in
      try
        if File.Exists lockfile |> not then
          try
            (File.Create lockfile).Dispose();
            Thread.Sleep 5000;
            TwitterBot.start (Session None)
          with
            | e ->
              cprintf ConsoleColor.Red "error: fatal error, wait 10 seconds to restart:\n%s" (to_s e);
              sprintf "fatal error, wait 10 seconds to restart:\n%s" (to_s e) |> TwitterBot.report |> ignore;
              Thread.Sleep 10000;
              Process.Start(Assembly.GetEntryAssembly().Location) |> ignore;
              Environment.Exit(0)
        else
          cprintf ConsoleColor.Red "error: process is a duplicate"
        0
      finally
        File.Delete lockfile |> ignore;
        0
    
