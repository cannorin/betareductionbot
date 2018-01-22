module Lambda.Main
open Lambda.Parser
open Lambda.Ast
open Lambda.Eval
open Lambda.Utils
open Lambda.Exception
open Lambda.Session

open System
open Microsoft.FSharp.Collections
open Mono.Terminal

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
          | Regex @"\s*:quit\s*" _ -> Environment.Exit 0; session
          | Regex @"([^ ]+)\s*:b=\s*(.+)" [n; t] ->
            let (_, res) = session.parse t |> session.eval in
            cprintfn ConsoleColor.DarkGray "%s := %s" n (to_s res);
            match res with
              | Some x -> session.defMeta n x
              | None -> session
          | Regex @"([^ ]+)\s*:=\s*(.+)" [n; t] ->
            let e = session.parse t in
            cprintfn ConsoleColor.DarkGray "%s := %s" n (to_s e);
            session.defMeta n e
          | t ->
            let (_, res) = session.parse t |> session.eval in
            match res with
              | Some x ->
                if sprintf "@cannorin %s [xxx]" (to_s x) |> TwitterText.isValid then
                  cprintfn ConsoleColor.DarkGray "----> Ok"
                else
                  cprintfn ConsoleColor.Red "big!" 
              | None ->
                cprintfn ConsoleColor.Red "inf!"
            session
#if !DEBUG
      with
        | e -> printfn "RUNTIME ERROR: %s" e.Message; session
#endif
    in
    printfn "";
    GC.Collect();
    loop session'

[<EntryPoint>]
let main argv =
  match (argv |> Array.toList) with
    | "--debug" :: _ ->
      loop (Session None)
    | _ -> TwitterBot.start (Session None)
  0


