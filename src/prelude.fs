[<AutoOpen>]
module Prelude

open System
open System.Text.RegularExpressions    

let inline to_s x = x.ToString()

let inline (?|) opt df = defaultArg opt df

let private ccl (fc: ConsoleColor) =
  Console.ForegroundColor <- fc;
  { new IDisposable with
      member x.Dispose() = Console.ResetColor() }

let cprintf color format =
  Printf.kprintf (fun s -> use c = ccl color in printf "%s" s) format

let cprintfn color format =
  Printf.kprintf (fun s -> use c = ccl color in printfn "%s" s) format

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

let (|DefaultValue|) dv x =
  match x with
    | Some v -> v
    | None -> dv

module List = 
  begin
    let rec takeToEnd n xs =
      if List.length xs > n then
        (xs |> List.take n) :: takeToEnd n (xs |> List.skip n)
      else
        [xs]
  end

module Async =
  begin
    open System.Threading
    open System.Threading.Tasks
    open Microsoft.FSharp.Control
    open FSharp.Control.Reactive

    let inline run x = Async.RunSynchronously x

    let withTimeout (timeout : TimeSpan) a =
      async {
        try
          let! child = Async.StartChild(a, int timeout.TotalMilliseconds) in
          let! result = child in
          return Some result
        with
          | :? TimeoutException -> return None
      }
end

