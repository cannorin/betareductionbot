module Lambda.Parser
open System
open ScanRat
open Lambda.Exception
open Lambda.Ast
open Microsoft.FSharp.Collections

type ParsedTerm =
  | PVar of char
  | PNat of uint32
  | PAbs of char * ParsedTerm
  | PApp of ParsedTerm * ParsedTerm
  | PMeta of string

let rec toUTerm names dict = function
  | PVar c ->
    match (names |> List.tryFindIndex ((=) c)) with
      | Some i -> TmBoundVar i
      | None -> TmFreeVar c
  | PNat i ->
    let rec c i =
      if i = 0u then
        PVar 'x'
      else
        PApp (PVar 'f', c(i - 1u))
    in
    PAbs ('f', PAbs ('x', c i)) |> toUTerm names dict
  | PAbs (c, t) ->
    TmAbs (toUTerm (c :: names) dict t, Some c)
  | PApp (l, r) ->
    TmApp (toUTerm names dict l, toUTerm names dict r)
  | PMeta s ->
    match (dict |> Map.tryFind s) with
      | Some x -> x
      | None ->
        LambdaException (sprintf "Meta variable '%s' is not defined" s, None, ErrorState.MetaVariableFailed) |> raise


let parse code =
  let tfold name x d f =
    let a = production name in
    a.rule
      <- a + x --> f
      |- d
    a
  let tfold_d name x f = tfold name x x f
  let add = fun (x, y) -> x + y

  let spaces = tfold "spaces" ~~" " ~~"" add
  let ss x = spaces +. x .+ spaces
  let prefix = ss (oneOf "\\λ^" --> ignore) --> ignore
  let name = List.concat [['a'..'z'];['A'..'Z']] |> String.Concat |> oneOf
  let num =
    tfold_d "digits" (oneOf "0123456789" --> fun d -> uint32 d - uint32 '0') (fun (a, b) -> a * 10u + b)
  
  let parstart = ss ~~"(" --> ignore
  let parend = ss ~~")" --> ignore
  let sep = ss ~~"." --> ignore
  let meta = ~~"&" --> ignore

  let term =
    let toAbs cs t = 
      Seq.fold (fun t x -> PAbs(x, t)) (PAbs(Seq.head cs, t)) (Seq.skip 1 cs)
    let toApp ts = 
      System.Linq.Enumerable.Aggregate (ts, fun a b -> PApp(a, b))
    
    let term : ScanRatCombinators.Parser<ParsedTerm> = production "term"
    let var = ss name --> fun x -> PVar x
    let abst = prefix +. (((tfold_d "args" (name --> fun x -> x.ToString()) add) --> fun (x : string) -> x.ToCharArray() |> Seq.rev) .+ sep ) + term --> fun(x, y) -> toAbs x y
    let meta = (spaces + meta) +. (tfold_d "name" (name --> fun x -> x.ToString()) add) .+ spaces --> fun x -> PMeta x
    let numt = spaces +. num .+ spaces --> fun x -> PNat x
    let apply : ScanRatCombinators.Parser<ParsedTerm> = production "apply"
    let tWithoutApply = spaces +. (var |- abst |- numt |-  meta |- (parstart +. term .+ parend)) .+ spaces
    apply.rule <- tfold_d "apply2" (tWithoutApply --> fun x -> [x]) (fun(x, y) -> List.concat[x;y]) --> toApp
    term.rule <- spaces +. (apply |- tWithoutApply) .+ spaces
    term

  match parse term code with
    | Success s -> s.value
    | Failure f -> LambdaException ("Wrong syntax", None, ErrorState.WrongSyntax) |> raise

