module Lambda.Ast
open System
open System.Security.Cryptography
open System.Text
open System.Linq
open System.Collections.Generic
open FSharp.Collections
open Lambda.Utils

module AstChar = 
  begin
    let names = "xyzabcdefghijklmnorstuvwABCDEFGHIJKLMNOPQRSTUVWXYZαβγδεζηθμξπσφψωбгдёжзилфцчшщъыэюя".ToCharArray()

    let arrow = "→"
  end

type Term =
  | TmBoundVar of int
  | TmFreeVar of char
  | TmAbs of Term * char option
  | TmApp of Term * Term
  | TmFix

let rec fvOf (t : Term) =
  match t with
    | TmFreeVar c  -> set [c]
    | TmApp (l, r) -> Set.union (fvOf l) (fvOf r)
    | TmAbs (b, _) -> fvOf b
    | _ -> set []

let rec termToString cs fvs = function
  | TmFreeVar c -> to_s c
  | TmBoundVar i -> cs |> List.item i |> to_s
  | TmAbs (_, _) & t ->
    let rec dig cs = function
      | TmAbs (b, c) -> dig (c :: cs) b
      | x -> (cs, x)
    in
    let (args, b) = dig [] t in
    let args' = 
      args |> List.fold (fun args' a ->
                let rs = Set.unionMany [fvs; (List.append args' cs |> Set.ofList)] in
                match a with
                  | Some c when rs |> Set.contains c |> not ->
                    c :: args'
                  | _ ->
                    let c = AstChar.names |> Array.filter (fun c -> rs |> Set.contains c |> not)
                                          |> Array.head
                    in
                    c :: args'
              ) []
    in
    let cs' = List.append (args' |> List.rev) cs in
    sprintf "(λ%s.%s)" (args' |> String.Concat) (b |> termToString cs' fvs)
  | TmApp (_, TmApp (_, _)) & TmApp (l, r) ->
    sprintf "%s(%s)" (termToString cs fvs l) (termToString cs fvs r)
  | TmApp (l, r) ->
    sprintf "%s%s" (termToString cs fvs l) (termToString cs fvs r)
  | TmFix -> "&fix"

type Term with
  member this.ToString() = termToString [] (fvOf this) this

