module Lambda.Eval
open Lambda.Ast
open Lambda.Exception
open Lambda.Utils
open Lambda.Parser

open System.Collections.Generic
open System
open System.Linq

exception InfLoopFound of Term

let rec shift t c d =
  match t with
    | TmFreeVar _  -> t
    | TmBoundVar k -> TmBoundVar (if k < c then k else k + d)
    | TmAbs (b, h) -> TmAbs ((shift b (c + 1) d), h)
    | TmApp (l, r) -> TmApp (shift l c d, shift r c d)

let rec replace t1 t2 i =
  match t1 with
    | TmFreeVar _  -> t1
    | TmBoundVar x -> if x = i then t2 else t1                
    | TmApp (l, r) -> TmApp (replace l t2 i, replace r t2 i)
    | TmAbs (b, h) -> TmAbs (replace b (shift t2 0 1) (i + 1), h)

let rec eval hist = function
  | t when hist |> HashedList.contains t ->
    InfLoopFound t |> raise
  | TmFreeVar _ | TmBoundVar _ -> None
  | TmAbs (b, h) ->
    eval hist b |> Option.map (fun x -> TmAbs (x, h))
  | TmApp (TmAbs (b, _), r) ->
    shift (replace b (shift r 0 1) 0) 0 -1 |> Some
  | TmApp (l, r) ->
    match (eval hist l) with
      | Some l' -> TmApp (l', r) |> Some
      | None -> eval hist r |> Option.map (fun x -> TmApp (l, x))


