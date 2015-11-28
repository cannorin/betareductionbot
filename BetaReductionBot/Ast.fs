namespace BetaReductionBot
open System
open System.Security.Cryptography
open System.Text
open System.Linq
open System.Collections.Generic

  module Ast =
    begin

      let names = "xyzabcdefghijklmnorstuvwABCDEFGHIJKLMNOPQRSTUVWXYZαβγδεζηθμξπσφψωбгдёжзилфцчшщъыэюя".ToCharArray()

      let arrow = "→"

      let private md5p = new MD5CryptoServiceProvider()

      let private md5 (x : int) = (BitConverter.GetBytes x |> md5p.ComputeHash |> BitConverter.ToString).GetHashCode()

      let private md5s (x : string) = (Encoding.ASCII.GetBytes x |> md5p.ComputeHash |> BitConverter.ToString).GetHashCode()

      type Term = 
        | Variable of char
        | Apply of Term * Term
        | Abstract of char * Term
        | Meta of option<string> * option<int>

        override x.ToString() =
          match x with
            | Variable c -> c.ToString()
            | Apply (l, Apply(l2, r2)) -> l.ToString() + "(" + Apply(l2, r2).ToString() +  ")"
            | Apply (l, r) -> l.ToString() + r.ToString()
            | Abstract (c, t) ->
              let rec getargs a =
                match a with
                  | Abstract(c1, Abstract(c2, t2)) ->
                    let (b1, b2) = getargs (Abstract(c2, t2)) in
                      (c1.ToString() + b1, b2)
                  | Abstract(c, t) -> (c.ToString(), t.ToString())
                  | _ -> raise (System.InvalidOperationException "will not thrown")
              in let (a, b) = getargs x in System.String.Format ("(λ{0}.{1})", a, b)
            | Meta (_, _) -> "{meta}"

      [<CustomEquality; NoComparison>]
      type TermI =
        | Indexed of int
        | Free of char
        | Apply of TermI * TermI
        | Abstract of TermI * option<char>

        override x.ToString() =
          match x with
            | Indexed i -> i.ToString()
            | Free c -> c.ToString()
            | Apply(l, Apply(l2, r2)) -> l.ToString() + "(" + Apply(l2, r2).ToString() +  ")"
            | Apply(l, r) -> l.ToString() + r.ToString()
            | Abstract (t, _) -> "(λ " + t.ToString() + ")"

        override x.GetHashCode() =
          match x with
            | Indexed i -> md5 i
            | Free c -> md5 (int c)
            | Apply(l, r) -> md5s (l.GetHashCode().ToString() + r.GetHashCode().ToString())
            | Abstract(b, _) -> b.GetHashCode() |> md5

        interface IEquatable<TermI> with
          member this.Equals t =
             match (t, this) with
              | (Apply(l, r), Apply(lt, rt)) -> l.Equals(lt) && r.Equals(rt)
              | (Indexed i, Indexed j) -> i = j
              | (Free n, Free m) -> n.Equals m
              | (Abstract(b, _), Abstract(c, _)) -> b.Equals c 
              | _ -> false
       
        override x.Equals y =
          match y with
            | :? TermI as t -> x.Equals t
            | _ -> false

        static member op_Equality(a : TermI, b : TermI) =
          a.Equals b
 
      let rec private FV (t : TermI) =
        match t with
          | Indexed _ -> []
          | Free c -> [c]
          | Apply(l, r) -> List.concat [ FV(l); FV(r) ]
          | Abstract(b, _) -> FV(b)
    
      let toTermI x =
        let rec toi x t =
          match x with 
            | Term.Abstract(a, b) ->
              let t = List.map (fun (x, y) -> (x, y + 1)) t in
              TermI.Abstract (toi b ((a, 0) :: t), Some a)
            | Term.Apply(l, r) ->
              TermI.Apply (toi l t, toi r t)
            | Term.Variable n ->
              match List.tryFind (fun (y, _) -> y.Equals n) t with
                | Some(x, y) -> TermI.Indexed y
                | None -> TermI.Free n
            | Term.Meta(a, b) -> raise (NotImplementedException "TODO")
        in toi x []

      let toTerm x =
        let ns = ref (Stack(names.Reverse())) in
        let rec tot x (s : Stack<char>) = 
          match x with
            | TermI.Abstract(b, h) ->
              let nsb = !ns in 
              let fv = FV b in
              let fvc a = Seq.contains a fv in
              let rec getv () =
                let a = nsb.Pop() in
                let f () = if fvc a then let a' = getv () in (nsb.Push a; a') else a in
                match h with
                  | Some(c) when (not (Seq.contains c nsb)) ->
                    f ()
                  | None ->
                    f ()
                  | Some(c) ->
                    nsb.Push a; ns := Stack(Seq.filter ((<>) c) nsb); c
              in 
              let n = getv () in s.Push n;
              let t = Term.Abstract (n, tot b s) in
              ignore (s.Pop()); nsb.Push n; t
            | Apply(l, r) ->
              Term.Apply(tot l s, tot r s)
            | Indexed i ->
              Term.Variable(s.ElementAt i)
            | Free n ->
              Term.Variable(n)
        in tot x (Stack())

    end