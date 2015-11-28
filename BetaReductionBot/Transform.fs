namespace BetaReductionBot
open BetaReductionBot.Ast
open BetaReductionBot.Exception
open BetaReductionBot.Utils
open BetaReductionBot.Parser
open System.Collections.Generic
open System
open System.Linq

module Transform =
  begin

    type BetaReducer () =
      let hist = HashSet<TermI>()
      let ct = ref (TermI.Free 'x')

      let rec shift t c d =
        match t with
          | Free _ -> t
          | Indexed(k)  -> TermI.Indexed(if k < c then k else k + d)
          | Abstract(b, h) -> TermI.Abstract((shift b (c + 1) d), h)
          | Apply(l, r) -> TermI.Apply(shift l c d, shift r c d)

      let rec replace t1 t2 i =
        match t1 with
          | Free _ -> t1
          | Indexed(x)  -> if x = i then t2 else t1                
          | Apply(l, r) -> TermI.Apply(replace l t2 i, replace r t2 i)
          | Abstract(b, h) -> TermI.Abstract(replace b (shift t2 0 1) (i + 1), h)

      let rec doReduce t i =
        if i > 100000 then raise (BetaReducerException ("Expression is too big", None, ErrorState.Unreducible))

        else if hist.Contains t then
          let ctb = !ct in
          let e = List.concat [Seq.toList hist; (if t = ctb then [t] else [ctb; t])] in
          let image = ImageBuilder.build (e |> List.mapi (fun i x -> if i > 0 then Ast.arrow + x.ToString() else x.ToString()) |> Enumerable.ToArray) None None in
          raise (BetaReducerException("Infinite reduction", Some(image), ErrorState.InfiniteReduction))
        
        else 
          match t with
            | Free _ | Indexed _ -> []
            | Abstract(b, h) -> List.map (fun x -> TermI.Abstract (x, h)) (doReduce b (i + 1))
            | Apply(l, r) ->
              let a = 
                match l with
                  | Abstract(b, _) -> 
                    [ shift (replace b (shift r 0 1) 0) 0 -1 ]
                  | _ -> []
              in List.concat [a; List.map (fun x -> TermI.Apply (x, r)) (doReduce l (i + 1)); List.map (fun x -> TermI.Apply (x, r)) (doReduce r (i + 1));]

      member this.reduce t =
        ct := t;
        match doReduce t 0 with
            | []     -> 
                ignore (hist.Add t);
                t
            | x :: _ -> 
                ignore (hist.Add t);
                this.reduce x

      member this.toBitmap () =
        ImageBuilder.build (Seq.map Ast.toTerm hist |> Seq.mapi (fun i x -> if i > 0 then Ast.arrow + x.ToString() else x.ToString()) |> Enumerable.ToArray) (Some 15) (Some 2)

    type TermConverter (x : option<Dictionary<string, TermI>>) =
      let dict = 
        match x with
          | Some d -> Dictionary(d)
          | None -> Dictionary()
      do
        let p = MainParser() in
        let dadd s x = dict.[s] <- Ast.toTermI (p.parse x) in
        let s = "(^xyz.xz(yz))" in
        dadd "s" s; dadd "S" s;
        let k = "(^xy.x)" in
        dadd "k" k; dadd "K" k;
        let i = "(^x.x)" in
        dadd "i" i; dadd "I" i;
        let b = "(^xyz.x(yz))" in
        dadd "b" b; dadd "B" b;
        let c = "(^xyz.xzy)" in
        dadd "c" c; dadd "C" c;
        let w = "(^xy.xyy)" in 
        dadd "w" w; dadd "W" w;
        let y = "(^f.(^x.f(xx))(^x.f(xx)))" in
        dadd "y" y; dadd "Y" y;
        let z = "(^f.(^x.f(^y.xxy))(^x.f(^y.xxy)))" in
        dadd "z" z; dadd "Z" z;
        dadd "succ" "(^nfx.f(nfx))";
        dadd "plus" "(^mnfx.mf(nfx))";
        dadd "mult" "(^mnf.m(nf))";
        dadd "pow" "(^xy.yx)";
        dadd "pred" "(^nfx.n(^gh.h(gf))(^u.x)(^u.u))";
        dadd "true" "(^xy.x)";
        let f = "(^xy.y)" in
        dadd "false" f; dadd "nil" f;
        dadd "and" "(^pq.pq(^xy.y))";
        dadd "or" "(^pq.p(^xy.x)q)";
        dadd "not" "(^p.p(^xy.y)(^xy.x))";
        dadd "ifthenelse" "(^pxy.pxy)";
        dadd "iszero" "(^n.n(^x.(^pq.q))(^pq.p))";
        dadd "cons" "(^sbf.fsb)";
        dadd "car" "(^p.p(^xy.x))";
        dadd "cdr" "(^p.p(^xy.y))";
        dadd "isnil" "(^l.l(^htd.(^xy.y))(^xy.x))";
      
      member this.toTermI (x : Term) =
        match x with
          | Meta(Some s, None) -> 
            if dict.ContainsKey s
            then dict.[s] 
            else BetaReducerException (String.Format("Meta variable '{0}' is not defined", s), None, ErrorState.MetaVariableFailed) |> raise
          | Meta(None, Some i) -> 
            let rec c i =
              if i = 0 then
                Term.Variable('x')
              else
                Term.Apply(Term.Variable('f'), c(i - 1))
            in
            Term.Abstract('f', Term.Abstract('x', c i)) |> Ast.toTermI
          | _ -> Ast.toTermI x

      member this.toTerm x = Ast.toTerm x

      member this.addMetaVariable s t =
        if dict.ContainsKey s
        then BetaReducerException (String.Format("Meta variable '{0}' is already defined", s), None, ErrorState.MetaVariableFailed) |> raise
        else dict.[s] <- t
      
      member this.removeMetaVariable s =
        if dict.ContainsKey s
        then dict.Remove s
        else BetaReducerException (String.Format("Meta variable '{0}' is not defined", s), None, ErrorState.MetaVariableFailed) |> raise 
  end

