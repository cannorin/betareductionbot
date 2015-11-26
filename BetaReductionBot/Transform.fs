namespace BetaReductionBot
open BetaReductionBot.Ast
open BetaReductionBot.Exception
open BetaReductionBot.Utils
open System.Collections.Generic
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

      member this.Reduce t =
        ct := t;
        match doReduce t 0 with
            | []     -> 
                ignore (hist.Add t);
                t
            | x :: _ -> 
                ignore (hist.Add t);
                this.Reduce x

      member this.ToBitmap () =
        ImageBuilder.build (Seq.map Ast.toTerm hist |> Seq.mapi (fun i x -> if i > 0 then Ast.arrow + x.ToString() else x.ToString()) |> Enumerable.ToArray) (Some 15) (Some 2)

  end

