namespace BetaReductionBot
open System
open BetaReductionBot.Ast
open BetaReductionBot.Exception
open ScanRat

module Parser =
  begin
    
    type MainParser () =
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
      let name = oneOf (String.Concat(List.concat[['a'..'z'];['A'..'Z']]))
      let num =
        tfold_d "digits" (oneOf "0123456789" --> fun d -> int(d) - int('0')) (fun (a, b) -> a * 10 + b)
      
      let parstart = ss ~~"(" --> ignore
      let parend = ss ~~")" --> ignore
      let sep = ss ~~"." --> ignore
      let meta = ~~"&" --> ignore

      let term =
        let toAbs cs t = 
          Seq.fold (fun t x -> Term.Abstract(x, t)) (Term.Abstract(Seq.head cs, t)) (Seq.skip 1 cs)
        let toApp ts = 
          System.Linq.Enumerable.Aggregate (ts, fun a b -> Term.Apply(a, b))
        
        let term : ScanRatCombinators.Parser<Term> = production "term"
        let var = ss name --> fun x -> Term.Variable x
        let abs = ((parstart + prefix) +. (((tfold_d "args" (name --> fun x -> x.ToString()) add) --> fun x -> x.ToCharArray() |> Seq.rev) .+ sep)) + term .+ parend --> fun(x, y) -> toAbs x y
        let meta = (spaces + meta) +. (tfold_d "name" (name --> fun x -> x.ToString()) add) .+ spaces --> fun x -> Term.Meta(Some x, None)
        let numt = spaces +. num .+ spaces --> fun x -> Term.Meta(None, Some x)
        let apply : ScanRatCombinators.Parser<Term> = production "apply"
        let tWithoutApply = spaces +. (var |- abs |- numt |-  meta |- (parstart +. apply .+ parend)) .+ spaces
        apply.rule <- tfold_d "apply2" (tWithoutApply --> fun x -> [x]) (fun(x, y) -> List.concat[x;y]) --> toApp
        term.rule <- spaces +. (apply |- tWithoutApply) .+ spaces
        term

      member this.parse s =
        match parse term s with
          | Success s -> s.value
          | Failure f -> BetaReducerException ("Wrong syntax", None, ErrorState.WrongSyntax) |> raise

  end
