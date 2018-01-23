module Lambda.Session
open Lambda.Parser
open Lambda.Ast
open Lambda.Eval
open Lambda.Utils
open Lambda.Exception

open ImageBuilder

open System
open System.Drawing
open System.Drawing.Text
open System.Drawing.Imaging

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control

let private sprintTermPart parent t =
  let fvs = fvOf parent in
  let rec dig cs = function
    | x when x = t -> [ termToString cs fvs x ]
    | TmApp (l, r) -> List.append (dig cs l) (dig cs r)
    | TmAbs (b, co) ->
      let rs = Set.unionMany [fvs; cs |> Set.ofList] in
      let c = 
        match co with
          | Some c when rs |> Set.contains c |> not -> c
          | _ ->
            AstChar.names |> Array.filter (fun c -> rs |> Set.contains c |> not)
                          |> Array.head
      in
      dig (c :: cs) b
    | _ -> []
  in
  dig [] parent |> List.tryHead

let rec private renderHist hist iro fo =
  let hs = hist |> List.map to_s in
  let fs = fo ?| (hs |> ImageHelper.estimateFontSize) in
  use font = ImageHelper.stixFont fs in
  let (ex, ey) = hs |> String.concat Environment.NewLine |> ImageHelper.measure font in

  if ex < 4000 && ey >= 4000 && ey / 4000 < 4 then
    let n = ey / 4000 + 1 in
    let len = List.length hist in
    let chunk = (len + (n - len % n)) / n in
    let imgs = hist |> List.takeToEnd chunk 
                    |> List.map (fun h -> renderHist h iro (Some fs))
                    |> List.choose (function | RenderedImages [x] -> Some x | _ -> None)
    in
    if List.length imgs = n then
      RenderedImages imgs
    else
      TooBig
  else if ex < 4000 && ey < 4000 then
    let lines =
      match iro with
        | None -> hist |> List.map (fun x -> [ (to_s x, Brushes.Black) ])
        | Some i ->
          hist |> List.map (fun x ->
                    match (sprintTermPart x i) with
                      | Some si ->
                        let sp = to_s x in
                        sp.Split([|si|], StringSplitOptions.None)
                          |> Array.map (fun x -> [| (si, Brushes.Red); (x, Brushes.Black) |])
                          |> Array.concat |> Array.skip 1 |> Array.toList
                      | None -> [ (to_s x, Brushes.Black) ]
                  )
    in
    let mutable ib = ImageBuilder.create 10 StringFormat.standard in
    for l in lines do
      ib <- ib |> iprintf font Brushes.Black "%s " AstChar.arrow
      for (s, b) in l do
        ib <- ib |> iprintf font b "%s" s;
      done
      ib <- ib |> iprintfn font Brushes.Black " "
    done
    let inline flatten x = match x with Some x -> x | None -> None in
    match iro |> Option.map (sprintTermPart (hist |> List.last)) |> flatten with
      | Some s ->
        ib <- ib |> iprintfn font Brushes.Black " "
                 |> iprintfn font Brushes.Red "[!] ... %s"  s
      | None -> ()
    ib |> ImageBuilder.render Color.White |> List.singleton |> RenderedImages
  else
    TooBig

[<Struct>]
type Session = 
  val dict: Map<string, Term>
  
  new (x: Map<string, Term> option) = 
    let dict = ref (x ?| Map.empty) in
    let dadd s x = 
      let e = parse x |> toUTerm [] !dict in
      dict := !dict |> Map.add s e
    in
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
    dadd "ack" "&fix (^fxy. &ifthenelse (&iszero x) (&succ y) (f (&pred x) (&ifthenelse (&iszero y) 1 (f x (&pred y)))))";
    dadd "fix" "&fix"
    { dict = !dict }
  
  member this.parse x =
    parse x |> toUTerm [] this.dict

  member this.eval x =
    let hist = ref HashedList.empty in
    let rec e x i =
      if i > 100000 then
        LambdaException ("Computation timeout", None, ErrorState.Unreducible) |> raise
      else
        try
          match (eval !hist x) with
            | Some x' ->
              hist := !hist |> HashedList.add x;
              e x' (i+1)
            | None ->
              hist := !hist |> HashedList.add x;
              (renderHist ((!hist).list |> List.rev) None None, Some x)
        with
          | InfLoopFound x' ->
            hist := !hist |> HashedList.add x;
            (renderHist ((!hist).list |> List.rev) (Some x') None, None)
    in
    e x 0

  member this.defMeta name e =
    if this.dict |> Map.containsKey name then
      failwith ""
    else
      Session (this.dict |> Map.add name e |> Some)
  
