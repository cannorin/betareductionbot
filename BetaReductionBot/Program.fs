namespace BetaReductionBot
open BetaReductionBot.Ast
open BetaReductionBot.Transform
open BetaReductionBot.Parser
open System
open System.Linq
open System.IO
open System.Reflection
open System.Diagnostics
open System.Runtime.Serialization
open System.Xml
open System.Threading
open System.Drawing
open System.Drawing.Imaging
open CoreTweet
open CoreTweet.Streaming

  module Main =

    [<EntryPoint>]
    let main argv =
      let k = Term.Apply(Term.Abstract ('x', Term.Abstract('y', Term.Variable 'x')), Term.Variable 'a') in
      let tc = TermConverter None in
      let ki = tc.toTermI k in
      Console.Write "input:         ";
      Console.WriteLine k;
      Console.Write "to TermI:      ";
      Console.WriteLine ki;
      Console.Write "TermI to Term: ";
      Console.WriteLine (tc.toTerm ki);
      Console.Write "beta reduce:   ";
      Console.WriteLine (tc.toTerm (BetaReducer().reduce ki));
      ignore (Console.ReadLine());
      let x = MainParser().parse "(\xyz.xz(yz))" in
      Console.WriteLine x;
      let y = MainParser().parse "&y" in
      tc.toTermI y |> tc.toTerm |> Console.WriteLine
      0