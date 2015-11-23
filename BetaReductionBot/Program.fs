namespace BetaReductionBot
open BetaReductionBot.Ast
open System

  module Main = 
    [<EntryPoint>]
    let main argv =
      let k = Term.Abstract ('x', Term.Abstract('y', Term.Variable 'x')) in
      let ki = toTermI k in
      Console.WriteLine k;
      Console.WriteLine ki;
      Console.WriteLine (toTerm ki);
      ignore (Console.ReadLine());
      0
