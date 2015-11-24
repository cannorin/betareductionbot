namespace BetaReductionBot
open BetaReductionBot.Ast
open BetaReductionBot.Transform
open System

  module Main = 
    [<EntryPoint>]
    let main argv =
      let k = Term.Apply(Term.Abstract ('x', Term.Abstract('y', Term.Variable 'x')), Term.Variable 'a') in
      let ki = toTermI k in
      Console.Write "input:         ";
      Console.WriteLine k;
      Console.Write "to TermI:      ";
      Console.WriteLine ki;
      Console.Write "TermI to Term: ";
      Console.WriteLine (toTerm ki);
      Console.Write "beta reduce:   ";
      Console.WriteLine (toTerm (BetaReducer().Reduce ki))
      ignore (Console.ReadLine());
      0