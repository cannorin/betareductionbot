namespace BetaReductionBot
open System.Drawing
open System
open BetaReductionBot.Utils

module Exception =
  begin
    
    type ErrorState =
      | TooManyVariables
      | Unreducible
      | InfiniteReduction
      | WrongSyntax
      | MetaVariableFailed

    type BetaReducerException(s : string, b : option<RenderResult>, es : ErrorState) =
      inherit Exception(s)
      let img = b
      let state = es
      member this.ErrorImage with get() = img
      member this.ErrorState with get() = state

  end
