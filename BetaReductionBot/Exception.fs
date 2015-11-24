namespace BetaReductionBot
open System.Drawing
open System


module Exception =
  begin
    
    type ErrorState =
      | TooManyVariables
      | Unreducible
      | InfiniteReduction
      | WrongSyntax

    type BetaReducerException(s : string, b : option<Bitmap>, es : ErrorState) =
      inherit Exception(s)
      let img = b
      let state = es
      member this.ErrorImage with get() = img
      member this.ErrorState with get() = state

  end