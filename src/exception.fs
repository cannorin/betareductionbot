module Lambda.Exception
open Lambda.Utils

type ErrorState =
  | TooManyVariables
  | Unreducible
  | InfiniteReduction
  | WrongSyntax
  | MetaVariableFailed

exception LambdaException of string * RenderResult option * ErrorState

