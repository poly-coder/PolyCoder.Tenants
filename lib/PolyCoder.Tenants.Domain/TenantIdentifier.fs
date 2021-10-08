[<RequireQualifiedAccess>]
module PolyCoder.Tenants.Domain.TenantIdentifier

type Event =
  | WasRequested of identifier: string
  | WasRejected of identifier: string
  | WasApproved of identifier: string
  | WasRetired

type Command =
  | Request of identifier: string
  | Reject
  | Approve
  | Retire

module Validate =
  open AccidentalFish.FSharp.Validation

  let identifier strings =
    Validators.createPropertyValidator "identifier" [
      Validators.isNotEmptyOrWhiteSpace strings
      Validators.matchesPatternWithMessage
        (fun _ -> strings.mustBeAValidUrlSegment)
        @"^([a-z][a-z0-9]*)(\-([a-z][a-z0-9]*))*$"
    ]

  let command strings = function
    | Request id ->
      identifier strings id
    | Reject -> Ok
    | Approve -> Ok
    | Retire -> Ok

module Aggregate =
  open AccidentalFish.FSharp.Validation

  type State =
    | NonExisting
    | RequestingFirst of requestedIdentifier: string
    | RequestingOther of requestedIdentifier: string * currentIdentifier: string
    | Active of currentIdentifier: string
    | Retired

  let initialState = NonExisting

  let apply state event =
    match event, state with
    // WasRequested
    | WasRequested requestedIdentifier, NonExisting
    | WasRequested requestedIdentifier, Retired ->
      RequestingFirst requestedIdentifier

    | WasRequested requestedIdentifier, Active currentIdentifier ->
      RequestingOther(requestedIdentifier, currentIdentifier)

    | WasRequested requestedIdentifier, RequestingFirst _ ->
      RequestingFirst requestedIdentifier

    | WasRequested requestedIdentifier, RequestingOther(_, currentIdentifier) ->
      RequestingOther(requestedIdentifier, currentIdentifier)

    | WasRequested _, _ -> state

    // WasRejected
    | WasRejected identifier, RequestingFirst requestedIdentifier when requestedIdentifier = identifier ->
      NonExisting

    | WasRejected identifier, RequestingOther(requestedIdentifier, currentIdentifier) when requestedIdentifier = identifier ->
      Active currentIdentifier

    | WasRejected _, _ -> state

    // WasApproved
    | WasApproved identifier, RequestingFirst requestedIdentifier when requestedIdentifier = identifier ->
      Active requestedIdentifier

    | WasApproved identifier, RequestingOther(requestedIdentifier, currentIdentifier) when requestedIdentifier = identifier ->
      Active currentIdentifier

    | WasApproved _, _ -> state

    // WasRetired
    | WasRetired, _ -> Retired
