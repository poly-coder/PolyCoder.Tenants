module PolyCoder.Tenants.Domain.TenantIdentifier

type TenantIdentifier = string

type Event =
  | WasRequested of WasRequestedData
  | WasApproved of WasApprovedData
  | WasRejected
  | WasRetired

and WasRequestedData = { id: TenantIdentifier }
and WasApprovedData = { id: TenantIdentifier }

type Command =
  | Request of RequestData
  | Approve of ApproveData
  | Reject of RejectData
  | Retire

and RequestData = { id: TenantIdentifier }
and ApproveData = { id: TenantIdentifier }
and RejectData = { id: TenantIdentifier }

module Validate =
  open AccidentalFish.FSharp.Validation

  let identifier strings propertyName =
    Validators.createPropertyValidator
      propertyName
      [ Validators.isNotEmptyOrWhiteSpace strings
        Validators.matchesPatternWithMessage
          (fun _ -> strings.mustBeAValidUrlSegment)
          @"^([a-z][a-z0-9]*)(\-([a-z][a-z0-9]*))*$" ]

  let requestData strings =
    createValidatorFor<RequestData> () { validate (fun d -> d.id) [ identifier strings ] }

  let rejectData strings =
    createValidatorFor<RejectData> () { validate (fun d -> d.id) [ identifier strings ] }

  let approveData strings =
    createValidatorFor<ApproveData> () { validate (fun d -> d.id) [ identifier strings ] }

  let command strings =
    function
    | Request data -> requestData strings data
    | Reject data -> rejectData strings data
    | Approve data -> approveData strings data
    | Retire -> Ok

module Aggregate =
  open AccidentalFish.FSharp.Validation

  type State =
    { current: TenantIdentifier option
      request: TenantIdentifier option }

  let initialState = { current = None; request = None }

  let apply state event =
    match event, state with
    | WasRequested { id = rid }, _ -> { state with request = Some rid }

    | WasRejected, _ -> { state with request = None }

    | WasApproved { id = id }, _ -> { current = Some id; request = None }

    | WasRetired, _ -> initialState

  let handle strings (state: State) (command: Command) =
    match Validate.command strings command with
    | Errors es -> Error es

    | Ok ->
      match command, state with
      | Request { id = rid }, { current = None } -> Result.Ok [ WasRequested { id = rid } ]
      | Request { id = rid }, { current = Some cid } when rid <> cid -> Result.Ok [ WasRequested { id = rid } ]
      | Request _, _ -> Result.Ok []

      | Approve { id = id }, { request = Some rid } when id = rid -> Result.Ok [ WasApproved { id = id } ]
      | Approve _, _ -> Result.Ok []

      | Reject { id = id }, { request = Some rid } when id = rid -> Result.Ok [ WasRejected ]
      | Reject _, _ -> Result.Ok []

      | Retire, state when state <> initialState -> Result.Ok [ WasRetired ]
      | Retire, _ -> Result.Ok []
