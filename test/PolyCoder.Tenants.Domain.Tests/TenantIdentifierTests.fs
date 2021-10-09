module PolyCoder.Tenants.Domain.TenantIdentifierTests

open AccidentalFish.FSharp.Validation
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open PolyCoder.Tenants.Domain.TenantIdentifier

let mockStrings = ValidatorsTests.mockStrings

(******************
 * Validate.identifier
 ******************)

[<TenantProperty>]
let validateIdentifierForValidInputShouldReturnOk (ValidTenantIdentifier identifier) =
  let validation = identifier |> Validate.identifier mockStrings "id"

  test <@ validation = Ok @>

[<Theory>]
[<InlineData(null)>]
let validateIdentifierForNullIdentifierShouldReturnErrors (identifier: TenantIdentifier) =
  let validation = identifier |> Validate.identifier mockStrings "id"

  let expectedError: ValidationItem = {
    errorCode = nameof(Validators.isNotEmptyOrWhiteSpace)
    property = "id"
    message = mockStrings.mustNotBeNull
  }

  test <@ validation = Errors [ expectedError ] @>

[<Theory>]
[<InlineData("")>]
let validateIdentifierForEmptyIdentifierShouldReturnErrors (identifier: TenantIdentifier) =
  let validation = identifier |> Validate.identifier mockStrings "id"

  let expectedErrors: ValidationItem list = [
    { errorCode = nameof(Validators.isNotEmptyOrWhiteSpace)
      property = "id"
      message = mockStrings.mustNotBeEmpty
    }
    { errorCode = nameof(Validators.matchesRegex)
      property = "id"
      message = mockStrings.mustBeAValidUrlSegment
    }
  ]

  test <@ validation = Errors expectedErrors @>

[<TenantProperty>]
let validateIdentifierForWhitespaceIdentifierShouldReturnErrors (WhiteSpaceString identifier) =
  let validation = identifier |> Validate.identifier mockStrings "id"

  let expectedErrors: ValidationItem list = [
    { errorCode = nameof(Validators.isNotEmptyOrWhiteSpace)
      property = "id"
      message = mockStrings.mustNotBeWhiteSpace
    }
    { errorCode = nameof(Validators.matchesRegex)
      property = "id"
      message = mockStrings.mustBeAValidUrlSegment
    }
  ]

  test <@ validation = Errors expectedErrors @>

[<TenantProperty>]
let validateIdentifierForUnmatchingIdentifierShouldReturnErrors (UnmatchingTenantIdentifier identifier) =
  let validation = identifier |> Validate.identifier mockStrings "id"

  let expectedErrors: ValidationItem list = [
    { errorCode = nameof(Validators.matchesRegex)
      property = "id"
      message = mockStrings.mustBeAValidUrlSegment
    }
  ]

  test <@ validation = Errors expectedErrors @>

(********************
 * Validate.command
 ********************)

[<Theory>]
[<InlineData("myid", true)>]
[<InlineData("my-identifier", true)>]
[<InlineData(null, false)>]
[<InlineData("", false)>]
[<InlineData(" \t\r\n", false)>]
[<InlineData("my identifier", false)>]
let validateCommandRequest (identifier: TenantIdentifier) (isValid: bool) =
  let command = Request { id = identifier }

  let validation = command |> Validate.command mockStrings

  if isValid
  then test <@ validation = Ok @>
  else test <@ validation <> Ok @>

[<Theory>]
[<InlineData("myid", true)>]
[<InlineData("my-identifier", true)>]
[<InlineData(null, false)>]
[<InlineData("", false)>]
[<InlineData(" \t\r\n", false)>]
[<InlineData("my identifier", false)>]
let validateCommandApprove (identifier: TenantIdentifier) (isValid: bool) =
  let command = Approve { id = identifier }

  let validation = command |> Validate.command mockStrings

  if isValid
  then test <@ validation = Ok @>
  else test <@ validation <> Ok @>

[<Theory>]
[<InlineData("myid", true)>]
[<InlineData("my-identifier", true)>]
[<InlineData(null, false)>]
[<InlineData("", false)>]
[<InlineData(" \t\r\n", false)>]
[<InlineData("my identifier", false)>]
let validateCommandReject (identifier: TenantIdentifier) (isValid: bool) =
  let command = Reject { id = identifier }

  let validation = command |> Validate.command mockStrings

  if isValid
  then test <@ validation = Ok @>
  else test <@ validation <> Ok @>

[<Fact>]
let validateCommandRetire () =
  let command = Retire

  let validation = command |> Validate.command mockStrings

  test <@ validation = Ok @>

(**************************
 * Aggregate.initialState
 **************************)

[<Fact>]
let aggregateInitialStateShouldReturnNonExisting () =
  let state = Aggregate.initialState
  let expectedState : Aggregate.State = { current = None; request = None }

  test <@ state = expectedState @>

(********************
 * Aggregate.apply
 ********************)

[<TenantProperty>]
let aggregateApplyWasRequestedToAnyStateShouldReturnAStateWithGivenRequestedId
  (state: Aggregate.State)
  (ValidTenantIdentifier rid) =
  let event = WasRequested { id = rid }
  let expectedState = { state with request = Some rid }

  let result = event |> Aggregate.apply state

  test <@ result = expectedState @>

[<TenantProperty>]
let aggregateApplyWasRejectedToAnyStateShouldReturnAStateWithoutPendingRequest
  (state: Aggregate.State) =
  let event = WasRejected
  let expectedState = { state with request = None }

  let result = event |> Aggregate.apply state

  test <@ result = expectedState @>

[<TenantProperty>]
let aggregateApplyWasApprovedToAnyStateShouldReturnAStateWithoutPendingRequestAndCurrentId
  (state: Aggregate.State)
  (ValidTenantIdentifier rid) =
  let event = WasApproved { id = rid }
  let expectedState : Aggregate.State = { request = None; current = Some rid }

  let result = event |> Aggregate.apply state

  test <@ result = expectedState @>

[<TenantProperty>]
let aggregateApplyWasRetiredToAnyStateShouldReturnTheInitialState
  (state: Aggregate.State) =
  let event = WasRetired
  let expectedState = Aggregate.initialState

  let result = event |> Aggregate.apply state

  test <@ result = expectedState @>

(*********************
 * Aggregate.handle
 *********************)

[<TenantProperty>]
let aggregateHandleWithInvalidCommandShouldReturnError
  (state: Aggregate.State)
  (UnmatchingTenantIdentifier rid) =
  let command = Request { id = rid }
  let expectedErrors : ValidationItem list = [
    { errorCode = nameof(Validators.matchesRegex)
      property = "id"
      message = mockStrings.mustBeAValidUrlSegment
    }
  ]

  let result = command |> Aggregate.handle mockStrings state

  test <@ result = Result.Error expectedErrors @>

/// Request

[<TenantProperty>]
let aggregateHandleRequestOnNonExistingTenantShouldReturnWasRequestedEvent
  (previousRequest: TenantIdentifier option)
  (ValidTenantIdentifier rid) =
  let state: Aggregate.State = { current = None; request = previousRequest }
  let command = Request { id = rid }
  let expectedEvents = [ WasRequested { id = rid } ]

  let result = command |> Aggregate.handle mockStrings state

  test <@ result = Result.Ok expectedEvents @>

[<TenantProperty>]
let aggregateHandleRequestOnExistingTenantShouldReturnWasRequestedEvent
  (ValidTenantIdentifier currentIdentifier)
  (ValidTenantIdentifier requestedIdentifier)
  (previousRequest: TenantIdentifier option) =
  currentIdentifier <> requestedIdentifier ==> lazy
    let state: Aggregate.State = { current = Some currentIdentifier; request = previousRequest }
    let command = Request { id = requestedIdentifier }
    let expectedEvents = [ WasRequested { id = requestedIdentifier } ]

    let result = command |> Aggregate.handle mockStrings state

    test <@ result = Result.Ok expectedEvents @>

[<TenantProperty>]
let aggregateHandleRequestOnSameTenantIdShouldReturnNoEvents
  (ValidTenantIdentifier currentIdentifier)
  (previousRequest: TenantIdentifier option) =
  let state: Aggregate.State = { current = Some currentIdentifier; request = previousRequest }
  let command = Request { id = currentIdentifier }
  let expectedEvents : Event list = []

  let result = command |> Aggregate.handle mockStrings state

  test <@ result = Result.Ok expectedEvents @>

/// Approve

[<TenantProperty>]
let aggregateHandleApproveOnSameRequestedIdShouldReturnWasApprovedEvent
  (currentState: TenantIdentifier option)
  (ValidTenantIdentifier requestedId) =
  let state: Aggregate.State = { current = currentState; request = Some requestedId }
  let command = Approve { id = requestedId }
  let expectedEvents = [ WasApproved { id = requestedId } ]

  let result = command |> Aggregate.handle mockStrings state

  test <@ result = Result.Ok expectedEvents @>

[<TenantProperty>]
let aggregateHandleApproveOnDistinctRequestedIdShouldReturnNoEvents
  (currentState: TenantIdentifier option)
  (ValidTenantIdentifier requestedId)
  (ValidTenantIdentifier approvedId) =
  requestedId <> approvedId ==> lazy
    let state: Aggregate.State = { current = currentState; request = Some requestedId }
    let command = Approve { id = approvedId }
    let expectedEvents : Event list = []

    let result = command |> Aggregate.handle mockStrings state

    test <@ result = Result.Ok expectedEvents @>

/// Reject

[<TenantProperty>]
let aggregateHandleRejectOnSameRequestedIdShouldReturnWasRejectedEvent
  (currentState: TenantIdentifier option)
  (ValidTenantIdentifier requestedId) =
  let state: Aggregate.State = { current = currentState; request = Some requestedId }
  let command = Reject { id = requestedId }
  let expectedEvents = [ WasRejected ]

  let result = command |> Aggregate.handle mockStrings state

  test <@ result = Result.Ok expectedEvents @>

[<TenantProperty>]
let aggregateHandleRejectOnDistinctRequestedIdShouldReturnNoEvents
  (currentState: TenantIdentifier option)
  (ValidTenantIdentifier requestedId)
  (ValidTenantIdentifier rejectedId) =
  requestedId <> rejectedId ==> lazy
    let state: Aggregate.State = { current = currentState; request = Some requestedId }
    let command = Reject { id = rejectedId }
    let expectedEvents : Event list = []

    let result = command |> Aggregate.handle mockStrings state

    test <@ result = Result.Ok expectedEvents @>

/// Reject

[<TenantProperty>]
let aggregateHandleRetireOnNonInitialStateShouldReturnWasRetiredEvent
  (state: Aggregate.State) =
  state <> Aggregate.initialState ==> lazy
    let command = Retire
    let expectedEvents = [ WasRetired ]

    let result = command |> Aggregate.handle mockStrings state

    test <@ result = Result.Ok expectedEvents @>

[<Fact>]
let aggregateHandleRetireOnInitialStateShouldReturnNoEvents () =
  let state = Aggregate.initialState
  let command = Retire
  let expectedEvents : Event list = []

  let result = command |> Aggregate.handle mockStrings state

  test <@ result = Result.Ok expectedEvents @>
