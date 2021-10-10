module PolyCoder.Tenants.Domain.TenantIdentifierTests

open AccidentalFish.FSharp.Validation
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open PolyCoder.Tenants.Domain.TenantIdentifier

let strings = ValidatorsTests.mockStrings

module ValidateIdentifier =
  [<TenantProperty>]
  let withValidInputShouldReturnOk (ValidTenantIdentifier identifier) =
    let validation = identifier |> Validate.identifier strings "id"

    test <@ validation = Ok @>

  [<Theory>]
  [<InlineData(null)>]
  let withNullIdentifierShouldReturnErrors (identifier: TenantIdentifier) =
    let validation = identifier |> Validate.identifier strings "id"

    let expectedError: ValidationItem = {
      errorCode = nameof(Validators.isNotEmptyOrWhiteSpace)
      property = "id"
      message = strings.mustNotBeNull
    }

    test <@ validation = Errors [ expectedError ] @>

  [<Theory>]
  [<InlineData("")>]
  let withEmptyIdentifierShouldReturnErrors (identifier: TenantIdentifier) =
    let validation = identifier |> Validate.identifier strings "id"

    let expectedErrors: ValidationItem list = [
      { errorCode = nameof(Validators.isNotEmptyOrWhiteSpace)
        property = "id"
        message = strings.mustNotBeEmpty
      }
      { errorCode = nameof(Validators.matchesRegex)
        property = "id"
        message = strings.mustBeAValidUrlSegment
      }
    ]

    test <@ validation = Errors expectedErrors @>

  [<TenantProperty>]
  let withWhitespaceIdentifierShouldReturnErrors (WhiteSpaceString identifier) =
    let validation = identifier |> Validate.identifier strings "id"

    let expectedErrors: ValidationItem list = [
      { errorCode = nameof(Validators.isNotEmptyOrWhiteSpace)
        property = "id"
        message = strings.mustNotBeWhiteSpace
      }
      { errorCode = nameof(Validators.matchesRegex)
        property = "id"
        message = strings.mustBeAValidUrlSegment
      }
    ]

    test <@ validation = Errors expectedErrors @>

  [<TenantProperty>]
  let withUnmatchingIdentifierShouldReturnErrors (UnmatchingTenantIdentifier identifier) =
    let validation = identifier |> Validate.identifier strings "id"

    let expectedErrors: ValidationItem list = [
      { errorCode = nameof(Validators.matchesRegex)
        property = "id"
        message = strings.mustBeAValidUrlSegment
      }
    ]

    test <@ validation = Errors expectedErrors @>

module ValidateCommand =
  [<Theory>]
  [<InlineData("myid", true)>]
  [<InlineData("my-identifier", true)>]
  [<InlineData(null, false)>]
  [<InlineData("", false)>]
  [<InlineData(" \t\r\n", false)>]
  [<InlineData("my identifier", false)>]
  let requestValidations (identifier: TenantIdentifier) (isValid: bool) =
    let command = Request { id = identifier }

    let validation = command |> Validate.command strings

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
  let approveValidations (identifier: TenantIdentifier) (isValid: bool) =
    let command = Approve { id = identifier }

    let validation = command |> Validate.command strings

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
  let rejectValidations (identifier: TenantIdentifier) (isValid: bool) =
    let command = Reject { id = identifier }

    let validation = command |> Validate.command strings

    if isValid
    then test <@ validation = Ok @>
    else test <@ validation <> Ok @>

  [<Fact>]
  let retireValidations () =
    let command = Retire

    let validation = command |> Validate.command strings

    test <@ validation = Ok @>

module AggregateInitialState =
  [<Fact>]
  let itShouldReturnNonExisting () =
    let state = Aggregate.initialState
    let expectedState : Aggregate.State = { current = None; request = None }

    test <@ state = expectedState @>

module AggregateApply =
  [<TenantProperty>]
  let wasRequestedToAnyStateShouldReturnAStateWithGivenRequestedId
    (state: Aggregate.State)
    (ValidTenantIdentifier rid) =
    let event = WasRequested { id = rid }
    let expectedState = { state with request = Some rid }

    let result = event |> Aggregate.apply state

    test <@ result = expectedState @>

  [<TenantProperty>]
  let wasRejectedToAnyStateShouldReturnAStateWithoutPendingRequest
    (state: Aggregate.State) =
    let event = WasRejected
    let expectedState = { state with request = None }

    let result = event |> Aggregate.apply state

    test <@ result = expectedState @>

  [<TenantProperty>]
  let wasApprovedToAnyStateShouldReturnAStateWithoutPendingRequestAndCurrentId
    (state: Aggregate.State)
    (ValidTenantIdentifier rid) =
    let event = WasApproved { id = rid }
    let expectedState : Aggregate.State = { request = None; current = Some rid }

    let result = event |> Aggregate.apply state

    test <@ result = expectedState @>

  [<TenantProperty>]
  let wasRetiredToAnyStateShouldReturnTheInitialState
    (state: Aggregate.State) =
    let event = WasRetired
    let expectedState = Aggregate.initialState

    let result = event |> Aggregate.apply state

    test <@ result = expectedState @>

module AggregateHandle =
  [<TenantProperty>]
  let withInvalidCommandShouldReturnError
    (state: Aggregate.State)
    (UnmatchingTenantIdentifier rid) =
    let command = Request { id = rid }
    let expectedErrors : ValidationItem list = [
      { errorCode = nameof(Validators.matchesRegex)
        property = "id"
        message = strings.mustBeAValidUrlSegment
      }
    ]

    let result = command |> Aggregate.handle strings state

    test <@ result = Result.Error expectedErrors @>

  /// Request

  [<TenantProperty>]
  let requestOnNonExistingTenantShouldReturnWasRequestedEvent
    (previousRequest: TenantIdentifier option)
    (ValidTenantIdentifier rid) =
    let state: Aggregate.State = { current = None; request = previousRequest }
    let command = Request { id = rid }
    let expectedEvents = [ WasRequested { id = rid } ]

    let result = command |> Aggregate.handle strings state

    test <@ result = Result.Ok expectedEvents @>

  [<TenantProperty>]
  let requestOnExistingTenantShouldReturnWasRequestedEvent
    (ValidTenantIdentifier currentIdentifier)
    (ValidTenantIdentifier requestedIdentifier)
    (previousRequest: TenantIdentifier option) =
    currentIdentifier <> requestedIdentifier ==> lazy
      let state: Aggregate.State = { current = Some currentIdentifier; request = previousRequest }
      let command = Request { id = requestedIdentifier }
      let expectedEvents = [ WasRequested { id = requestedIdentifier } ]

      let result = command |> Aggregate.handle strings state

      test <@ result = Result.Ok expectedEvents @>

  [<TenantProperty>]
  let requestOnSameTenantIdShouldReturnNoEvents
    (ValidTenantIdentifier currentIdentifier)
    (previousRequest: TenantIdentifier option) =
    let state: Aggregate.State = { current = Some currentIdentifier; request = previousRequest }
    let command = Request { id = currentIdentifier }
    let expectedEvents : Event list = []

    let result = command |> Aggregate.handle strings state

    test <@ result = Result.Ok expectedEvents @>

  /// Approve

  [<TenantProperty>]
  let approveOnSameRequestedIdShouldReturnWasApprovedEvent
    (currentState: TenantIdentifier option)
    (ValidTenantIdentifier requestedId) =
    let state: Aggregate.State = { current = currentState; request = Some requestedId }
    let command = Approve { id = requestedId }
    let expectedEvents = [ WasApproved { id = requestedId } ]

    let result = command |> Aggregate.handle strings state

    test <@ result = Result.Ok expectedEvents @>

  [<TenantProperty>]
  let approveOnDistinctRequestedIdShouldReturnNoEvents
    (currentState: TenantIdentifier option)
    (ValidTenantIdentifier requestedId)
    (ValidTenantIdentifier approvedId) =
    requestedId <> approvedId ==> lazy
      let state: Aggregate.State = { current = currentState; request = Some requestedId }
      let command = Approve { id = approvedId }
      let expectedEvents : Event list = []

      let result = command |> Aggregate.handle strings state

      test <@ result = Result.Ok expectedEvents @>

  /// Reject

  [<TenantProperty>]
  let rejectOnSameRequestedIdShouldReturnWasRejectedEvent
    (currentState: TenantIdentifier option)
    (ValidTenantIdentifier requestedId) =
    let state: Aggregate.State = { current = currentState; request = Some requestedId }
    let command = Reject { id = requestedId }
    let expectedEvents = [ WasRejected ]

    let result = command |> Aggregate.handle strings state

    test <@ result = Result.Ok expectedEvents @>

  [<TenantProperty>]
  let rejectOnDistinctRequestedIdShouldReturnNoEvents
    (currentState: TenantIdentifier option)
    (ValidTenantIdentifier requestedId)
    (ValidTenantIdentifier rejectedId) =
    requestedId <> rejectedId ==> lazy
      let state: Aggregate.State = { current = currentState; request = Some requestedId }
      let command = Reject { id = rejectedId }
      let expectedEvents : Event list = []

      let result = command |> Aggregate.handle strings state

      test <@ result = Result.Ok expectedEvents @>

  /// Reject

  [<TenantProperty>]
  let retireOnNonInitialStateShouldReturnWasRetiredEvent
    (state: Aggregate.State) =
    state <> Aggregate.initialState ==> lazy
      let command = Retire
      let expectedEvents = [ WasRetired ]

      let result = command |> Aggregate.handle strings state

      test <@ result = Result.Ok expectedEvents @>

  [<Fact>]
  let retireOnInitialStateShouldReturnNoEvents () =
    let state = Aggregate.initialState
    let command = Retire
    let expectedEvents : Event list = []

    let result = command |> Aggregate.handle strings state

    test <@ result = Result.Ok expectedEvents @>
