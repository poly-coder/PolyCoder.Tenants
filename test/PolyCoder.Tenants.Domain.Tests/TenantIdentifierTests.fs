module PolyCoder.Tenants.Domain.TenantIdentifierTests

open AccidentalFish.FSharp.Validation
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

let mockStrings = ValidatorsTests.mockStrings

(******************
 * Validate.identifier
 ******************)

[<Theory>]
[<InlineData("a")>]
[<InlineData("abc")>]
[<InlineData("abc-def")>]
[<InlineData("abc-def-gh1")>]
let validateIdentifierForValidInputShouldReturnOk (identifier: string) =
  let validation = identifier |> TenantIdentifier.Validate.identifier mockStrings

  test <@ validation = Ok @>

[<Theory>]
[<InlineData(null)>]
let validateIdentifierForNullIdentifierShouldReturnErrors (identifier: string) =
  let validation = identifier |> TenantIdentifier.Validate.identifier mockStrings

  let expectedError: ValidationItem = {
    errorCode = "isNotEmptyOrWhiteSpace"
    property = "identifier"
    message = mockStrings.mustNotBeNull
  }

  test <@ validation = Errors [ expectedError ] @>

[<Theory>]
[<InlineData("")>]
let validateIdentifierForEmptyIdentifierShouldReturnErrors (identifier: string) =
  let validation = identifier |> TenantIdentifier.Validate.identifier mockStrings

  let expectedErrors: ValidationItem list = [
    { errorCode = "isNotEmptyOrWhiteSpace"
      property = "identifier"
      message = mockStrings.mustNotBeEmpty
    }
    { errorCode = "matchesRegex"
      property = "identifier"
      message = mockStrings.mustBeAValidUrlSegment
    }
  ]

  test <@ validation = Errors expectedErrors @>

[<Theory>]
[<InlineData(" ")>]
[<InlineData("\t")>]
[<InlineData("\r")>]
[<InlineData("\n")>]
[<InlineData("\r\n\t\f ")>]
let validateIdentifierForWhitespaceIdentifierShouldReturnErrors (identifier: string) =
  let validation = identifier |> TenantIdentifier.Validate.identifier mockStrings

  let expectedErrors: ValidationItem list = [
    { errorCode = "isNotEmptyOrWhiteSpace"
      property = "identifier"
      message = mockStrings.mustNotBeWhiteSpace
    }
    { errorCode = "matchesRegex"
      property = "identifier"
      message = mockStrings.mustBeAValidUrlSegment
    }
  ]

  test <@ validation = Errors expectedErrors @>

[<Theory>]
[<InlineData("with spaces")>]
[<InlineData("With-Upper-Case")>]
[<InlineData("with-$ymbols-...")>]
[<InlineData("with-0number-1first")>]
[<InlineData("  with-initial-spaces")>]
[<InlineData("with-trailling-spaces  ")>]
let validateIdentifierForUnmatchingIdentifierShouldReturnErrors (identifier: string) =
  let validation = identifier |> TenantIdentifier.Validate.identifier mockStrings

  let expectedErrors: ValidationItem list = [
    { errorCode = "matchesRegex"
      property = "identifier"
      message = mockStrings.mustBeAValidUrlSegment
    }
  ]

  test <@ validation = Errors expectedErrors @>

(********************
 * Validate.command
 ********************)

[<Theory>]
[<InlineData("my-identifier")>]
let validateCommandForValidRequestShouldReturnOk (identifier: string) =
  let command = TenantIdentifier.Request identifier

  let validation = command |> TenantIdentifier.Validate.command mockStrings

  test <@ validation = Ok @>

[<Theory>]
[<InlineData("my identifier")>]
let validateCommandForInvalidRequestShouldReturnError (identifier: string) =
  let command = TenantIdentifier.Request identifier

  let validation = command |> TenantIdentifier.Validate.command mockStrings

  test <@ validation <> Ok @>

[<Fact>]
let validateCommandForRejectShouldReturnOk () =
  let command = TenantIdentifier.Reject

  let validation = command |> TenantIdentifier.Validate.command mockStrings

  test <@ validation = Ok @>

[<Fact>]
let validateCommandForApproveShouldReturnOk () =
  let command = TenantIdentifier.Approve

  let validation = command |> TenantIdentifier.Validate.command mockStrings

  test <@ validation = Ok @>

[<Fact>]
let validateCommandForRetireShouldReturnOk () =
  let command = TenantIdentifier.Retire

  let validation = command |> TenantIdentifier.Validate.command mockStrings

  test <@ validation = Ok @>

(**************************
 * Aggregate.initialState
 **************************)

[<Fact>]
let aggregateInitialStateShouldReturnNonExisting () =
  let state = TenantIdentifier.Aggregate.initialState

  test <@ state = TenantIdentifier.Aggregate.NonExisting @>

(********************
 * Aggregate.apply
 ********************)

[<Fact>]
let aggregateApplyWasRequestedToNonExistingShouldReturnRequestingFirst () =
  let state = TenantIdentifier.Aggregate.NonExisting
  let event = TenantIdentifier.WasRequested "my-identifier"
  let expectedState = TenantIdentifier.Aggregate.RequestingFirst "my-identifier"

  let result = event |> TenantIdentifier.Aggregate.apply state

  test <@ result = expectedState @>
