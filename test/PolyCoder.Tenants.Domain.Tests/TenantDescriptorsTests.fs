module PolyCoder.Tenants.Domain.TenantDescriptorsTests

open AccidentalFish.FSharp.Validation
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

let mockStrings = ValidatorsTests.mockStrings

(*********
 * Data
 *********)

[<Fact>]
let dataEqualsOnTwoSimilarReturnsTrue() =
  let data1 : TenantDescriptors.Data = { title = "A"; description = "A" }
  let data2 : TenantDescriptors.Data = { title = "A"; description = "A" }

  test <@ data1.Equals(data2) @>

[<Fact>]
let dataEqualsOnTwoDistinctReturnsFalse() =
  let data1 : TenantDescriptors.Data = { title = "A"; description = "A" }
  let data2 : TenantDescriptors.Data = { title = "B"; description = "B" }

  test <@ data1.Equals(data2) |> not @>

[<Fact>]
let dataGetHashCodeReturns0() =
  let data1 : TenantDescriptors.Data = { title = "A"; description = "A" }

  test <@ data1.GetHashCode() <> 0 @>

[<Fact>]
let dataCompareOnTwoSortedReturnsPositive() =
  let data1 : TenantDescriptors.Data = { title = "A"; description = "A" }
  let data2 : TenantDescriptors.Data = { title = "B"; description = "B" }

  test <@ data1 < data2 @>

(******************
 * Validate.data
 ******************)

[<Theory>]
[<InlineData("my title", null)>]
[<InlineData("my title", "")>]
[<InlineData("my title", "Some description")>]
let validateDataForValidInputShouldReturnOk (title: string) (description: string) =
  let data : TenantDescriptors.Data =
    { title = title; description = description }

  let validation = data |> TenantDescriptors.Validate.data mockStrings

  test <@ validation = Ok @>

[<Fact>]
let validateDataForNullTitleShouldReturnError () =
  let title: string = null
  let description = "Some description"

  let data : TenantDescriptors.Data =
    { title = title; description = description }

  let validation = data |> TenantDescriptors.Validate.data mockStrings

  let expectedError: ValidationItem = {
    errorCode = "isNotEmptyOrWhiteSpace"
    property = "title"
    message = mockStrings.mustNotBeNull
  }

  test <@ validation = Errors [ expectedError ] @>

[<Fact>]
let validateDataForEmptyTitleShouldReturnError () =
  let title: string = ""
  let description = "Some description"

  let data : TenantDescriptors.Data =
    { title = title; description = description }

  let validation = data |> TenantDescriptors.Validate.data mockStrings

  let expectedError1: ValidationItem = {
    errorCode = "isNotEmptyOrWhiteSpace"
    property = "title"
    message = mockStrings.mustNotBeEmpty
  }

  let expectedError2: ValidationItem = {
    errorCode = "isNotShorterThan"
    property = "title"
    message = mockStrings.mustNotBeShorterThan 5
  }

  test <@ validation = Errors [ expectedError1; expectedError2 ] @>

[<Fact>]
let validateDataForShortTitleShouldReturnError () =
  let title: string = "abc"
  let description = "Some description"

  let data : TenantDescriptors.Data =
    { title = title; description = description }

  let validation = data |> TenantDescriptors.Validate.data mockStrings

  let expectedError: ValidationItem = {
    errorCode = "isNotShorterThan"
    property = "title"
    message = mockStrings.mustNotBeShorterThan 5
  }

  test <@ validation = Errors [ expectedError ] @>

[<Fact>]
let validateDataForLongTitleShouldReturnError () =
  let title: string = String.init 81 (fun i -> "T")
  let description = "Some description"

  let data : TenantDescriptors.Data =
    { title = title; description = description }

  let validation = data |> TenantDescriptors.Validate.data mockStrings

  let expectedError: ValidationItem = {
    errorCode = "isNotLongerThan"
    property = "title"
    message = mockStrings.mustNotBeLongerThan 80
  }

  test <@ validation = Errors [ expectedError ] @>

[<Fact>]
let validateDataForLongDescriptionShouldReturnError () =
  let title: string = "My title"
  let description = String.init 1001 (fun i -> "D")

  let data : TenantDescriptors.Data =
    { title = title; description = description }

  let validation = data |> TenantDescriptors.Validate.data mockStrings

  let expectedError: ValidationItem = {
    errorCode = "isNotLongerThan"
    property = "description"
    message = mockStrings.mustNotBeLongerThan 1000
  }

  test <@ validation = Errors [ expectedError ] @>

(***************
 * Validate.command
 ***************)

[<Theory>]
[<InlineData("my title", null)>]
[<InlineData("my title", "")>]
[<InlineData("my title", "Some description")>]
let validateCommandForValidUpdateShouldReturnOk (title: string) (description: string) =
  let data : TenantDescriptors.Data =
    { title = title; description = description }

  let command = TenantDescriptors.Update data

  let validation = command |> TenantDescriptors.Validate.command mockStrings

  test <@ validation = Ok @>

[<Theory>]
[<InlineData(null, null)>]
[<InlineData("", "")>]
[<InlineData("   ", "Some description")>]
let validateCommandForInvalidUpdateShouldReturnError (title: string) (description: string) =
  let data : TenantDescriptors.Data =
    { title = title; description = description }

  let command = TenantDescriptors.Update data

  let validation = command |> TenantDescriptors.Validate.command mockStrings

  test <@ validation <> Ok @>

(***************
 * Aggregate.initialState
 ***************)

[<Fact>]
let aggregateInitialStateShouldReturnEmptyState() =
  let state = TenantDescriptors.Aggregate.initialState

  let expected : TenantDescriptors.Aggregate.State = { title = ""; description = "" }

  test <@ state = expected @>

(***************
 * Aggregate.apply
 ***************)

[<Property>]
let aggregateApplyWasUpdatedShouldReturnModifiedState (NonEmptyString title) (NonEmptyString description) =
  let state = TenantDescriptors.Aggregate.initialState

  let event = TenantDescriptors.WasUpdated { title = title; description = description }

  let newState = event |> TenantDescriptors.Aggregate.apply state

  let expected : TenantDescriptors.Aggregate.State = { title = title; description = description }

  test <@ newState = expected @>

(***************
 * Aggregate.fold
 ***************)

[<Fact>]
let aggregateFoldShouldReturnModifiedState () =
  let state = TenantDescriptors.Aggregate.initialState

  let events = [
    TenantDescriptors.WasUpdated { title = "A"; description = "a" }
    TenantDescriptors.WasUpdated { title = "B"; description = "b" }
    TenantDescriptors.WasUpdated { title = "C"; description = "c" }
  ]

  let newState = events |> TenantDescriptors.Aggregate.fold state

  let expected : TenantDescriptors.Aggregate.State = { title = "C"; description = "c" }

  test <@ newState = expected @>

(***************
 * Aggregate.handle
 ***************)

[<Fact>]
let aggregateHandleWithSameDataShouldReturnEmptyEvents () =
  let state: TenantDescriptors.Aggregate.State =
    { title = "My title"; description = "My description" }

  let command = TenantDescriptors.Update { title = "My title"; description = "My description" }

  let result = command |> TenantDescriptors.Aggregate.handle mockStrings state

  test <@ result = Result.Ok [] @>

[<Fact>]
let aggregateHandleWithNewDataShouldReturnWasUpdatedEvent () =
  let state: TenantDescriptors.Aggregate.State =
    { title = "My title"; description = "My description" }

  let command = TenantDescriptors.Update { title = "New title"; description = "New description" }

  let result = command |> TenantDescriptors.Aggregate.handle mockStrings state

  let expected: Result<_, ValidationItem list> = Result.Ok [
    TenantDescriptors.WasUpdated { title = "New title"; description = "New description" }
  ]

  test <@ result = expected @>

[<Fact>]
let aggregateHandleWithInvalidDataShouldReturnErrors () =
  let state: TenantDescriptors.Aggregate.State =
    { title = "My title"; description = "My description" }

  let command = TenantDescriptors.Update { title = ""; description = "" }

  let result = command |> TenantDescriptors.Aggregate.handle mockStrings state

  let hasErrors count = function
    | Result.Ok (_: TenantDescriptors.Event list) -> false
    | Result.Error (es: ValidationItem list) -> List.length es = count

  test <@  result |> hasErrors 2 @>

(*******************
 * ListView.empty
 *******************)

[<Fact>]
let aggregateListViewEmptyItemIsEmpty () =
  let item = TenantDescriptors.ListView.empty

  let expected: TenantDescriptors.ListView.Item = { title = "" }

  test <@ item = expected @>

(*******************
 * ListView.project
 *******************)

[<Fact>]
let aggregateListViewProjectWasUpdatedOnItemShouldReturnUpdateItemViewCommand () =
  let item = TenantDescriptors.ListView.empty

  let event = TenantDescriptors.WasUpdated { title = "My title"; description = "My description" }

  let result = TenantDescriptors.ListView.project event

  match result with
  | TenantDescriptors.ListView.UpdateItem fn ->
    let newItem = fn item
    let expected: TenantDescriptors.ListView.Item = { title = "My title" }
    test <@ newItem = expected @>

(*******************
 * DetailsView.empty
 *******************)

[<Fact>]
let aggregateDetailsViewEmptyItemIsEmpty () =
  let item = TenantDescriptors.DetailsView.empty

  let expected: TenantDescriptors.DetailsView.Details =
    { title = ""; description = "" }

  test <@ item = expected @>

(*******************
 * DetailsView.project
 *******************)

[<Fact>]
let aggregateDetailsViewProjectWasUpdatedOnItemShouldReturnUpdateDetailsViewCommand () =
  let item = TenantDescriptors.DetailsView.empty

  let event = TenantDescriptors.WasUpdated { title = "My title"; description = "My description" }

  let result = TenantDescriptors.DetailsView.project event

  match result with
  | TenantDescriptors.DetailsView.UpdateDetails fn ->
    let newItem = fn item
    let expected: TenantDescriptors.DetailsView.Details =
      { title = "My title"; description = "My description" }
    test <@ newItem = expected @>

(*******************
 * PrintUpdates.project
 *******************)

[<Fact>]
let aggregatePrintUpdatesHandleWasUpdatedOnItemShouldReturnProcessCommand () =
  let event = TenantDescriptors.WasUpdated { title = "My title"; description = "My description" }

  let result = TenantDescriptors.PrintUpdates.handle event

  match result with
  | TenantDescriptors.PrintUpdates.LogMessage message ->
    let expected = "Tenant My title was updated"
    test <@ message = expected @>
