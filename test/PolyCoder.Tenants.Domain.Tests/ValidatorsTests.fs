module PolyCoder.Tenants.Domain.ValidatorsTests

open AccidentalFish.FSharp.Validation
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

let mockStrings =
  { new ITenantsDomainStrings with
      member _.mustNotBeNull = "mustNotBeNull"
      member _.mustNotBeEmpty = "mustNotBeEmpty"
      member _.mustNotBeWhiteSpace = "mustNotBeWhiteSpace"
      member _.mustNotBeShorterThan value = sprintf "mustNotBeShorterThan%i" value
      member _.mustNotBeLongerThan value = sprintf "mustNotBeLongerThan%i" value
      member _.mustMatchPattern pattern = sprintf "mustMatchPattern%s" pattern

      member _.mustBeAValidUrlSegment = "mustBeAValidUrlSegment"
  }

type StringOfLength = StringOfLength of string

type StringOfLengthAtLeast5Chars =
  static member StringOfLength() =
    Arb.Default.NonEmptyString().Generator
    |> Gen.map (fun (NonEmptyString s) -> s)
    |> Gen.filter (fun s -> String.length s >= 5)
    |> Gen.map StringOfLength
    |> Arb.fromGen

type StringOfLengthAtMost4Chars =
  static member StringOfLength() =
    Arb.Default.NonEmptyString().Generator
    |> Gen.map (fun (NonEmptyString s) -> s)
    |> Gen.filter (fun s -> String.length s <= 4)
    |> Gen.map StringOfLength
    |> Arb.fromGen

module IsNotEmpty =
  [<Fact>]
  let withNullShouldReturnError () =
    let propertyName = "my-property"
    let value: string = null
    let result = value |> Validators.isNotEmpty mockStrings propertyName
    let expectedError: ValidationItem = {
      errorCode = "isNotEmpty"
      property = propertyName
      message = "mustNotBeNull"
    }

    test <@ result = Errors [ expectedError ] @>

  [<Fact>]
  let withEmptyShouldReturnError () =
    let propertyName = "my-property"
    let value: string = ""
    let result = value |> Validators.isNotEmpty mockStrings propertyName
    let expectedError: ValidationItem = {
      errorCode = "isNotEmpty"
      property = propertyName
      message = "mustNotBeEmpty"
    }

    test <@ result = Errors [ expectedError ] @>

  [<Theory>]
  [<InlineData(" ")>]
  [<InlineData("       ")>]
  [<InlineData("\t")>]
  [<InlineData("\r")>]
  [<InlineData("\r\n")>]
  let withWhiteSpaceShouldReturnOk (value: string) =
    let propertyName = "my-property"
    let result = value |> Validators.isNotEmpty mockStrings propertyName

    test <@ result = Ok @>

  [<Property>]
  let withAnyNonEmptyShouldBeOk (NonEmptyString value) =
    let propertyName = "my-property"
    let result = value |> Validators.isNotEmpty mockStrings propertyName

    test <@ result = Ok @>

module IsNotEmptyOrWhiteSpace =
  [<Fact>]
  let withNullShouldReturnError () =
    let propertyName = "my-property"
    let value: string = null
    let result = value |> Validators.isNotEmptyOrWhiteSpace mockStrings propertyName
    let expectedError: ValidationItem = {
      errorCode = "isNotEmptyOrWhiteSpace"
      property = propertyName
      message = "mustNotBeNull"
    }

    test <@ result = Errors [ expectedError ] @>

  [<Fact>]
  let withEmptyShouldReturnError () =
    let propertyName = "my-property"
    let value: string = ""
    let result = value |> Validators.isNotEmptyOrWhiteSpace mockStrings propertyName
    let expectedError: ValidationItem = {
      errorCode = "isNotEmptyOrWhiteSpace"
      property = propertyName
      message = "mustNotBeEmpty"
    }

    test <@ result = Errors [ expectedError ] @>

  [<Theory>]
  [<InlineData(" ")>]
  [<InlineData("       ")>]
  [<InlineData("\t")>]
  [<InlineData("\r")>]
  [<InlineData("\r\n")>]
  let withWhiteSpaceShouldReturnOk (value: string) =
    let propertyName = "my-property"
    let result = value |> Validators.isNotEmptyOrWhiteSpace mockStrings propertyName
    let expectedError: ValidationItem = {
      errorCode = "isNotEmptyOrWhiteSpace"
      property = propertyName
      message = "mustNotBeWhiteSpace"
    }

    test <@ result = Errors [ expectedError ] @>

  [<Property>]
  let withAnyNonEmptyShouldBeOk (NonWhiteSpaceString value) =
    let propertyName = "my-property"
    let result = value |> Validators.isNotEmptyOrWhiteSpace mockStrings propertyName

    test <@ result = Ok @>


module IsNotShorterThan =
  [<Property>]
  let withNullShouldBeOk (NonNegativeInt minLength) =
    let propertyName = "my-property"
    let value: string = null
    let result = value |> Validators.isNotShorterThan minLength mockStrings propertyName

    test <@ result = Ok @>

  [<Property(Arbitrary = [| typeof<StringOfLengthAtLeast5Chars> |])>]
  let withLongStringShouldBeOk (StringOfLength value) =
    let propertyName = "my-property"
    let result = value |> Validators.isNotShorterThan 5 mockStrings propertyName

    test <@ result = Ok @>

  [<Property(Arbitrary = [| typeof<StringOfLengthAtMost4Chars> |])>]
  let withShortStringShouldReturnError (StringOfLength value) =
    let propertyName = "my-property"
    let result = value |> Validators.isNotShorterThan 5 mockStrings propertyName
    let expectedError: ValidationItem = {
      errorCode = "isNotShorterThan"
      property = propertyName
      message = "mustNotBeShorterThan5"
    }

    test <@ result = Errors [ expectedError ] @>

module IsNotLongerThan =
  [<Property>]
  let withNullShouldBeOk (NonNegativeInt minLength) =
    let propertyName = "my-property"
    let value: string = null
    let result = value |> Validators.isNotLongerThan minLength mockStrings propertyName

    test <@ result = Ok @>

  [<Property(Arbitrary = [| typeof<StringOfLengthAtMost4Chars> |])>]
  let withLongStringShouldBeOk (StringOfLength value) =
    let propertyName = "my-property"
    let result = value |> Validators.isNotLongerThan 4 mockStrings propertyName

    test <@ result = Ok @>

  [<Property(Arbitrary = [| typeof<StringOfLengthAtLeast5Chars> |])>]
  let withShortStringShouldReturnError (StringOfLength value) =
    let propertyName = "my-property"
    let result = value |> Validators.isNotLongerThan 4 mockStrings propertyName
    let expectedError: ValidationItem = {
      errorCode = "isNotLongerThan"
      property = propertyName
      message = "mustNotBeLongerThan4"
    }

    test <@ result = Errors [ expectedError ] @>
