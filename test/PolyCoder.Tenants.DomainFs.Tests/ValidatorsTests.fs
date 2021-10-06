module PolyCoder.Tenants.DomainFs.ValidatorsTests

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
      member _.mustNotBeShorterThan v = sprintf "mustNotBeShorterThan%i" v
      member _.mustNotBeLongerThan v = sprintf "mustNotBeLongerThan%i" v
  }

(***************
 * isNotEmpty
 ***************)

[<Fact>]
let isNotEmptyWithNullShouldReturnError () =
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
let isNotEmptyWithEmptyShouldReturnError () =
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
let isNotEmptyWithWhiteSpaceShouldReturnOk (value: string) =
  let propertyName = "my-property"
  let result = value |> Validators.isNotEmpty mockStrings propertyName

  test <@ result = Ok @>

[<Property>]
let isNotEmptyWithAnyNonEmptyShouldBeOk (NonEmptyString value) =
  let propertyName = "my-property"
  let result = value |> Validators.isNotEmpty mockStrings propertyName

  test <@ result = Ok @>

(***************************
 * isNotEmptyOrWhiteSpace
 ***************************)
  
[<Fact>]
let isNotEmptyOrWhiteSpaceWithNullShouldReturnError () =
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
let isNotEmptyOrWhiteSpaceWithEmptyShouldReturnError () =
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
let isNotEmptyOrWhiteSpaceWithWhiteSpaceShouldReturnOk (value: string) =
  let propertyName = "my-property"
  let result = value |> Validators.isNotEmptyOrWhiteSpace mockStrings propertyName
  let expectedError: ValidationItem = {
    errorCode = "isNotEmptyOrWhiteSpace"
    property = propertyName
    message = "mustNotBeWhiteSpace"
  }

  test <@ result = Errors [ expectedError ] @>

[<Property>]
let isNotEmptyOrWhiteSpaceWithAnyNonEmptyShouldBeOk (NonWhiteSpaceString value) =
  let propertyName = "my-property"
  let result = value |> Validators.isNotEmptyOrWhiteSpace mockStrings propertyName

  test <@ result = Ok @>

(*********************
 * isNotShorterThan
 *********************)
  
[<Property>]
let isNotShorterThanWithNullShouldBeOk (NonNegativeInt minLength) =
  let propertyName = "my-property"
  let value: string = null
  let result = value |> Validators.isNotShorterThan minLength mockStrings propertyName

  test <@ result = Ok @>

type StringOfLength = StringOfLength of string

type StringOfLengthAtLeast5Chars =
  static member StringOfLength() =
    Arb.Default.NonEmptyString().Generator
    |> Gen.map (fun (NonEmptyString s) -> s)
    |> Gen.filter (fun s -> String.length s >= 5)
    |> Gen.map StringOfLength
    |> Arb.fromGen
  
[<Property(Arbitrary = [| typeof<StringOfLengthAtLeast5Chars> |])>]
let isNotShorterThanWithLongStringShouldBeOk (StringOfLength value) =
  let propertyName = "my-property"
  let result = value |> Validators.isNotShorterThan 5 mockStrings propertyName

  test <@ result = Ok @>

type StringOfLengthAtMost4Chars =
  static member StringOfLength() =
    Arb.Default.NonEmptyString().Generator
    |> Gen.map (fun (NonEmptyString s) -> s)
    |> Gen.filter (fun s -> String.length s <= 4)
    |> Gen.map StringOfLength
    |> Arb.fromGen
  
[<Property(Arbitrary = [| typeof<StringOfLengthAtMost4Chars> |])>]
let isNotShorterThanWithShortStringShouldReturnError (StringOfLength value) =
  let propertyName = "my-property"
  let result = value |> Validators.isNotShorterThan 5 mockStrings propertyName
  let expectedError: ValidationItem = {
    errorCode = "isNotShorterThan"
    property = propertyName
    message = "mustNotBeShorterThan5"
  }

  test <@ result = Errors [ expectedError ] @>

(*********************
 * isNotShorterThan
 *********************)
  
[<Property>]
let isNotLongerThanWithNullShouldBeOk (NonNegativeInt minLength) =
  let propertyName = "my-property"
  let value: string = null
  let result = value |> Validators.isNotLongerThan minLength mockStrings propertyName

  test <@ result = Ok @>
  
[<Property(Arbitrary = [| typeof<StringOfLengthAtMost4Chars> |])>]
let isNotLongerThanWithLongStringShouldBeOk (StringOfLength value) =
  let propertyName = "my-property"
  let result = value |> Validators.isNotLongerThan 4 mockStrings propertyName

  test <@ result = Ok @>
  
[<Property(Arbitrary = [| typeof<StringOfLengthAtLeast5Chars> |])>]
let isNotLongerThanWithShortStringShouldReturnError (StringOfLength value) =
  let propertyName = "my-property"
  let result = value |> Validators.isNotLongerThan 4 mockStrings propertyName
  let expectedError: ValidationItem = {
    errorCode = "isNotLongerThan"
    property = propertyName
    message = "mustNotBeLongerThan4"
  }

  test <@ result = Errors [ expectedError ] @>
