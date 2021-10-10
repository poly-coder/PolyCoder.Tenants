module PolyCoder.Tenants.Domain.TenantDescriptorsTests

open AccidentalFish.FSharp.Validation
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open PolyCoder.Tenants.Domain.TenantDescriptors

let strings = ValidatorsTests.mockStrings

module ValidateConstants =
  [<Fact>]
  let minTitleLengthShouldBe5() = test <@ Validate.MinTitleLength = 5 @>

  [<Fact>]
  let maxTitleLengthShouldBe80() = test <@ Validate.MaxTitleLength = 80 @>

  [<Fact>]
  let maxDescriptionLengthShouldBe1000() = test <@ Validate.MaxDescriptionLength = 1000 @>

module ValidateData =
  [<TenantProperty>]
  let validateDataForValidInputShouldReturnOk
    (ValidTenantTitle title)
    (ValidTenantDescription description) =
    let data : Data =
      { title = title; description = description }

    let validation = data |> Validate.data strings

    test <@ validation = Ok @>

  [<Theory>]
  [<InlineData(null, "Some description")>]
  let validateDataForNullTitleShouldReturnError
    (title: string)
    (description: string) =
    let data : Data =
      { title = title; description = description }

    let expectedErrors : ValidationItem list = [
      {
        errorCode = nameof(Validators.isNotEmptyOrWhiteSpace)
        property = "title"
        message = strings.mustNotBeNull
      }
    ]

    let validation = data |> Validate.data strings

    test <@ validation = Errors expectedErrors @>

  [<Theory>]
  [<InlineData("", "Some description")>]
  let validateDataForEmptyTitleShouldReturnError
    (title: string)
    (description: string) =
    let data : Data =
      { title = title; description = description }

    let expectedErrors : ValidationItem list = [
      {
        errorCode = nameof(Validators.isNotEmptyOrWhiteSpace)
        property = "title"
        message = strings.mustNotBeEmpty
      }
      {
        errorCode = nameof(Validators.isNotShorterThan)
        property = "title"
        message = strings.mustNotBeShorterThan Validate.MinTitleLength
      }
    ]

    let validation = data |> Validate.data strings

    test <@ validation = Errors expectedErrors @>
