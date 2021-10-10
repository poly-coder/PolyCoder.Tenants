module PolyCoder.Tenants.Domain.TenantDescriptors

type Data = {
  title: string
  description: string
}

module Validate =
  open AccidentalFish.FSharp.Validation

  [<Literal>]
  let MinTitleLength = 5

  [<Literal>]
  let MaxTitleLength = 80

  [<Literal>]
  let MaxDescriptionLength = 1000

  let titleValidators strings = [
    Validators.isNotEmptyOrWhiteSpace strings
    Validators.isNotShorterThan MinTitleLength strings
    Validators.isNotLongerThan MaxTitleLength strings
  ]

  let descriptionValidators strings = [
    Validators.isNotLongerThan MaxDescriptionLength strings
  ]

  let data strings =
    createValidatorFor<Data>() {
      validate (fun d -> d.title) (titleValidators strings)
      validate (fun d -> d.description) (descriptionValidators strings)
    }
