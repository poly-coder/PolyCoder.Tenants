module PolyCoder.Tenants.DomainFs.Validators

open AccidentalFish.FSharp.Validation

let internal errorItem errorCode property message : ValidationItem =
  {
    message = message
    property = property
    errorCode = errorCode
  }

let internal error errorCode property message : ValidationState =
  Errors([errorItem errorCode property message])

let isNotEmpty =
  let error = error "isNotEmpty"

  fun (strings: ITenantsDomainStrings) propertyName (value: string) ->
    if isNull value then
      strings.mustNotBeNull |> error propertyName
    elif System.String.IsNullOrEmpty(value) then
      strings.mustNotBeEmpty |> error propertyName
    else
      Ok

let isNotEmptyOrWhiteSpace =
  let error = error "isNotEmptyOrWhiteSpace"

  fun (strings: ITenantsDomainStrings) propertyName (value: string) ->
    if isNull value then
      strings.mustNotBeNull |> error propertyName
    elif System.String.IsNullOrEmpty(value) then
      strings.mustNotBeEmpty |> error propertyName
    elif System.String.IsNullOrWhiteSpace(value) then
      strings.mustNotBeWhiteSpace |> error propertyName
    else
      Ok

let isNotShorterThan minLength =
  let error = error "isNotShorterThan"

  fun (strings: ITenantsDomainStrings) propertyName (value: string) ->
    if isNull value |> not && String.length value < minLength then
      strings.mustNotBeShorterThan minLength |> error propertyName
    else
      Ok

let isNotLongerThan maxLength =
  let error = error "isNotLongerThan"

  fun (strings: ITenantsDomainStrings) propertyName (value: string) ->
    if isNull value |> not && String.length value > maxLength then
      strings.mustNotBeLongerThan maxLength |> error propertyName
    else
      Ok
