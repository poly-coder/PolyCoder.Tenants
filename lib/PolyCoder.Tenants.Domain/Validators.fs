module PolyCoder.Tenants.Domain.Validators

open AccidentalFish.FSharp.Validation
open System.Text.RegularExpressions

let mergeValidations validations =
  validations
  |> Seq.fold (fun prev -> function Ok -> prev | Errors es -> Seq.append prev es) Seq.empty
  |> Seq.toList
  |> function [] -> Ok | es -> Errors es

let getErrors = function Ok -> [] | Errors es -> es

let internal prependToProperty propertyName itemProperty =
  match propertyName, itemProperty with
  | "", _ -> itemProperty
  | _, "" -> propertyName
  | _, _ -> propertyName + "." + itemProperty

let internal prependToItemProperty propertyName (item: ValidationItem) =
  { item with property = prependToProperty propertyName item.property }

let onProperty propertyName = function
  | Ok -> Ok
  | Errors es ->
    es |> List.map (prependToItemProperty propertyName)
       |> Errors

let createValueValidator validators =
  fun value ->
    validators
    |> Seq.map (fun validator -> validator "" value)
    |> mergeValidations

let createPropertyValidator propertyName validators =
  fun value ->
    createValueValidator validators value
      |> onProperty propertyName

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

let matchesRegexWithMessage messageFn (regex: Regex) =
  let error = error "matchesRegex"

  fun propertyName (value: string) ->
    if isNull value |> not && regex.IsMatch(value) |> not then
      string regex |> messageFn |> error propertyName
    else
      Ok

let matchesRegex (regex: Regex) =
  fun (strings: ITenantsDomainStrings) ->
    matchesRegexWithMessage strings.mustMatchPattern regex

let matchesPatternAndOptionsWithMessage messageFn (options: RegexOptions) (pattern: string) =
  let regex = Regex(pattern, options)
  matchesRegexWithMessage messageFn regex

let matchesPatternAndOptions (options: RegexOptions) (pattern: string) =
  let regex = Regex(pattern, options)
  matchesRegex regex

let matchesPatternWithMessage messageFn (pattern: string) =
  matchesPatternAndOptionsWithMessage messageFn RegexOptions.None pattern

let matchesPattern (pattern: string) =
  matchesPatternAndOptions RegexOptions.None pattern
