[<AutoOpen>]
module PolyCoder.Tenants.Domain.TestUtils

open FsCheck
open FsCheck.Xunit
open System.Text.RegularExpressions
open PolyCoder.Tenants.Domain

let validTenantIdentifierRegex = Regex(@"^([a-z][a-z0-9]*)(\-([a-z][a-z0-9]*))*$")

type WhiteSpaceString = WhiteSpaceString of string
type EmptyOrWhiteSpaceString = EmptyOrWhiteSpaceString of string

type ValidTenantIdentifier = ValidTenantIdentifier of string
type InvalidTenantIdentifier = InvalidTenantIdentifier of string
type UnmatchingTenantIdentifier = UnmatchingTenantIdentifier of string

type ValidTenantTitle = ValidTenantTitle of string
type ValidTenantDescription = ValidTenantDescription of string

let internal whitespaces = [' '; '\t'; '\n'; '\r'; '\f']
let internal atoz = ['a'..'z']
let internal zeroToNine = ['0'..'9']
let internal atoz09 = [
  yield! atoz
  yield! zeroToNine
]

type TenantArbitraries() =

  static member WhiteSpaceString() =
    gen {
      let! length = Gen.choose(1, 10)
      let! chars = Gen.arrayOfLength length (Gen.elements whitespaces)
      return System.String chars
    }
      |> Gen.map WhiteSpaceString
      |> Arb.fromGen

  static member EmptyOrWhiteSpaceString() =
    gen {
      let! length = Gen.choose(0, 10)
      let! chars = Gen.arrayOfLength length (Gen.elements whitespaces)
      return System.String chars
    }
      |> Gen.map WhiteSpaceString
      |> Arb.fromGen

  static member InvalidTenantIdentifier() =
    Arb.Default.String().Generator
      |> Gen.filter (fun s -> isNull s || validTenantIdentifierRegex.IsMatch s |> not)
      |> Gen.map InvalidTenantIdentifier
      |> Arb.fromGen

  static member ValidTenantIdentifier() =
    let genSegment = gen {
      let! firstChar = Gen.elements atoz
      let! length = Gen.choose(0, 6)
      let! restChars = Gen.listOfLength length (Gen.elements atoz09)
      let segment = System.String(firstChar :: restChars |> List.toArray)
      return segment
    }

    let genIdentifier = gen {
      let! firstSegment = genSegment
      let! length = Gen.choose(0, 3)
      let! restSegments = Gen.listOfLength length genSegment
      let identifier = System.String.Join("-", firstSegment :: restSegments)
      return identifier
    }

    genIdentifier
      |> Gen.map ValidTenantIdentifier
      |> Arb.fromGen

  static member UnmatchingTenantIdentifier() : Arbitrary<UnmatchingTenantIdentifier> =
    TenantArbitraries.InvalidTenantIdentifier().Generator
      |> Gen.map (fun (InvalidTenantIdentifier s) -> s)
      |> Gen.filter (fun s -> System.String.IsNullOrWhiteSpace s |> not)
      |> Gen.map UnmatchingTenantIdentifier
      |> Arb.fromGen

  static member ValidTenantTitle() =
    let validTitle (title: string) =
      System.String.IsNullOrWhiteSpace title |> not &&
      title.Length >= TenantDescriptors.Validate.MinTitleLength &&
      title.Length <= TenantDescriptors.Validate.MaxTitleLength

    let stringArb = Arb.Default.String()

    let generator =
      stringArb.Generator
        |> Gen.filter (validTitle)
        |> Gen.map ValidTenantTitle

    let shrinker (ValidTenantTitle title) =
      stringArb.Shrinker title
        |> Seq.filter validTitle
        |> Seq.map ValidTenantTitle

    Arb.fromGenShrink (generator, shrinker)

  static member ValidTenantDescription() =
    let validDescription (description: string) =
      isNull description ||
      description.Length <= TenantDescriptors.Validate.MaxDescriptionLength

    let stringArb = Arb.Default.String()

    let generator =
      stringArb.Generator
        |> Gen.filter (validDescription)
        |> Gen.map ValidTenantDescription

    let shrinker (ValidTenantDescription description) =
      stringArb.Shrinker description
        |> Seq.filter validDescription
        |> Seq.map ValidTenantDescription

    Arb.fromGenShrink (generator, shrinker)

type TenantProperty() =
  inherit PropertyAttribute(Arbitrary = [| typeof<TenantArbitraries> |])
