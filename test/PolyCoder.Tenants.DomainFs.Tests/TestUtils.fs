module TestUtils

//open FsCheck

//type NonWhitespaceString = NonWhitespaceString of string

//type CommonTestTypes = 
//  static member NonWhitespaceString() =
//    Arb.Default.NonEmptyString().Generator
//    |> Gen.map (fun (NonEmptyString s) -> s)
//    |> Gen.filter (System.String.IsNullOrWhiteSpace >> not)
//    |> Gen.map NonWhiteSpaceString
//    |> Arb.fromGen
