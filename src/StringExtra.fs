module Elm.Community.StringExtra.String

open System
open Elm.Core
open Elm.Regex


let private regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never


/// Change the case of the first letter of a string to either uppercase or
/// lowercase, depending of the value of `wantedCase`. This is an internal
/// function for use in `toSentenceCase` and `decapitalize`.
///
//changeCase : (Char -> Char) -> String -> String
let changeCase mutator word =
    String.uncons word
        |> Maybe.map (fun ( head, tail ) -> String.cons (mutator head) tail)
        |> Maybe.withDefault ""


/// Capitalize the first letter of a string.
///
///     toSentenceCase "this is a phrase" == "This is a phrase"
///
///     toSentenceCase "hello, world" == "Hello, world"
///
let toSentenceCase word =
    changeCase Char.toUpper word


/// Decapitalize the first letter of a string.
///
///     decapitalize "This is a phrase" == "this is a phrase"
///
///     decapitalize "Hello, World" == "hello, World"
///
let decapitalize word =
    changeCase Char.toLower word


/// Capitalize the first character of each word in a string.
///
///     toTitleCase "this is a phrase" == "This Is A Phrase"
///
///     toTitleCase "hello, world" == "Hello, World"
///
let toTitleCase ws =
    let
        uppercaseMatch =
            Regex.replace (regexFromString "\\w+") (fun v -> v.Match |> toSentenceCase)
    in
    ws
        |> Regex.replace
            (regexFromString "^([a-z])|\\s+([a-z])")
            (fun v -> v.Match |> uppercaseMatch)


/// Replace text within a portion of a string given a substitution
/// string, a start index and an end index. The substitution includes the character
/// at the start index but not the one at the end index.
///
///     replaceSlice "Sue" 4 7 "Hi, Bob" == "Hi, Sue"
///
///     replaceSlice "elephants" 0 6 "snakes on a plane!" == "elephants on a plane!"
///
///     replaceSlice "under" 7 9 "snakes on a plane!" == "snakes under a plane!"
///
let replaceSlice substitution start end' string =
    String.slice 0 start string ++ substitution ++ String.slice end' (String.length string) string


/// Insert a substring at the specified index.
///
///     insertAt "world" 6 "Hello " == "Hello world"
///
//insertAt : String -> Int -> String -> String
let insertAt insert pos string =
    replaceSlice insert pos pos string


let rec private breaker width string acc =
    match string with
    | "" ->
        List.reverse acc

    | _ ->
        breaker width
            (String.dropLeft width string)
            (String.slice 0 width string :: acc)


/// Break a string into a list of strings of a specified maximum length.
///
///     break 10 "The quick brown fox" == [ "The quick ", "brown fox" ]
///
///     break 2 "" == [ "" ]
///
let break' width string =
    if width == 0 || string == "" then
        [ string ]

    else
        breaker width string []


let private softBreakRegexp width =
    regexFromString <| ".{1," ++ String.fromInt width ++ "}(\\s+|$)|\\S+?(\\s+|$)"


/// Break a string into a list of strings of a specified maximum length,
/// without truncating words.
///
///     softBreak 6 "The quick brown fox" == [ "The quick", " brown", " fox" ]
///
//softBreak : Int -> String -> List String
let softBreak width string =
    if width <= 0 then
        []
    else
        string
            |> Regex.find (softBreakRegexp width)
            |> List.map (fun v -> v.Match)


/// Trim the whitespace of both sides of the string and compress
/// repeated whitespace internally to a single whitespace char.
///
///     clean " The   quick brown   fox    " == "The quick brown fox"
///
let clean string =
    string
        |> Regex.replace (regexFromString "\\s\\s+") (always " ")
        |> String.trim


///Test if a string is empty or only contains whitespace.
///
///    isBlank "" == True
///
///    isBlank "\n" == True
///
///    isBlank "  " == True
///
///    isBlank " a" == False
///
let isBlank string =
    Regex.contains (regexFromString "^\\s*$") string


/// Convert an underscored or dasherized string to a camelized one.
///
///    camelize "-moz-transform" == "MozTransform"
///
let camelize string =
    Regex.replace
        (regexFromString "[-_\\s]+(.)?")
        (fun v ->
            match v.SubMatches with
            | (Just m) :: _ ->
                String.toUpper m

            | _ ->
                ""
        )
        (String.trim string)


/// All non-word characters will be stripped out of the original string.
/// Convert a string to a camelized string starting with an uppercase letter.
///
///     classify "some_class_name" == "SomeClassName"
///
///     classify "myLittleCamel.class.name" == "MyLittleCamelClassName"
///
let classify string =
    string
        |> Regex.replace (regexFromString "[\\W_]") (always " ")
        |> camelize
        |> String.replace " " ""
        |> toSentenceCase


/// Surround a string with another string.
///
///     surround "bar" "foo" == "barfoobar"
///
let surround wrapper (string:string) =
    wrapper ++ string ++ wrapper


/// Remove surrounding strings from another string.
///
///     unsurround "foo" "foobarfoo" == "bar"
///
let unsurround wrapper string =
    if String.startsWith wrapper string && String.endsWith wrapper string then
        let
            length =
                String.length wrapper
        in
        string
            |> String.dropLeft length
            |> String.dropRight length

    else
        string


/// Add quotes to a string.
///
///     quote "foo" == "\"foo\""
///
let quote string =
    surround "\"" string


/// Remove quotes that surround a string.
///
///     unquote "\"foo\"" == "foo"
///
///     unquote "\"foo\"bar\""
///
let unquote string =
    unsurround "\"" string


/// Return a string joined by underscores after separating it by its uppercase characters.
/// Any sequence of spaces or dashes will also be converted to a single underscore.
/// The final string will be lowercased.
///
///     underscored "SomeClassName" == "some_class_name"
///     underscored "some-class-name" == "some_class_name"
///     underscored "SomeClass name" == "some_class_name
///
let underscored string =
    string
        |> String.trim
        |> Regex.replace (regexFromString "([a-z\\d])([A-Z]+)") (fun v -> v.SubMatches |> List.filterMap identity |> String.join "_")
        |> Regex.replace (regexFromString "[_-\\s]+") (always "_")
        |> String.toLower


/// Return a string joined by dashes after separating it by its uppercase characters.
/// Any sequence of spaces or underscores will also be converted to a single dash.
/// The final string will be lowercased.
///
///     dasherize "SomeClassName" == "-some-class-name"
///     dasherize "some_class_name" = "some-class-name"
///     dasherize "someClass name" = "some-class-name"
///
let dasherize string =
    string
        |> String.trim
        |> Regex.replace (regexFromString "([A-Z])") (fun v -> v.Match |> String.append "-")
        |> Regex.replace (regexFromString "[_-\\s]+") (always "-")
        |> String.toLower


/// Separate a string into parts of a given width, using a given separator.
///
/// Look at `wrap` if you just want to wrap using newlines.
///
///     wrapWith 7 "\n" "My very long text" === "My very\nlong text"
///
///     wrapWith 100 "\n" "Too short" === "Too short"
///
let wrapWith width separator string =
    string
        |> break' width
        |> String.join separator


/// Chop a given string into parts of a given width, separating them with a
/// new line.
///
///     wrap 7 "My very long text" === "My very\nlong te\nxt"
///
///     wrap 100 "Too short" === "Too short"
///
let inline wrap width string =
    wrapWith width "\n" string


/// Chop a given string into parts of a given width without breaking words apart,
/// and then separate them using the given separator.
///
///     softWrapWith 7 "..." "My very long text" === "My very...long text"
///
///     softWrapWith 3 "\n" "Hello World" === "Hello \nWorld"
///
///     softWrapWith 100 "\t" "Too short" === "Too short"
///
let softWrapWith width separator string =
    string
        |> softBreak width
        |> String.join separator


/// Chop a given string into parts of a given width without breaking words apart,
/// and then separate them using a new line.
///
///     softWrap 7 "My very long text" === "My very\nlong text"
///
///     softWrap 3 "Hello World" === "Hello \nWorld"
///
///     softWrap 100 "Too short" === "Too short"
///
let softWrap width string =
    softWrapWith width "\n" string


/// Convert an underscored, camelized, or dasherized string into one that can be
/// read by humans. Also remove beginning and ending whitespace, and removes the
/// postfix '\_id'. The first character will be capitalized.
///
///     humanize "this_is_great" == "This is great"
///     humanize "ThisIsGreat" = "This is great"
///     humanize "this-is-great" = "This is great"
///     humanize "author_id" = "Author"
///
let humanize string =
    string
        |> Regex.replace (regexFromString "[A-Z]+") (fun v -> v.Match |> String.append "-")
        |> Regex.replace (regexFromString "_id$|[-_\\s]+") (always " ")
        |> String.trim
        |> String.toLower
        |> toSentenceCase


/// Remove the shortest sequence of leading spaces or tabs on each line
/// of the string, so that at least one of the lines will not have any
/// leading spaces nor tabs and the rest of the lines will have the same
/// amount of indentation removed.
///
///     unindent "  Hello\n    World " == "Hello\n  World"
///
///     unindent "\t\tHello\n\t\t\t\tWorld" == "Hello\n\t\tWorld"
///
let unindent multilineSting =
    let lines =
        String.lines multilineSting

    let rec countLeadingWhitespace count line =
        match String.uncons line with
        | Nothing ->
            count

        | Just ( char, rest ) ->
            match char with
            | ' ' ->
                countLeadingWhitespace (count + 1) rest

            | '\t' ->
                countLeadingWhitespace (count + 1) rest

            | _ ->
                count

    let isNotWhitespace char =
        char /= ' ' && char /= '\t'

    let minLead =
        lines
            |> List.filter (String.any isNotWhitespace)
            |> List.map (countLeadingWhitespace 0)
            |> List.minimum
            |> Maybe.withDefault 0

    lines
        |> List.map (String.dropLeft minLead)
        |> String.join "\n"


/// Return the number of occurrences of a substring in another string.
///
///     countOccurrences "Hello" "Hello World" == 1
///
///     countOccurrences "o" "Hello World" == 2
///
let countOccurrences needle haystack =
    if String.length needle == 0 || String.length haystack == 0 then
        0

    else
        haystack
            |> String.indexes needle
            |> List.length


/// Truncate the second string at the specified length if the string is
/// longer than the specified length, and replace the end of the truncated
/// string with the first string, such that the resulting string is of the
/// specified length.
///
/// The resulting string will have at most the specified length.
///
///     ellipsisWith 5 " .." "Hello World" == "He .."
///
///     ellipsisWith 10 " .." "Hello World" == "Hello W .."
///
///     ellipsisWith 10 " .." "Hello" == "Hello"
///
///     ellipsisWith 8 " .." "Hello World" == "Hello .."
///
let ellipsisWith howLong append string =
    if String.length string <= howLong then
        string

    else
        String.left (howLong - String.length append) string ++ append


/// Truncate the string at the specified length if the string is
/// longer than the specified length, and replace the end of the truncated
/// string with `"..."`, such that the resulting string is of the
/// specified length.
///
/// The resulting string will have at most the specified length.
///
///     ellipsis 5 "Hello World" == "He..."
///
///     ellipsis 10 "Hello World" == "Hello W..."
///
///     ellipsis 10 "Hello" == "Hello"
///
///     ellipsis 8 "Hello World" == "Hello..."
///
let inline ellipsis howLong string =
    ellipsisWith howLong "..." string


/// Truncate the string at the last complete word less than or equal to
/// the specified length and append `"..."`. When the specified length is
/// less than the length of the first word, the ellipsis is appended to the
/// first word. When the specified length is greater than or equal to the
/// length of the string, an identical string is returned.
///
/// In contrast to `ellipsis`, this function will not produce incomplete
/// words, and the resulting string can exceed the specified length. In
/// addition, it removes trailing whitespace and punctuation characters at
/// the end of the truncated string.
///
///     softEllipsis 1 "Hello, World" == "Hello..."
///
///     softEllipsis 5 "Hello, World" == "Hello..."
///
///     softEllipsis 6 "Hello, World" == "Hello..."
///
///     softEllipsis 15 "Hello, cruel world" == "Hello, cruel..."
///
///     softEllipsis 10 "Hello" == "Hello"
///
let softEllipsis howLong string =
    if String.length string <= howLong then
        string

    else
        string
            |> Regex.findAtMost 1 (softBreakRegexp howLong)
            |> List.map(fun v -> v.Match)
            |> String.join ""
            |> Regex.replace (regexFromString "([\\.,;:\\s])+$") (always "")
            |> (fun a -> String.append a "...")


let private toSentenceBaseCase list =
    match list with
    | x :: [] ->
        x

    | x :: y :: [] ->
        x ++ " and " ++ y

    | _ ->
        ""


let rec private toSentenceHelper lastPart sentence list =
    match list with
    | [] ->
        sentence

    | x :: [] ->
        sentence ++ lastPart ++ x

    | x :: xs ->
        toSentenceHelper lastPart (sentence ++ ", " ++ x) xs

/// Convert a list of strings into a human-readable list.
///
///     toSentence [] == ""
///
///     toSentence [ "lions" ] == "lions"
///
///     toSentence [ "lions", "tigers" ] == "lions and tigers"
///
///     toSentence [ "lions", "tigers", "bears" ] == "lions, tigers and bears"
///
let toSentence list =
    match list with
    | x :: y :: z :: more ->
        toSentenceHelper " and " (x ++ ", " ++ y) (z :: more)

    | _ ->
        toSentenceBaseCase list


/// Convert a list of strings into a human-readable list using an oxford comma.
///
///     toSentenceOxford [] == ""
///
///     toSentenceOxford [ "lions" ] == "lions"
///
///     toSentenceOxford [ "lions", "tigers" ] == "lions and tigers"
///
///     toSentenceOxford [ "lions", "tigers", "bears" ] == "lions, tigers, and bears"
///
let toSentenceOxford list =
    match list with
    | x :: y :: z :: more ->
        toSentenceHelper ", and " (x ++ ", " ++ y) (z :: more)

    | _ ->
        toSentenceBaseCase list


/// Remove all HTML tags from the string, preserving the text inside them.
///
///     stripTags "a <a href=\"#\">link</a>" == "a link"
///     stripTags "<script>alert('hello world!')</script> == "alert('hello world!')"
///
let stripTags string =
    string
        |> Regex.replace (regexFromString "<\\/?[^>]+>") (always "")


/// Given a number, a singular string, and a plural string, return the number
/// followed by a space, followed by either the singular string if the number was 1,
/// or the plural string otherwise.
///
///     pluralize "elf" "elves" 2 == "2 elves"
///
///     pluralize "elf" "elves" 1 == "1 elf"
///
///     pluralize "elf" "elves" 0 == "0 elves"
///
let pluralize singular plural count =
    if count == 1 then
        "1 " ++ singular

    else
        String.fromInt count ++ " " ++ plural



let private regexEscape =
    Regex.replace (regexFromString "[-/\\^$*+?.()|[\\]{}]") (fun v -> "\\" ++ v.Match)

let rec private firstResultHelp def list =
    match list with
    | [] ->
        def

    | (Just a) :: _ ->
        a

    | Nothing :: rest ->
        firstResultHelp def rest


let private firstResult list =
    firstResultHelp "" list


/// Search a string from left to right for a pattern and return a substring
/// consisting of the characters in the string that are to the right of the pattern.
///
///     rightOf "_" "This_is_a_test_string" == "is_a_test_string"
///
let rightOf pattern string =
    string
        |> Regex.findAtMost 1 (regexFromString <| regexEscape pattern ++ "(.*)$")
        |> List.map (fun v -> v.SubMatches |> firstResult)
        |> String.join ""


/// Search a string from left to right for a pattern and return a substring
/// consisting of the characters in the string that are to the left of the pattern.
///
///     leftOf "_" "This_is_a_test_string" == "This"
///
let leftOf pattern string =
    string
        |> Regex.findAtMost 1 (regexFromString <| "^(.*?)" ++ regexEscape pattern)
        |> List.map (fun v -> v.SubMatches |> firstResult)
        |> String.join ""

/// Search a string from right to left for a pattern and return a substring
/// consisting of the characters in the string that are to the right of the pattern.
///
///     rightOfBack "_" "This_is_a_test_string" == "string"
///
let rightOfBack pattern string =
    string
        |> String.indexes pattern
        |> List.reverse
        |> List.head
        |> Maybe.map ((+) (String.length pattern) >> (fun a -> String.dropLeft a string))
        |> Maybe.withDefault ""


/// Search a string from right to left for a pattern and return a substring
/// consisting of the characters in the string that are to the left of the pattern.
///
///     leftOfBack "_" "This_is_a_test_string" == "This_is_a_test"
///
let leftOfBack pattern string =
    string
        |> String.indexes pattern
        |> List.reverse
        |> List.head
        |> Maybe.map (fun a -> String.left a string)
        |> Maybe.withDefault ""


/// Convert a string into a list of UTF-32 code points.
///
///     toCodePoints "abc" == [ 97, 98, 99 ]
///
///     toCodePoints "©§π" == [ 169, 167, 960 ]
///
///     toCodePoints "💩!" == [ 128169, 33 ]
///
/// Note that code points do not necessarily correspond to logical/visual
/// characters, since it is possible for things like accented characters to be
/// represented as two separate UTF-32 code points (a base character and a
/// combining accent).
///
/// `toCodePoints string` is equivalent to:
///
///     List.map Char.toCode (String.toList string)
///
let inline toCodePoints string =
    List.map Char.toCode (String.toList string)


/// Convert a list of UTF-32 code points into a string. Inverse of
/// `toCodePoints`.
///
///     fromCodePoints [ 97, 98, 99 ] == "abc"
///
///     fromCodePoints [ 169, 167, 960 ] == "©§π"
///
///     fromCodePoints [ 128169, 33 ] == "💩!"
///
/// `fromCodePoints codePoints` is equivalent to:
///
///     String.fromList (List.map Char.fromCode codePoints)
///
let fromCodePoints codePoints =
    String.fromList (List.map Char.fromCode codePoints)


/// Convert a string to a Nothing when empty.
///
///     nonEmpty "" == Nothing
///
///     nonEmpty "Hello world" == Just "Hello world"
///
let nonEmpty string =
    if String.isEmpty string then
        Nothing

    else
        Just string


/// Convert a string to a Nothing when blank.
///
///     nonBlank "" == Nothing
///
///     nonBlank "Hello world" == Just "Hello world"
///
let nonBlank string =
    if isBlank string then
        Nothing

    else
        Just string


/// Create list with regex and char to replace.
let private accentRegex =
    let matches =
        [ ( "[à-æ]", "a" )
          ( "[À-Æ]", "A" )
          ( "ç", "c" )
          ( "Ç", "C" )
          ( "[è-ë]", "e" )
          ( "[È-Ë]", "E" )
          ( "[ì-ï]", "i" )
          ( "[Ì-Ï]", "I" )
          ( "ñ", "n" )
          ( "Ñ", "N" )
          ( "[ò-ö]", "o" )
          ( "[Ò-Ö]", "O" )
          ( "[ù-ü]", "u" )
          ( "[Ù-Ü]", "U" )
          ( "ý", "y" )
          ( "ÿ", "y" )
          ( "Ý", "Y" )
        ]

    List.map (Tuple.mapFirst regexFromString) matches


/// Remove accents from string.
///
///     removeAccents "andré" == "andre"
///
///     removeAccents "Atenção" == "Atencao"
///
let removeAccents string =
    if String.isEmpty string then
        string

    else
        let do_regex_to_remove_acents ( regex, replace_character ) =
                Regex.replace regex (fun _ -> replace_character)
        in
        List.foldl do_regex_to_remove_acents string accentRegex






