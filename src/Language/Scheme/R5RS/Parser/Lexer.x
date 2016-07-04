{
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Scheme.R5RS.Parser.Lexer
-- Copyright   : (c) Tyler Prete 
-- License     : BSD-style
-- Maintainer  : psyonic@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Scheme R5RS lexer.
-- See: <http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72>
-----------------------------------------------------------------------------

module Language.Scheme.R5RS.Parser.Lexer ( alexScanTokens )
where
import qualified Language.Scheme.R5RS.Parser.Token as Token
import Language.Scheme.R5RS.Syntax.Expression (Number(Integer))
}

%wrapper "basic"

-- character sets
--$whitespace = [\ \n\r\f\v\t]
$linebreak = [\n \r]
$not_linebreak = ~$linebreak
$lparen = \(
$rparen = \)
$comma = \,
$dot = \.
$tick = \'
$backtick = \`
$delimiter = [$white $lparen $rparen \| \" \;] --"] (Fixing vim's highlighting)
$letter = [a-zA-Z]
$digit = 0-9
$special_initial = [\! \$ \% \& \* \/ \: \< \= \> \? \^ \_ \~]
$initial = [$letter $special_initial]
$special_subsequent = [\+ \- \. \@]
$subsequent = [$initial $digit $special_subsequent]

@true = \#t | true
@false = \#f | false
@pound_lparen = \# $lparen
@comma_at = $comma \@
@boolean = @true | @false
@ellipsis = \.\.\.
@comment = \;$not_linebreak*
@atmosphere = $white | @comment
@intertoken_space = @atmosphere*
@peculiar_identifier = \+ | \- | \.\.\.
@expression_keyword = quote | lambda | if
    | set\! | begin | cond | and | or | case
    | let | let\* | letrec | do | delay
    | quasiquote
@syntactic_keyword = @expression_keyword
    | else | \=\> | define
    | unquote | unquote-splicing
@identifier = $initial $subsequent* | @peculiar_identifier
@number = $digit+
@character_name = space | newline
@character = \#\\. | \#\\ @character_name
@string_element = [^\"\\] | \" | \\ | \n
@string = \" @string_element* \"

thing :-

    @comment ;
    $white+ ;

    -- Token Characters
    $lparen { \s -> Token.LeftParen }
    $rparen { \s -> Token.RightParen }
    @pound_lparen { \s -> Token.PoundLeftParen }
    $tick { \s -> Token.Tick }
    $backtick { \s -> Token.BackTick }
    $comma { \s -> Token.Comma }
    @comma_at { \s -> Token.CommaAt }
    $dot { \s -> Token.Dot }
    @number { \s -> Token.Number $ Integer (toInt s) }
    @boolean { \s -> Token.Boolean (boolean_val s) }
    @character { \s -> Token.Character (toChar s) }
    @string { \s -> Token.String (stripString s) }
    @identifier { \s -> Token.Identifier s }
    

{
boolean_val str | str == "#t" = True
        | str == "true" = True
        | str == "#f" = False
        | str == "false" = False
        | otherwise = error "Not a boolean string"

toInt :: String -> Int
toInt = read

toChar :: String -> Char
toChar ('#':'\\':xs) = case xs of
    "space" -> ' '
    "newline" -> '\n'
    c:[] -> c

stripString :: String -> String
stripString = (init . tail)
}
