module Lexer (
    lexer
) where
-- module Lexer ( lexer) where

import TinyPrelude
import Main (Stage, Error, StageErrorOrOutput)

data Token
  = Let -- `let`
  | Where -- `where`
  | In -- `in`
  | Data -- `data`
  | Type -- `type`
  | If -- `if`
  | Then -- `then`
  | Else -- `else`
  | Case -- `case`
  | Of -- `of`
  | Underscore -- `_`
  | OpenParens -- `(`
  | CloseParens -- `)`
  | OpenBrace -- `{`
  | CloseBrace -- `}`
  | Semicolon -- `;`
  | DoubleColon -- `::`
  | To -- `->`
  | VBar -- `|`
  | BSlash -- `\`
  | FSlash -- `/`
  | Plus -- `+`
  | PlusPlus -- `++`
  | Dash -- `-`
  | Asterisk -- `*`
  | Equal -- `=`
  | Dollar -- `$`
  | LeftAngle -- `<`
  | Dot -- `.`
  | LeftAngleEqual -- `<=`
  | RightAngle -- `>`
  | RightAngleEqual -- `>=`
  | EqualEqual -- `==`
  | FSlashEqual -- `/=`
  | VBarVBar -- `||`
  | AmpersandAmpersand -- `&&`
  -- An Int literal
  | IntLit Int -- An Int literal
  -- A String literal
  | StringLit String
  -- A Bool literal
  | BoolLit Bool
  -- The type `Int`
  | IntType
  -- The type `String`
  | StringType
  -- The type `Bool`
  | BoolType
  -- A name starting with an uppercase letter
  | UpperName String
  -- A name starting witha lowercase letter
  | LowerName String
  deriving (Eq, Show)

listOfToken = ["where"
              ,"let"
              ,"in"
              ,"data"
              ,"in"
              ,"if"
              ,"then"
              ,"else"
              ,"case"
              ,"of"
              ,"_"
              ,"("
              ,")"
              ,"{"
              ,"}"
              ,";"
              ,"::"
              ,"->"
              ,"|"
              ,"\\"
              ,"/"
              ,"+"
              ,"++"
              ,"-"
              ,"*"
              ,"="
              ,"$"
              ,"<"
              ,"."
              ,"<="
              ,">"
              ,">="
              ,"=="
              ,"/="
              ,"||"
              ,"&&"]



newtype Lexer a = Lexer {runLexer :: String -> Either (Error LexerError) (a, String)}

instance Functor Lexer where
    fmap f (Lexer runLexer) = Lexer (runLexer >>> fmap (fist f))


satisfies :: (Char -> Bool) -> Lexer Char
satisfies p = Lexer <| \case
                        (char:_) | p char -> Right (char, _)
                        _               -> Left (LexerError (unexpectedError _))

-- satisfies :: ([Char] -> Int -> Bool) -> Lexer Char
-- satisfies p = Lexer <| \case
--                         input@(char:_) | p (takeinput -> Right (char, _)
--                         _               -> Left (LexerError (unexpectedError _))
-- lexer :: String -> StageErrorOrOutput [Token]
-- lexer a = Right [BoolType]



data LexerError = Unexpected Char 
                | UnexpectedEoF
                | UnmatchedLayout
                deriving Show

unexpectedError :: String -> LexerError
unexpectedError [] = UnexpectedEoF
unexpectedError (char:_) = Unexpected char


-- charsToToken :: String -> Token  Lexer <| \case

-- chars
-- myHumbleLexer :: String -> StageErrorOrOutput [Tokens]
-- myhumbleLexer source@(char:rest)
--              | char `elem` token