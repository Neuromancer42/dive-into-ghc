jsonString :: Parser Chars
jsonString =
  between
    (is $ fromSpecialCharacter DoubleQuote)
    (charTok $ fromSpecialCharacter DoubleQuote)
    (list (spacialHex <|> noneof (listh "\\\"")))
  where
    spacialHex = is '\\' *> (hexu ||| special)
    special =
      do
        c <- character
        case toSpecialCharacter c of
            Full sc -> valueParser 
              $ fromSpecialCharacter sc
            Empty -> unexpectedCharParser c
            
jsonNumber :: Parser Rational
jsonNumber = P $ \inp ->
  case readFloats inp of
      Full (num, inp') -> Result inp' num
      Empty -> ErrorResult $
                case inp of
                  Nil -> UnexpectedEof
                  c :. _ -> UnexpectedChar c

jsonTrue :: Parser Chars
jsonTrue = stringTok "true"

jsonFalse :: Parser Chars
jsonFalse = stringTok "false"

jsonNull :: Parser Chars
jsonNull = stringTok "null"

jsonArray :: Parser (List JsonValue)
jsonArray = betweenSepbyComma '[' ']' jsonValue

jsonObject :: Parser Assoc
jsonObject = betweenSepbyComma '{' '}' singleObject
  where
    singleObject =
      do
        spaces
        s <- jsonString
        spaces
        is ':'
        v <- jsonValue
        spaces
        return (s, v)

jsonValue :: Parser JsonValue
jsonValue = spaces *> (
  (pure JsonNull <* jsonNull) <|>
  (pure JsonTrue <* jsonTrue) <|>
  (pure JsonFalse <* jsonFalse) <|>
  (JsonArray <$> jsonArray) <|>
  (JsonString <$> jsonString) <|>
  (JsonObject <$> jsonObject) <|>
  (JsonRational False <$> jsonNumber)
  ) <* spaces

readJsonValue :: Filename -> IO (ParseResult JsonValue)
readJsonValue filename =
  do
    inp <- readFile filename
    return (parse jsonValue inp)