module Globber (matchGlob) where

type GlobPattern = String

data GlobChar = Literal Char
            |   Set String
            |   AnyChar
            |   AnyString deriving (Show, Eq)

type GlobString = [GlobChar]

parseGlobString :: GlobPattern -> GlobString
parseGlobString [] = []
parseGlobString ('*':cs) = AnyString:parseGlobString cs
parseGlobString ('?':cs) = AnyChar:parseGlobString cs
parseGlobString ('[':cs) = let (s1, s2) = convertSet cs in (Set s1):parseGlobString s2
    where convertSet [] = error "Invalid bracket ["
          convertSet (']':xs) = ("", xs)
          convertSet ('\\':x:xs) = let (s1, s2) = convertSet xs in (x:s1, s2)
          convertSet (l:'-':r:xs) | r == ']' = ([l, '-'], xs)
                                  | otherwise = let (s1, s2) = convertSet xs in ([l..r] ++ s1, s2)
          convertSet (x:xs) = let (s1, s2) = convertSet xs in (x:s1, s2)
parseGlobString ('\\':c:cs) = Literal c:parseGlobString cs
parseGlobString (c:cs) = Literal c:parseGlobString cs

matchGlobChar :: GlobChar -> Char -> Bool
matchGlobChar (Literal gc) c = gc == c
matchGlobChar (Set str) c = elem c str
matchGlobChar AnyChar _ = True
matchGlobChar _ _ = False

matchWithoutAnyString :: GlobString -> String -> Maybe String
matchWithoutAnyString [] str = Just str
matchWithoutAnyString (g:gs) (s:str) = if matchGlobChar g s 
                                        then matchWithoutAnyString gs str
                                        else Nothing
matchWithoutAnyString _ _ = Nothing

firstMatchWithoutAnyString :: GlobString -> String -> Maybe String
firstMatchWithoutAnyString gs str = case matchWithoutAnyString gs str of
                                        Just remain -> Just remain
                                        Nothing -> if length str >= 1 
                                                    then firstMatchWithoutAnyString gs (tail str)
                                                    else Nothing

splitByAnyString :: GlobString -> (GlobString, GlobString)
splitByAnyString [] = ([], [])
splitByAnyString (g:gs) | g == AnyString = ([], g:gs)
                          | otherwise = let (pat, rm) = splitByAnyString gs in (g:pat, rm)

matchGlobString :: GlobString -> String -> Bool
matchGlobString [] str = length str == 0
matchGlobString [AnyString] _ = True
matchGlobString (AnyString:AnyString:gs) str = matchGlobString (AnyString:gs) str
matchGlobString (AnyString:gs) str = let (pat, rm) = splitByAnyString gs
                                        in (case firstMatchWithoutAnyString pat str of
                                                Just remain -> matchGlobString rm remain 
                                                Nothing -> False)
matchGlobString gs str = let (pat, rm) = splitByAnyString gs
                            in (case matchWithoutAnyString pat str of
                                    Just remain -> matchGlobString rm remain 
                                    Nothing -> False)




matchGlob :: GlobPattern -> String -> Bool
matchGlob gp str = matchGlobString (parseGlobString gp) str
