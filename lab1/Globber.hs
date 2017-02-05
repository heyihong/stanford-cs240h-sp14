module Globber (matchGlob) where

type GlobPattern = String

data GlobChar = Literal Char
            |   OneChar String
            |   AnyChar
            |   AnyString deriving (Show, Eq)

type GlobString = [GlobChar]

parseGlobString :: GlobPattern -> GlobString
parseGlobString [] = []
parseGlobString ('*':cs) = AnyString:parseGlobString cs
parseGlobString ('?':cs) = AnyChar:parseGlobString cs
parseGlobString ('[':cs) = let (s1, s2) = findParen cs in (OneChar (convertDash s1)):parseGlobString s2
    where findParen [] = error "Invalid bracket ["
          findParen (']':xs) = ("", xs)
          findParen ('\\':x:xs) = let (s1, s2) = findParen xs in (x:s1, s2)
          findParen (x:xs) = let (s1, s2) = findParen xs in (x:s1, s2)
          convertDash [] = []
          convertDash (l:'-':r:xs) = [l..r] ++ (convertDash xs)
          convertDash (x:xs) = x:(convertDash xs)
parseGlobString ('\\':c:cs) = Literal c:parseGlobString cs
parseGlobString (c:cs) = Literal c:parseGlobString cs

matchGlobChar :: GlobChar -> Char -> Bool
matchGlobChar (Literal gc) c = gc == c
matchGlobChar (OneChar str) c = elem c str
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
