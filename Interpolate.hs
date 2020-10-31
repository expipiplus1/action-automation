{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Interpolate
  ( i
  ) where

import           Control.Applicative            ( liftA2 )
import           Data.Char
import           GHC.TypeLits                   ( AppendSymbol )
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.ParserCombinators.ReadP

i :: QuasiQuoter
i = QuasiQuoter { quoteExp  = interpExp
                , quoteType = interpType
                , quoteDec  = error "No declaration quoting for i"
                , quotePat  = error "No pattern quoting for i"
                }

interpExp :: String -> Q Exp
interpExp =
  foldEither varOrConE (litE . stringL) (\e1 e2 -> [|$e1 <> $e2|]) . parse

interpType :: String -> Q Type
interpType =
  foldEither varOrConT (litT . strTyLit) (\t1 t2 -> [t|AppendSymbol $t1 $t2|])
    . parse

varOrConE :: String -> ExpQ
varOrConE n = (if isLower (head n) then varE else conE) . mkName $ n

varOrConT :: String -> TypeQ
varOrConT n = (if isLower (head n) then varT else conT) . mkName $ n

foldEither
  :: (Foldable t, Functor t)
  => (a -> c)
  -> (b -> c)
  -> (c -> c -> c)
  -> t (Either a b)
  -> c
foldEither l r f = foldr1 f . fmap (either l r)

type Var = String

-- >>> parse ""
-- []
--
-- >>> parse "hello $world"
-- [Right "hello ",Left "world"]
--
-- >>> parse "$hello$world"
-- [Left "hello",Left "world"]
--
-- >>> parse "$"
-- [Right "$"]
--
-- >>> parse "hi"
-- [Right "hi"]
--
-- >>> parse "h$hi"
-- [Right "h",Left "hi"]
--
-- >>> parse "$$hi"
-- [Right "$",Left "hi"]
--
-- >>> parse "$1"
-- [Right "$1"]
--
-- >>> parse "$$$"
-- [Right "$$$"]
--
-- >>> parse "\\"
-- [Right "\\"]
--
-- >>> parse "\\$"
-- [Right "$"]
--
-- >>> parse "\\$hi"
-- [Right "$hi"]
--
-- >>> parse "\\\\$hi"
-- [Right "\\$hi"]
--
-- >>> parse "\\hi"
-- [Right "\\hi"]
--
-- >>> parse "$hi\\$foo"
-- [Left "hi",Right "$foo"]
parse :: String -> [Either Var String]
parse s =
  let -- A haskell var or con
      ident = (:) <$> satisfy (isLower <||> isUpper <||> (== '_')) <*> munch
        (isAlphaNum <||> (== '\'') <||> (== '_'))
      -- parse a var, if that doesn't work out return just the '$'
      var    = char '$' *> ((Left <$> ident) <++ pure (Right "$"))
      -- Everything up to a $
      normal = Right <$> munch1 (/= '$')
      -- One normal or var
      -- - Check escaped '$' first
      -- - variables, starting with $
      -- - normal string
      one    = escape <++ var <++ normal
      -- escape a $
      escape = char '\\' *> ((Right <$> string "$") <++ pure (Right "\\"))
      parser = many one <* eof
  in  case readP_to_S parser s of
        [(r, "")] -> foldr mergeRights [] r
        _         -> error "Failed to parse string"

mergeRights :: Either Var String -> [Either Var String] -> [Either Var String]
mergeRights = \case
  Left  v -> (Left v :)
  Right n -> \case
    (Right m : xs) -> Right (n <> m) : xs
    xs             -> Right n : xs

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
