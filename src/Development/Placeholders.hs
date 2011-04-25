{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Development.Placeholders (
    PlaceholderException(..),
    placeholder,
    placeholderNoWarning,
    notImplemented,
    todo
) where

import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Language.Haskell.TH (Q, Exp, Loc(..), litE, stringL, location, report)

-- | Thrown when attempting to evaluate a placeholder at runtime.
data PlaceholderException = PlaceholderException String
    deriving (Show, Typeable)

instance Exception PlaceholderException

-- | Shorthand for @placeholder "Unimplemented feature"@
notImplemented :: Q Exp
notImplemented = placeholder "Unimplemented feature"

-- | Shorthand for @placeholder ("TODO: " ++ msg)@
todo :: String -> Q Exp
todo msg = placeholder $ "TODO: " ++ msg

-- | Generates an expression of any type that, if evaluated at runtime will throw
-- a 'PlaceholderException'. It is therefore similar to 'undefined' or 'error',
-- except that this clearly communicates that this is temporary, and the source
-- location is automatically included. Also, a warning is generated at compile time
-- so you won't forget to replace placeholders before shipping.
placeholder :: String -> Q Exp
placeholder msg = do
    emitWarning msg
    placeholderNoWarning msg

-- | Similar to 'placeholder', but does not generate a compiler warning. Use
-- with care!
placeholderNoWarning :: String -> Q Exp
placeholderNoWarning msg = do
    runtimeMsg <- formatMessage msg `fmap` location
    [| throw $ PlaceholderException $(litE $ stringL runtimeMsg) |]

emitWarning :: String -> Q ()
emitWarning msg = report False $ msg

formatMessage :: String -> Loc -> String
formatMessage msg loc = msg ++ " at " ++ formatLoc loc

formatLoc :: Loc -> String
formatLoc loc = let file = loc_filename loc
                    (line, col) = loc_start loc
                in concat [file, ":", show line, ":", show col]

