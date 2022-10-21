module Test where

True && x = x
_    && _ = False

and True True = True
and _    _    = False
