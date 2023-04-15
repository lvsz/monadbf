{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Originally by Thomas Winant:
-- https://github.com/mrBliss/ghc/blob/explicit-dicts-8.4/DictUtils.hs
--
-----------------------------------------------------------------------------

module DictUtils ( HasDict(..) ) where

import GHC.Base ( Functor(..), Applicative(..), Monad(..) )
import GHC.Exts ( Constraint )

class HasDict (c :: Constraint) where
    type Dict c :: *
    getDict :: c => Dict c

instance HasDict (Functor f) where
    type Dict (Functor f) = Functor.Dict f
    getDict = Functor.Dict
        { fmap = fmap
        , (<$) = (<$)
        }

instance HasDict (Applicative f) where
    type Dict (Applicative f) = Applicative.Dict f
    getDict = Applicative.Dict
        { parent1 = getDict @(Functor f)
        , pure = pure
        , (<*>) = (<*>)
        , (<*) = (<*)
        , (*>) = (*>)
        , liftA2 = liftA2
        }

instance HasDict (Monad m) where
    type Dict (Monad m) = Monad.Dict m
    getDict = Monad.Dict
        { parent1 = getDict @(Applicative m)
        , (>>=) = (>>=)
        , (>>) = (>>)
        , return = return
        , fail = fail
        }

