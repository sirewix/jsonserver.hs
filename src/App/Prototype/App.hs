{-# LANGUAGE
    MultiParamTypeClasses
  , AllowAmbiguousTypes
  , FlexibleInstances
  #-}

module App.Prototype.App where

class HasEnv e m where
  getEnv :: m e
