{-# LANGUAGE TemplateHaskell #-}
module ProgramState where

import Control.Lens

data PState = PState { _ra   :: !Int
                     , _rb   :: !Int
                     , _rins :: !Int } deriving Show

$(makeLenses ''PState)
