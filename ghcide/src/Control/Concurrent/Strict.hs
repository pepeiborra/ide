module Control.Concurrent.Strict
    (Var
    ,modifyVar, modifyVar_
    ,readVar
    ,module Control.Concurrent.Extra
    ,newVar) where

import Control.Concurrent.Extra hiding (Var, modifyVar, modifyVar_, newVar, readVar)
import Data.Atomics
import Data.IORef

newtype Var a = Var {getVar :: IORef a}

newVar :: a -> IO (Var a)
newVar = fmap Var . newIORef

readVar :: Var a -> IO a
readVar = readIORef . getVar

modifyVar_ :: Var a -> (a -> a) -> IO ()
modifyVar_ (Var var) = atomicModifyIORefCAS_ var

modifyVar :: Var a -> (a -> (a, b)) -> IO b
modifyVar (Var var) = atomicModifyIORefCAS var

