module Hydra.Modelv1.Initial where

import Data.List ((!!))
import Hydra.Prelude

data Datum

data Policy

data Token

data Address

data Ref

type Value = [(Policy, Token, Int)]

instance Num Value

data TxIn = TxIn {outRef :: Ref}

data TxOut = TxOut
  { delta :: Datum
  , value :: Value
  , nu :: Address
  }

o :: [TxOut]
o = _

i :: [(TxIn, TxOut)]
i = _

omega_ctx :: TxOut
omega_ctx = _

rho_ctx :: [Ref]
rho_ctx = _

initial ("context") = do
  -- First option: spent by Commit Tx
  (pid, val, pkh, val', nu', pid', eta', oMega) <- parse $ do
    pid <- omega_ctx & delta
    val <- omega_ctx & value
    pkh <- [pkh | (p, pkh, 1) <- val, pid == p] !! 1
    (val', nu', (pid', eta')) <- o !! 1

    ci <- rho_ctx -- List of outRefs pointing to committed inputs
    -- Needed to tell committed inputs apart from fee inputs

    -- Omega is a list of all committed (val,nu,delta)
    -- The ordering in Omega corresponds to the ordering in CI
    -- (which is assumed implicitly in the next line)
    oMega <- [omega | (i_j, omega) <- i, (i_j & outRef) `elem` ci]
    return (pid, val, pkh, val', nu', pid', eta', oMega)
  check
    [ -- PT is there
      (pid, pkh, 1) `elem` val'
    , -- Committed value in output
      val' - (pid, pkh, 1) == val + sum [omega & value | omega <- oMega]
    , -- Correct script
      nu' == nu_commit
    , -- Correct PID
      pid' == pid
    , -- Committed UTxOs recorded correctly
      eta' == recordOutputs (oMega)
    , -- Authorization
      pks == [pkh]
    , -- No forge/burn
      mint == 0
    ]

pks :: [a6]
pks = _

check :: [Bool] -> m b
check = _

recordOutputs :: [a7] -> a4
recordOutputs = _
