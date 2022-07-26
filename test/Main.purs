module Test.Main where

import Prelude

import Control.Monad.Writer (class MonadTell, WriterT, execWriterT, tell)
import Data.Identity (Identity(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Node.Process as Process
import Stylish.Record (class Attrable, attrsM)
import Stylish.Types (Classy, Stylish)

testStylish :: forall i f. Attrable i Identity Stylish => MonadEffect f => MonadTell (Additive Int) f => i -> String -> f Unit
testStylish a v =
  let v' = show $ unwrap (attrsM a :: Identity Stylish) in
  if v' == v then log (show v') else do
    tell (Additive 1)
    log $ show v' <> " /= " <> show v
testClassy :: forall i f. Attrable i Identity Classy => MonadEffect f => MonadTell (Additive Int) f => i -> String -> f Unit
testClassy a v =
  let v' = show $ unwrap (attrsM a :: Identity Classy) in
  if v' == v then log (show v') else do
    tell (Additive 1)
    log $ show v' <> " /= " <> show v

main :: Effect Unit
main = do
  Additive failed <- execWriterT tests
  when (failed > 0) do
    log $ show failed <> " failed"
    Process.exit 1

tests :: WriterT (Additive Int) Effect Unit
tests = do
  testStylish {} ""
  testStylish {"a": false} ""
  testStylish {"a": Identity false} ""
  testStylish {"a": unit} "a"
  testStylish {"a": true} "a"
  testStylish {"a": Identity true} "a"
  testStylish {"k": "v"} "k:v"
  testStylish {"k": [ "v1", "v2" ]} "k:v1;k:v2"
  testStylish {"k": { "v1": unit, "v2": unit }} "k:v1;k:v2"
  testStylish {"k1": {"k2": [ "v1", "v2" ]}} "k1:k2:v1;k1:k2:v2"

  testClassy {} ""
  testClassy {"a": false} ""
  testClassy {"a": Identity false} ""
  testClassy {"a": unit} "a"
  testClassy {"a": true} "a"
  testClassy {"a": Identity true} "a"
  testClassy {"k": "v"} "k-v"
  testClassy {"k": [ "v1", "v2" ]} "k-v1 k-v2"
  testClassy {"k": { "v1": unit, "v2": unit }} "k-v1 k-v2"
  testClassy {"k1": {"k2": [ "v1", "v2" ]}} "k1-k2-v1 k1-k2-v2"
