import Lean.Data.RBMap
open Lean

structure Coin where
  minter : String
  balances : RBMap String Float (fun s1 s2 => compareOfLessAndEq s1 s2)

instance : ToString Coin where
  toString coin := toString coin.balances.toList

def newCoin (minter : String) : Coin :=
  { minter := minter, balances := RBMap.empty }

def mint (coin : Coin) (receiver : String) (amount : Float) : Coin :=
  match coin.balances.find? receiver with
  | .some balance => { coin with balances := coin.balances.insert receiver (balance + amount) }
  | _ => { coin with balances := coin.balances.insert receiver amount }

def send (coin : Coin) (sender : String) (receiver : String) (amount : Float) : Except String Coin :=
  match coin.balances.find? sender with
  | .none => .error "failed to send: sender not found"
  | .some senderBalance =>
    if senderBalance < amount then .error "failed to send: insufficient funds" else
    let coin := { coin with balances := coin.balances.insert sender (senderBalance - amount) }
    match coin.balances.find? receiver with
    | .some balance => .ok { coin with balances := coin.balances.insert receiver (balance+amount) }
    | _ => .ok { coin with balances := coin.balances.insert receiver amount }

def main : IO Unit := do
  let coin := newCoin "minter"
  let coin := mint coin "user1" 1.0
  let coin := mint coin "user2" 2.0
  match send coin "user1" "user2" 0.2 with
  | .ok coin => IO.println s!"{coin}"
  | .error err => IO.println s!"{err}"
