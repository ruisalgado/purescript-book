module Data.PhoneBook where

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }

type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ entry.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons
 
findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

hasName :: String -> String -> Entry -> Boolean
hasName firstName lastName entry = entry.firstName == firstName && entry.lastName == lastName

findByName :: String -> String -> PhoneBook -> Maybe Entry
findByName firstName lastName = findEntry (hasName firstName lastName)

findByPhone :: String -> PhoneBook -> Maybe Entry
findByPhone phoneNumber = findEntry filterEntry
  where
  filterEntry entry = entry.phone == phoneNumber

hasEntryWithName :: String -> String -> PhoneBook -> Boolean
hasEntryWithName firstName lastName book = not $ null $ filter (hasName firstName lastName) book

removeDuplicates :: String -> String -> PhoneBook -> PhoneBook
removeDuplicates firstName lastName = nubBy sameName
  where
  sameName :: Entry -> Entry -> Boolean
  sameName one other = one.firstName == other.firstName && one.lastName == other.lastName
