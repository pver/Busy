module Busy

open System
open System.Text.RegularExpressions

let IsValidObjectPath path = 
    match path with
    | "" | null -> false
    | "/" -> true
    | x -> Regex.IsMatch(x, "^(/([A-Z]|[a-z]|[0-9]|_)+)+$")
