open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit
open Util

val checkGetListOfFiles (userId: int) (state: Orbit.system): bool