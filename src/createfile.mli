open Ezcurl
open Yojson
open Yojson.Basic.Util
open Http_common
open Orbit

val checkCreateFile (userId: int) (parentId: int) (fileTitle: string) (timestamp: string) (state: Orbit.system): bool 