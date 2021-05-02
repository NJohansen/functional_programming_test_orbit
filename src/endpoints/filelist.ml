open Yojson
open Yojson.Basic.Util

type jsonData = {
  directoryVersions: directoryVersionsElement list;
  fileList: fileListElement list;
} [@@deriving show]

type directoryVersionsElement = {
  id: int;
  version: int;
} [@@deriving show]

type fileListElement = {
  id: int;
  name: string;
  parentId: int;
  version: int;
  versionChanged: int;
  timestamp: string;
} [@@deriving show]