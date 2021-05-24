# functional_programming_test_orbit

Start docker image
```
docker run --rm -p8085:8085 -eCLICOLOR_FORCE=1 --name orbit cr.orbit.dev/sdu/filesync-server:latest
```

```
#require "ezcurl";;
#require "yojson";;
#require "qcheck";;
#require "qcstm";;
#require "unix";;
#mod_use "util.ml";;
#mod_use "orbit.ml";;
#mod_use "http_common.ml";;

#mod_use "filelist.ml";;
#mod_use "getfile.ml";;
#mod_use "deletedir.ml";;

#require "ppx_deriving.show";;  is breaks orbit.ml

#use  "machine.ml";;
```

```
make
./machine.native
```