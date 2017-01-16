

type error =
  | Archive_failed      of string
  | UnknownError        of string
[@@deriving rpcty]

exception Error of error

type t = {
  name : string;
  description : string;
  enabled : bool;
  standard : bool;
  min : float;
  max : float;
  units : string
} [@@deriving rpcty]

type plugin_protocol    = V1 | V2 [@@deriving rpcty]
type sampling_frequency = Five_Seconds [@@deriving rpcty]

module Statefile_latency = struct
  type t = {id: string; latency: float option} [@@deriving rpcty]
end

type interdomain_uid = {
  name: string;
  frontend_domid: int;
} [@@deriving rpcty]

type interdomain_info = {
  frequency: sampling_frequency;
  shared_page_refs: int list;
} [@@deriving rpcty]


let raiser = function
  | error -> raise (Error error)

let matcher = function
  | Error(e)  -> Some (e)
  | e         -> Some (UnknownError(Printexc.to_string e))

let rpc_error = Idl.Error.
              { def     = error
              ; raiser  = raiser
              ; matcher = matcher
              }

(* Shared constants like file paths and so on *)

let service_name = "rrd"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let (//) = Filename.concat
let default_sockets_dir = "/var/lib/xcp"
let daemon_name     = "xcp-rrdd"
let default_path    = ref (default_sockets_dir // daemon_name)
let forwarded_path  =
  ref (default_sockets_dir // daemon_name ^ ".forwarded")

let set_sockets_dir x =
  default_path := x // daemon_name;
  forwarded_path := !default_path ^ ".forwarded"

let uri () = "file:" ^ !default_path


module API(R: Idl.RPC) = struct

  module T = struct (* RPC call parameter type representation *)

    (* base types *)
    let int     = Idl.Param.mk Rpc.Types.int
    let int32   = Idl.Param.mk Rpc.Types.int32
    let int64   = Idl.Param.mk Rpc.Types.int64
    let bool    = Idl.Param.mk Rpc.Types.bool
    let float   = Idl.Param.mk Rpc.Types.float
    let string  = Idl.Param.mk Rpc.Types.string
    let char    = Idl.Param.mk Rpc.Types.char
    let unit    = Idl.Param.mk Rpc.Types.unit

    let option ~name ~description ty = Idl.Param.mk
        ~name
        ~description
        Rpc.Types.
          { name = name
          ; description = description
          ; ty = Option ty
          }

    let list ~name ~description ty = Idl.Param.mk
        ~name
        ~description
        Rpc.Types.
          { name = name
          ; description = description
          ; ty = List ty
          }


    let uuid x = Idl.Param.mk
        ~name:x
        ~description:[Printf.sprintf "The uuid of a %s" x]
        Rpc.Types.string

    let host_uuid = uuid "host"
    let sr_uuid   = uuid "sr"
    let vm_uuid   = uuid "vm"
    let rrd_uuid  = uuid "rrd"

    let ds_name = Idl.Param.mk
        ~name:"ds_name"
        ~description:["Name of a data source"]
        Rpc.Types.string

    let domid = Idl.Param.mk
        ~name:"domid"
        ~description:["domain id"]
        Rpc.Types.int

    let address = Idl.Param.mk
        ~name:"address"
        ~description:["remote address"]
        Rpc.Types.string

    let address_opt = option
        ~name:"address"
        ~description:["remote address"]
        Rpc.Types.(Basic String)

    let path = Idl.Param.mk
        ~name:"path"
        ~description:["file path"]
        Rpc.Types.string

    let session_id = Idl.Param.mk
        ~name:"session_id"
        ~description:["session id"]
        Rpc.Types.string

    let datasources = list
        ~name:"data_sources"
        ~description:["RRD data sources"]
        Rpc.Types.(t.ty)

    let memory = Idl.Param.mk
        ~name:"memory"
        ~description:["memory size"]
        Rpc.Types.int64

    let plugin_id = Idl.Param.mk
        ~name:"plugin_id"
        ~description:["plugin id"]
        Rpc.Types.string

    let protocol = Idl.Param.mk
        ~name:"plugin_protocol"
        ~description:["RRD plugin protocol"]
        plugin_protocol

    let sampling_frequency = Idl.Param.mk
        ~name:"sampling_freq"
        ~description:["RRD sampling frequency"]
        sampling_frequency

    let interdomain_id = Idl.Param.mk
        ~name:"interdomain_id"
        ~description:["RRD interdomain ID"]
        interdomain_uid

    let interdomain_info = Idl.Param.mk
        ~name:"interdomain_info"
        ~description:["RRD interdomain details"]
        interdomain_info
  end (* T *)

  (* declare RPC function using the types defined above *)

  let has_vm_rrd = R.declare
      "has_vm_rrd"
      ["True if we have RRD data for the given VM"]
      R.(T.vm_uuid @-> returning T.bool rpc_error)

  let push_rrd_local = R.declare
      "push_rrd_local"
      []
      R.(T.vm_uuid @-> T.domid @-> returning T.unit rpc_error)

  let push_rrd_remote = R.declare
      "push_rrd_remote"
      ["push_rrd_remote"]
      R.(T.vm_uuid @-> T.address @-> returning T.unit rpc_error)

  let remove_rrd = R.declare
      "remove_rrd"
      ["remove_rrd"]
      R.(T.rrd_uuid @-> returning T.unit rpc_error)

  let migrate_rrd = R.declare
      "migrate_rrd"
      ["migrate_rrd"]
      R.(T.session_id
         @-> T.address
         @-> T.vm_uuid
         @-> T.host_uuid
         @-> returning T.unit rpc_error
        )

  let send_host_rrd_to_master = R.declare
      "send_host_rrd_to_master"
      ["send_host_rrd_to_master"]
      R.(T.address @-> returning T.unit rpc_error)

  let backup_rrds = R.declare
      "backup_rrds"
      ["backup_rrds"]
      R.(T.address_opt @-> T.unit @-> returning T.unit rpc_error)

  let archive_rrd = R.declare
      "archive_rrd"
      ["archive_rrd"]
      R.(T.vm_uuid @-> T.address_opt @-> returning T.unit rpc_error)

  let archive_sr_rrd = R.declare
      "archive_sr_rrd"
      ["archive_sr_rrd"]
      R.(T.sr_uuid @-> returning T.string rpc_error)

  let push_sr_rrd = R.declare
      "push_sr_rrd"
      ["push_sr_rrd"]
      R.(T.sr_uuid @-> T.path @-> returning T.unit rpc_error)

  let add_host_ds = R.declare
      "add_host_ds"
      ["add_host_ds"]
      R.(T.ds_name @-> returning T.unit rpc_error)

  let forget_host_ds = R.declare
      "forget_host_ds"
      ["forget_host_ds"]
      R.(T.ds_name @-> returning T.unit rpc_error)

  let query_possible_host_dss = R.declare
      "query_possible_host_dss"
      ["query_possible_host_dss"]
      R.(T.unit @-> returning T.datasources rpc_error)

  let query_host_ds = R.declare
      "query_host_ds"
      ["query_host_ds"]
      R.(T.ds_name @-> returning T.float rpc_error)

  let add_vm_ds = R.declare
      "add_vm_ds"
      ["add_vm_ds"]
      R.(T.vm_uuid
         @-> T.domid
         @-> T.ds_name
         @-> returning T.unit rpc_error
        )

  let forget_vm_ds = R.declare
      "forget_vm_ds"
      ["forget_vm_ds"]
      R.(T.vm_uuid @-> T.ds_name @-> returning T.unit rpc_error)

  let query_possible_vm_dss = R.declare
      "query_possible_vm_dss"
      ["query_possible_vm_dss"]
      R.(T.vm_uuid @-> returning T.datasources rpc_error)

  let query_vm_ds = R.declare
      "query_vm_ds"
      ["query_vm_ds"]
      R.(T.vm_uuid @-> T.ds_name @-> returning T.float rpc_error)

  let add_sr_ds = R.declare
      "add_sr_ds"
      ["add_sr_ds"]
      R.(T.sr_uuid @-> T.ds_name @-> returning T.unit rpc_error)

  let forget_sr_ds = R.declare
      "forget_sr_ds"
      ["forget_sr_ds"]
      R.(T.sr_uuid @-> T.ds_name @-> returning T.unit rpc_error)

  let query_possible_sr_dss = R.declare
      "query_possible_sr_dss"
      ["query_possible_sr_dss"]
      R.(T.sr_uuid @-> returning T.datasources rpc_error)

  let query_sr_ds = R.declare
      "query_sr_ds"
      ["query_sr_ds"]
      R.(T.sr_uuid @-> T.ds_name @-> returning T.float rpc_error)

  let update_use_min_max = R.declare
      "update_use_min_max"
      ["update_use_min_max"]
      R.(T.bool @-> returning T.unit rpc_error)

  let update_vm_memory_target = R.declare
      "update_vm_memory_target"
      ["update_vm_memory_target"]
      R.(T.domid @-> T.memory @-> returning T.unit rpc_error)

  let set_cache_sr = R.declare
      "set_cache_sr"
      ["set_cache_sr"]
      R.(T.sr_uuid @-> returning T.unit rpc_error)

  let unset_cache_sr = R.declare
      "unset_cache_sr"
      ["unset_cache_sr"]
      R.(T.unit @-> returning T.unit rpc_error)

  module Plugin = struct
    let get_header = R.declare
        "get_header"
        ["get_header"]
        R.(T.unit @-> returning T.string rpc_error)

    let get_path = R.declare
        "get_path"
        ["get_path"]
        R.(T.plugin_id @-> returning T.string rpc_error)

    module Local = struct
      let register = R.declare
          "register"
          ["register"]
          R.(T.plugin_id
             @-> T.sampling_frequency
             @-> T.protocol
             @-> returning T.float rpc_error
            )

      let deregister = R.declare
          "deregister"
          ["deregister"]
          R.(T.plugin_id @-> returning T.unit rpc_error)

      let next_reading = R.declare
          "next_reading"
          ["next_reading"]
          R.(T.plugin_id @-> returning T.float rpc_error)

    end

    module Interdomain = struct
      let register = R.declare
          "register"
          ["register"]
          R.(T.interdomain_id
             @-> T.interdomain_info
             @-> T.protocol
             @-> returning T.float rpc_error
            )

      let deregister = R.declare
          "deregister"
          ["deregister"]
          R.(T.interdomain_id @-> returning T.unit rpc_error)

      let next_reading = R.declare
          "next_reading"
          ["next_reading"]
          R.(T.interdomain_id @-> returning T.float rpc_error)
    end

    let register = R.declare
        "register"
        ["register"]
        R.(T.plugin_id
           @-> T.sampling_frequency
           @-> returning T.float rpc_error
          )

    let deregister = R.declare
        "deregister"
        ["deregister"]
        R.(T.plugin_id @-> returning T.unit rpc_error)

    let next_reading = R.declare
        "next_reading"
        ["next_reading"]
        R.(T.plugin_id @-> returning T.float rpc_error)
  end
  module HA = struct
    let enable_and_update = R.declare
        "enable_and_update"
        ["enable_and_update"]
        R.(T.list
             ~name:"statefile_latencies"
             ~description:["statefile latencies"]
             Rpc.Types.(Statefile_latency.t.ty)
           @-> Idl.Param.mk
             ~name:"heartbeat_latency"
             ~description:["heartbeat latency"]
             Rpc.Types.float
           @-> Idl.Param.mk
             ~name:"xapi_latency"
             ~description:["xapi latency"]
             Rpc.Types.float
           @-> returning T.unit rpc_error
          )

    let disable = R.declare
        "disable"
        ["disable"]
        R.(T.unit @-> returning T.unit rpc_error)
  end

  module Deprecated = struct
    let load_rrd = R.declare
        "load_rrd"
        ["load_rrd"]
        R.(T.plugin_id
           @-> Idl.Param.mk
             ~name:"timescale"
             ~description:["timescale"]
             Rpc.Types.int
           @-> T.option
             ~name:"address"
             ~description:["master address"]
             Rpc.Types.(Basic String)
           @-> returning T.unit rpc_error
          )
  end

end (* API *)


