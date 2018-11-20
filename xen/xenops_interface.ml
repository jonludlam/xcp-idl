(*
 * Copyright (C) 2011 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(*
 * @group Xenops
 *)

open Rpc
open Idl

module D = Debug.Make (struct
  let name = "xenops_interface"
end)

open D

module TypeCombinators = struct
  let option ?name ?(description= []) d =
    let open Rpc.Types in
    let name =
      match name with Some n -> n | None -> Printf.sprintf "%s option" d.name
    in
    {name; description; ty= Option d.ty}

  let list ?name ?(description= []) d =
    let open Rpc.Types in
    let name =
      match name with
      | Some n -> n
      | None -> Printf.sprintf "list of %ss" d.name
    in
    {name; description; ty= List d.ty}

  let pair ?name ?(description= []) (p1, p2) =
    let open Rpc.Types in
    let name =
      match name with
      | Some n -> n
      | None -> Printf.sprintf "pair of %s and %s" p1.name p2.name
    in
    {name; description; ty= Tuple (p1.ty, p2.ty)}

  let triple ?name ?(description= []) (p1, p2, p3) =
    let open Rpc.Types in
    let name =
      match name with
      | Some n -> n
      | None ->
          Printf.sprintf "triple of %s, %s and %s" p1.name p2.name p3.name
    in
    {name; description; ty= Tuple3 (p1.ty, p2.ty, p3.ty)}
end

include Xenops_types

let service_name = "xenops"

let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"

let default_path = ref (Filename.concat default_sockets_dir "xenopsd")

let forwarded_path =
  ref (Filename.concat default_sockets_dir "xenopsd" ^ ".forwarded")

let set_sockets_dir x =
  default_path := Filename.concat x "xenopsd" ;
  forwarded_path := !default_path ^ ".forwarded"

let default_uri () = "file:" ^ !default_path

let json_url () = Printf.sprintf "file:%s.json" !default_path

module Errors = struct
  type error =
    | Already_exists of (string * string)
    | Does_not_exist of (string * string)
    | Unimplemented of string
    | Domain_not_built
    | Invalid_vcpus of int
    | Bad_power_state of (power_state * power_state)
    | Failed_to_acknowledge_shutdown_request
    | Failed_to_shutdown of (string * float)
    | Device_is_connected
    | Device_not_connected
    | Device_detach_rejected of (string * string * string)
    | Media_not_ejectable
    | Media_present
    | Media_not_present
    | No_bootable_device
    | Bootloader_error of (string * string)
    | Cannot_free_this_much_memory of (int64 * int64)
    | Vms_failed_to_cooperate of string list
    | IO_error
    | Failed_to_contact_remote_service of string
    | Hook_failed of (string * string * string * string)
    | Not_enough_memory of int64
    | Cancelled of string
    | Storage_backend_error of (string * string list)
    | PCIBack_not_loaded
    | Failed_to_run_script of string
    | Failed_to_start_emulator of (string * string * string)
    | Ballooning_timeout_before_migration
    | Internal_error of string
    | Unknown_error [@@default Unknown_error] [@@deriving rpcty]
end

exception Xenopsd_error of Errors.error

let () =
  (* register printer *)
  let sprintf = Printf.sprintf in
  let string_of_error e =
    Rpcmarshal.marshal Errors.error.Rpc.Types.ty e |> Rpc.to_string
  in
  let printer = function
    | Xenopsd_error e ->
        Some
          (sprintf "Xenops_interface.Xenopsd_error(%s)" (string_of_error e))
    | _ -> None
  in
  Printexc.register_printer printer

let err =
  let open Error in
  { def= Errors.error
  ; raiser=
      (fun e ->
        let exn = Xenopsd_error e in
        error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
        raise exn )
  ; matcher=
      (function
        | Xenopsd_error e as exn ->
            error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
            Some e
        | exn ->
            error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
            Some (Internal_error (Printexc.to_string exn))) }

type debug_info = string [@@deriving rpcty]

module Query = struct
  type t =
    { name: string
    ; vendor: string
    ; version: string
    ; features: string list
    ; instance_id: string
    (* Unique to this invocation of xenopsd *) }
  [@@deriving rpcty]
end


(* XXX: this code shouldn't care about the vswitch/bridge difference *)


module XenopsAPI (R : RPC) = struct
  open R

  let description =
    let open Interface in
    { name= "Xen"
    ; namespace= None
    ; description= ["This interface is used by xapi to talk to xenopsd"]
    ; version= (1, 0, 0) }

  let implementation = implement description

  let debug_info_p =
    Param.mk
      ~description:["an uninterpreted string to associate with the operation."]
      ~name:"debug_info" Types.string

  let unit_p = Param.mk ~name:"unit" Types.unit

  let query =
    let query_p = Param.mk Query.t in
    declare "query"
      ["Query some information specific to the backend we're talking to"]
      (debug_info_p @-> unit_p @-> returning query_p err)

  let get_diagnostics =
    let result_p = Param.mk Rpc.Types.string in
    declare "get_diagnostics"
		  ["Get diagnostics information from the backend"]
      (debug_info_p @-> unit_p @-> returning result_p err)

  module TASK = struct
    let task_id_p =
      Param.mk ~description:["Task identifier"] ~name:"id" Task.id

    let task_t_p =
      Param.mk ~description:["The state of the task"] ~name:"task" Task.t

    let task_list_p =
      Param.mk ~description:["A list of task states"] ~name:"task_list"
        Task.t_list

    let stat =
      declare "Task.stat" ["Get the state of the task"]
        (debug_info_p @-> task_id_p @-> returning task_t_p err)

    let cancel =
      declare "Task.cancel" ["Cancel a task"]
        (debug_info_p @-> task_id_p @-> returning unit_p err)

    let destroy =
      declare "Task.destroy" ["Destroy a task"]
        (debug_info_p @-> task_id_p @-> returning unit_p err)

    let list =
      declare "Task.list" ["List all the current tasks"]
        (debug_info_p @-> returning task_list_p err)
  end

  module HOST = struct
    let host_t_p =
      Param.mk ~description:["The state of the host"] ~name:"host"
        Host.t

    let console_data_p =
      Param.mk ~description:["The console data"] ~name:"console_data"
        Types.string

    let memory_p =
      Param.mk ~description:["The total memory"] ~name:"total_memory"
        Types.int64

    let debug_keys_p =
      Param.mk ~description:["The debug keys"] ~name:"debug_keys" Types.string

    let pool_size_p =
      Param.mk ~description:["The size of the worker pool"] ~name:"pool_size"
        Types.int

    let feature_list_p =
      Param.mk ~description:["The list of features"] ~name:"features"
        Host.guest_agent_feature_list

    type cpu_features_array = int64 array [@@deriving rpcty]

    let cpu_features_array_p =
      Param.mk ~description:["An array containing the raw CPU feature flags"]
        ~name:"features_array" cpu_features_array

    let stat =
      declare "HOST.stat" ["Get the state of the host"]
        (debug_info_p @-> returning host_t_p err)

    let get_console_data =
      declare "HOST.get_console_data" ["Get the console data of the host"]
        (debug_info_p @-> returning console_data_p err)

    let get_total_memory_mib =
      declare "HOST.get_total_memory_mib" ["Get the total memory of the host"]
        (debug_info_p @-> returning memory_p err)

    let send_debug_keys =
      declare "HOST.send_debug_keys" []
        (debug_info_p @-> debug_keys_p @-> returning unit_p err)

    let set_worker_pool_size =
      declare "HOST.set_worker_pool_size" []
        (debug_info_p @-> pool_size_p @-> returning unit_p err)

    let update_guest_agent_features =
      declare "HOST.update_guest_agent_features" []
        (debug_info_p @-> feature_list_p @-> returning unit_p err)

    let upgrade_cpu_features =
      let is_hvm_p = Param.mk ~name:"is_hvm" Types.bool in
      declare "HOST.upgrade_cpu_features" []
        ( debug_info_p @-> cpu_features_array_p @-> is_hvm_p
        @-> returning cpu_features_array_p err )
  end

  module VM = struct
    let vm_t_p = Param.mk ~name:"vm" Vm.t

    let vm_id_p = Param.mk ~name:"id" Vm.id

    let task_id_p = Param.mk ~name:"task" Task.id

    open TypeCombinators

    let add =
      declare "VM.add" ["Add VM metadata"]
        (debug_info_p @-> vm_t_p @-> returning vm_id_p err)

    let remove =
      declare "VM.remove" ["Remove VM metadata"]
        (debug_info_p @-> vm_id_p @-> returning unit_p err)

    let generate_state_string =
      declare "VM.generate_state_string" []
        (debug_info_p @-> vm_t_p @-> returning (Param.mk Types.string) err)

    let migrate =
      let vdimap =
        Param.mk ~name:"vdi_map" ~description:["Map of src VDI -> dest VDI"]
          (list (pair (Types.string, Types.string)))
      in
      let vifmap =
        Param.mk ~name:"vif_map" ~description:["Map of src VIF -> dest network"]
          (list (pair (Types.string, Network.t)))
      in
      let pcimap =
        Param.mk ~name:"pci_map" ~description:["Map of src PCI -> dest PCI"]
          (list (pair (Types.string, Pci.address)))
      in
      let xenops_url =
        Param.mk ~name:"xenops_url"
          ~description:["URL on which the remote xenopsd can be contacted"]
          Types.string
      in
      declare "VM.migrate" []
        ( debug_info_p @-> vm_id_p @-> vdimap @-> vifmap @-> pcimap
        @-> xenops_url @-> returning task_id_p err )

    let create =
      declare "VM.create" []
        (debug_info_p @-> vm_id_p @-> returning task_id_p err)

    let build =
      let force_p = Param.mk ~name:"force" Types.bool in
      declare "VM.build" []
        (debug_info_p @-> vm_id_p @-> force_p @-> returning task_id_p err)

    let create_device_model =
      let save_state_p = Param.mk ~name:"save_state" Types.bool in
      declare "VM.create_device_model" []
        (debug_info_p @-> vm_id_p @-> save_state_p @-> returning task_id_p err)

    let destroy =
      declare "VM.destroy" []
        (debug_info_p @-> vm_id_p @-> returning task_id_p err)

    let pause =
      declare "VM.pause" []
        (debug_info_p @-> vm_id_p @-> returning task_id_p err)

    let unpause =
      declare "VM.unpause" []
        (debug_info_p @-> vm_id_p @-> returning task_id_p err)

    let request_rdp =
      let enabled_p = Param.mk ~name:"enabled" Types.bool in
      declare "VM.request_rdp" []
        (debug_info_p @-> vm_id_p @-> enabled_p @-> returning task_id_p err)

    let run_script =
      declare "VM.run_script" []
        ( debug_info_p @-> vm_id_p
        @-> Param.mk ~name:"script" Types.string
        @-> returning task_id_p err )

    let set_xsdata =
      declare "VM.set_xsdata" []
        ( debug_info_p @-> vm_id_p
        @-> Param.mk ~name:"xsdata" (list (pair (Types.string, Types.string)))
        @-> returning task_id_p err )

    let set_vcpus =
      declare "VM.set_vcpus" []
        ( debug_info_p @-> vm_id_p
        @-> Param.mk ~name:"vcpus" Types.int
        @-> returning task_id_p err )

    let set_shadow_multiplier =
      declare "VM.set_shadow_multiplier" []
        ( debug_info_p @-> vm_id_p
        @-> Param.mk ~name:"multiplier" Types.float
        @-> returning task_id_p err )

    let set_memory_dynamic_range =
      let min_p = Param.mk ~name:"minimum" Types.int64 in
      let max_p = Param.mk ~name:"maximum" Types.int64 in
      declare "VM.set_memory_dynamic_range" []
        ( debug_info_p @-> vm_id_p @-> min_p @-> max_p
        @-> returning task_id_p err )

    let stat =
      let stat_p = Param.mk (pair (Vm.t, Vm.state)) in
      declare "VM.stat" [] (debug_info_p @-> vm_id_p @-> returning stat_p err)

    let exists =
      declare "VM.exists" []
        (debug_info_p @-> vm_id_p @-> returning (Param.mk Types.bool) err)

    let list =
      let stat_list_p = Param.mk (list (pair (Vm.t, Vm.state))) in
      declare "VM.list" []
        (debug_info_p @-> unit_p @-> returning stat_list_p err)

    let delay =
      declare "VM.delay" []
        ( debug_info_p @-> vm_id_p
        @-> Param.mk ~name:"delay" Types.float
        @-> returning task_id_p err )

    let start =
      let paused_p = Param.mk ~name:"paused" Types.bool in
      declare "VM.start" []
        (debug_info_p @-> vm_id_p @-> paused_p @-> returning task_id_p err)

    let shutdown =
      let delay_p = Param.mk ~name:"delay" (option Types.float) in
      declare "VM.shutdown" []
        (debug_info_p @-> vm_id_p @-> delay_p @-> returning task_id_p err)

    let reboot =
      let delay_p = Param.mk ~name:"delay" (option Types.float) in
      declare "VM.reboot" []
        (debug_info_p @-> vm_id_p @-> delay_p @-> returning task_id_p err)

    let suspend =
      let disk_p = Param.mk ~name:"suspend_vdi" disk in
      declare "VM.suspend" []
        (debug_info_p @-> vm_id_p @-> disk_p @-> returning task_id_p err)

    let resume =
      let disk_p = Param.mk ~name:"suspend_vdi" disk in
      declare "VM.resume" []
        (debug_info_p @-> vm_id_p @-> disk_p @-> returning task_id_p err)

    let s3suspend =
      declare "VM.s3suspend" []
        (debug_info_p @-> vm_id_p @-> returning task_id_p err)

    let s3resume =
      declare "VM.s3resume" []
        (debug_info_p @-> vm_id_p @-> returning task_id_p err)

    let export_metadata =
      declare "VM.export_metadata" []
        (debug_info_p @-> vm_id_p @-> returning (Param.mk Types.string) err)

    let import_metadata =
      declare "VM.import_metadata" []
        ( debug_info_p
        @-> Param.mk ~name:"metadata" Types.string
        @-> returning vm_id_p err )
  end

  module PCI = struct
    open TypeCombinators

    let pci_t_p = Param.mk ~name:"pci" Pci.t

    let pci_id_p = Param.mk ~name:"id" Pci.id

    let add =
      declare "PCI.add" [] (debug_info_p @-> pci_t_p @-> returning pci_id_p err)

    let remove =
      declare "PCI.remove" []
        (debug_info_p @-> pci_id_p @-> returning unit_p err)

    let stat =
      let stat_p = Param.mk ~name:"stat" (pair (Pci.t, Pci.state)) in
      declare "PCI.stat" [] (debug_info_p @-> pci_id_p @-> returning stat_p err)

    let list =
      let stat_list_p =
        Param.mk ~name:"stat list" (list (pair (Pci.t, Pci.state)))
      in
      declare "PCI.list" []
        (debug_info_p @-> VM.vm_id_p @-> returning stat_list_p err)
  end

  module VBD = struct
    open TypeCombinators

    let vbd_t_p = Param.mk ~name:"vbd" Vbd.t

    let vbd_id_p = Param.mk ~name:"id" Vbd.id

    let task_id_p = Param.mk ~name:"task" Task.id

    let add =
      declare "VBD.add" [] (debug_info_p @-> vbd_t_p @-> returning vbd_id_p err)

    let plug =
      declare "VBD.plug" []
        (debug_info_p @-> vbd_id_p @-> returning task_id_p err)

    let unplug =
      let force_p = Param.mk ~name:"force" Types.bool in
      declare "VBD.unplug" []
        (debug_info_p @-> vbd_id_p @-> force_p @-> returning task_id_p err)

    let eject =
      declare "VBD.eject" []
        (debug_info_p @-> vbd_id_p @-> returning task_id_p err)

    let insert =
      let disk_p = Param.mk ~name:"disk" disk in
      declare "VBD.insert" []
        (debug_info_p @-> vbd_id_p @-> disk_p @-> returning task_id_p err)

    let stat =
      let stat_p = Param.mk ~name:"stat" (pair (Vbd.t, Vbd.state)) in
      declare "VBD.stat" [] (debug_info_p @-> vbd_id_p @-> returning stat_p err)

    let list =
      let stat_list_p =
        Param.mk ~name:"stat list" (list (pair (Vbd.t, Vbd.state)))
      in
      declare "VBD.list" []
        (debug_info_p @-> VM.vm_id_p @-> returning stat_list_p err)

    let remove =
      declare "VBD.remove" []
        (debug_info_p @-> vbd_id_p @-> returning unit_p err)
  end

  module VUSB = struct
    open TypeCombinators

    let vusb_t_p = Param.mk ~name:"vusb" Vusb.t

    let vusb_id_p = Param.mk ~name:"id" Vusb.id

    let task_id_p = Param.mk ~name:"task" Task.id

    let add =
      declare "VUSB.add" []
        (debug_info_p @-> vusb_t_p @-> returning vusb_id_p err)

    let plug =
      declare "VUSB.plug" []
        (debug_info_p @-> vusb_id_p @-> returning task_id_p err)

    let unplug =
      declare "VUSB.unplug" []
        (debug_info_p @-> vusb_id_p @-> returning task_id_p err)

    let stat =
      let stat_p = Param.mk ~name:"stat" (pair (Vusb.t, Vusb.state)) in
      declare "VUSB.stat" []
        (debug_info_p @-> vusb_id_p @-> returning stat_p err)

    let list =
      let stat_list_p =
        Param.mk ~name:"stat list" (list (pair (Vusb.t, Vusb.state)))
      in
      declare "VUSB.list" []
        (debug_info_p @-> VM.vm_id_p @-> returning stat_list_p err)

    let remove =
      declare "VUSB.remove" []
        (debug_info_p @-> vusb_id_p @-> returning unit_p err)
  end

  module VIF = struct
    open TypeCombinators

    let vif_t_p = Param.mk ~name:"vif" Vif.t

    let vif_id_p = Param.mk ~name:"id" Vif.id

    let task_id_p = Param.mk ~name:"task" Task.id

    let add =
      declare "VIF.add" [] (debug_info_p @-> vif_t_p @-> returning vif_id_p err)

    let plug =
      declare "VIF.plug" []
        (debug_info_p @-> vif_id_p @-> returning task_id_p err)

    let unplug =
      let force_p = Param.mk ~name:"force" Types.bool in
      declare "VIF.unplug" []
        (debug_info_p @-> vif_id_p @-> force_p @-> returning task_id_p err)

    let move =
      let network_p = Param.mk ~name:"network" Network.t in
      declare "VIF.move" []
        (debug_info_p @-> vif_id_p @-> network_p @-> returning task_id_p err)

    let stat =
      let stat_p = Param.mk ~name:"stat" (pair (Vif.t, Vif.state)) in
      declare "VIF.stat" [] (debug_info_p @-> vif_id_p @-> returning stat_p err)

    let list =
      let stat_list_p =
        Param.mk ~name:"stat list" (list (pair (Vif.t, Vif.state)))
      in
      declare "VIF.list" []
        (debug_info_p @-> VM.vm_id_p @-> returning stat_list_p err)

    let remove =
      declare "VIF.remove" []
        (debug_info_p @-> vif_id_p @-> returning unit_p err)

    let set_carrier =
      let carrier_p = Param.mk ~name:"carrier" Types.bool in
      declare "VIF.set_carrier" []
        (debug_info_p @-> vif_id_p @-> carrier_p @-> returning task_id_p err)

    let set_locking_mode =
      let locking_mode_p = Param.mk ~name:"locking_mode" Vif.locking_mode in
      declare "VIF.set_locking_mode" []
        ( debug_info_p @-> vif_id_p @-> locking_mode_p
        @-> returning task_id_p err )

    let set_ipv4_configuration =
      let config_p =
        Param.mk ~name:"ipv4_configuration" Vif.ipv4_configuration
      in
      declare "VIF.set_ipv4_configuration" []
        (debug_info_p @-> vif_id_p @-> config_p @-> returning task_id_p err)

    let set_ipv6_configuration =
      let config_p =
        Param.mk ~name:"ipv6_configuration" Vif.ipv6_configuration
      in
      declare "VIF.set_ipv6_configuration" []
        (debug_info_p @-> vif_id_p @-> config_p @-> returning task_id_p err)

    let set_pvs_proxy =
      let proxy_p = Param.mk ~name:"proxy" (option Vif.PVS_proxy.t) in
      declare "VIF.set_pvs_proxy" []
        (debug_info_p @-> vif_id_p @-> proxy_p @-> returning task_id_p err)
  end

  module VGPU = struct
    open TypeCombinators

    let vgpu_t_p = Param.mk ~name:"vgpu" Vgpu.t

    let vgpu_id_p = Param.mk ~name:"id" Vgpu.id

    let add =
      declare "VGPU.add" []
        (debug_info_p @-> vgpu_t_p @-> returning vgpu_id_p err)

    let stat =
      let stat_p = Param.mk ~name:"stat" (pair (Vgpu.t, Vgpu.state)) in
      declare "VGPU.stat" []
        (debug_info_p @-> vgpu_id_p @-> returning stat_p err)

    let list =
      let stat_list_p =
        Param.mk ~name:"stat list" (list (pair (Vgpu.t, Vgpu.state)))
      in
      declare "VGPU.list" []
        (debug_info_p @-> VM.vm_id_p @-> returning stat_list_p err)

    let remove =
      declare "VGPU.remove" []
        (debug_info_p @-> vgpu_id_p @-> returning unit_p err)
  end

  module UPDATES = struct
    open TypeCombinators

    let last_p = Param.mk ~name:"last_id" (option Types.int)
    let timeout_p = Param.mk ~name:"timeout" (option Types.int)

    let get =
      let result_p =
        Param.mk ~name:"updates"
          (triple (list Dynamic.barrier, list Dynamic.id, Types.int))
      in
      declare "UPDATES.get" []
        (debug_info_p @-> last_p @-> timeout_p @-> returning result_p err)

    let last_id =
      let id_p = Param.mk ~name:"id" Types.int in
      declare "UPDATES.last_id" [] (debug_info_p @-> returning id_p err)

    let inject_barrier =
      let id_p = Param.mk ~name:"id" Types.int in
      let vm_id_p = Param.mk ~name:"vm_id" Vm.id in
      declare "UPDATES.inject_barrier" []
        (debug_info_p @-> vm_id_p @-> id_p @-> returning unit_p err)

    let remove_barrier =
      let id_p = Param.mk ~name:"id" Types.int in
      declare "UPDATES.remove_barrier" []
        (debug_info_p @-> id_p @-> returning unit_p err)

    let refresh_vm =
      declare "UPDATES.refresh_vm" []
        (debug_info_p @-> VM.vm_id_p @-> returning unit_p err)

    let inject =
      let result_p = Param.mk ~name:"gen" Types.int64 in
      declare "UPDATES.inject" []
        (debug_info_p @-> returning result_p err)

    let get_deltas =
      let result_p =
        Param.mk ~name:"updates"
          (pair (Types.int64, Rpc.t))
      in
      declare "UPDATES.get_deltas" []
        (debug_info_p @-> last_p @-> timeout_p @-> returning result_p err)
  end

  module DEBUG = struct
    open TypeCombinators

    let trigger =
      declare "DEBUG.trigger" []
        ( debug_info_p
        @-> Param.mk ~name:"cmd" Types.string
        @-> Param.mk ~name:"args" (list Types.string)
        @-> returning unit_p err )

    let shutdown =
      declare "DEBUG.shutdown" []
        (debug_info_p @-> unit_p @-> returning unit_p err)
  end
end
