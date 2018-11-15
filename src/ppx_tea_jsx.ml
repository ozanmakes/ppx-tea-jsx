open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let str_const ~pexp_loc s =
  { pexp_desc = Pexp_constant (Const_string (s, None))
  ; pexp_loc
  ; pexp_attributes = [] }


let filter_map f l =
  let rec recurse acc l =
    match l with
    | [] ->
        List.rev acc
    | x :: l' ->
        let acc' = match f x with None -> acc | Some y -> y :: acc in
        recurse acc' l'
  in
  recurse [] l


let rec classify = function
  (**************
   * Attributes *
   **************)
  | ( "accept"
    | "challenge"
    | "charset"
    | "content"
    | "contextmenu"
    | "datetime"
    | "draggable"
    | "enctype"
    | "form"
    | "formaction"
    | "href"
    | "itemprop"
    | "list"
    | "manifest"
    | "max"
    | "media"
    | "min"
    | "pubdate"
    | "rel"
    | "src"
    | "step"
    | "title" ) as name ->
      (* attribute("href", value) *)
      Some (`Attribute name)
  (* aliases *)
  | "acceptCharset" ->
      Some (`Attribute "accept-charset")
  | ("disabled" | "selected") as name ->
      (* if (value) { attribute("readOnly", "true") } else { noProp }  *)
      Some (`Bool_attribute name)
  | ( "cols"
    | "colspan"
    | "height"
    | "maxlength"
    | "minlength"
    | "rows"
    | "rowspan"
    | "size"
    | "tabindex"
    | "width" ) as name ->
      (* attribute("cols", string_of_int(value)) *)
      Some (`Int_attribute name)
  (**************
   * Properties *
   **************)
  | ( "action"
    | "align"
    | "alt"
    | "cite"
    | "className"
    | "coords"
    | "defaultValue"
    | "dir"
    | "dropzone"
    | "headers"
    | "hreflang"
    | "htmlFor"
    | "id"
    | "keytype"
    | "kind"
    | "lang"
    | "language"
    | "name"
    | "pattern"
    | "ping"
    | "placeholder"
    | "poster"
    | "preload"
    | "sandbox"
    | "scope"
    | "scoped"
    | "shape"
    | "srcdoc"
    | "srclang"
    | "target"
    | "usemap"
    | "value"
    | "wrap" ) as name ->
      (* prop("className", value) *)
      Some (`Property name)
  (* aliases *)
  | "class'" ->
      Some (`Property "className")
  | "downloadAs" ->
      Some (`Property "download")
  | "httpEquiv" ->
      Some (`Property "http-equiv")
  | "method'" | "method_" ->
      Some (`Property "method")
  | "type'" | "type_" ->
      Some (`Property "type")
  | "start" ->
      (* prop("start", string_of_int(value)) *)
      Some (`Int_property "start")
  | "autocomplete" ->
      (* prop("autocomplete", if (value) { "on" } else { "off" }) *)
      Some (`On_off_property "autocomplete")
  | "accesskey" ->
      (* prop("accesskey", String.mk(1, value)) *)
      Some (`Char_property "accesskey")
  | ( "async"
    | "autofocus"
    | "autoplay"
    | "checked"
    | "contenteditable"
    | "controls"
    | "default"
    | "defer"
    | "hidden"
    | "ismap"
    | "loop"
    | "multiple"
    | "muted"
    | "novalidate"
    | "readonly"
    | "required"
    | "reversed"
    | "seamless"
    | "spellcheck" ) as name ->
      (* if (value) { prop("readOnly", "readOnly") } else { noProp } *)
      Some (`Bool_property name)
  | "download" ->
      (* if (value) { prop("download", "") } else { noProp } *)
      Some (`Empty_bool_property "download")
  (**********
   * Events *
   **********)
  | ( "onAbort"
    | "onAnimationEnd"
    | "onAnimationIteration"
    | "onAnimationStart"
    | "onBlur"
    | "onCanPlay"
    | "onCanPlayThrough"
    | "onClick"
    | "onContextMenu"
    | "onDblClick"
    | "onDrag"
    | "onDragEnd"
    | "onDragEnter"
    | "onDragExit"
    | "onDragLeave"
    | "onDragOver"
    | "onDragStart"
    | "onDrop"
    | "onDurationChange"
    | "onEmptied"
    | "onEncrypted"
    | "onEnded"
    | "onError"
    | "onFocus"
    | "onGotPointerCapture"
    | "onInvalid"
    | "onKeyDown"
    | "onKeyPress"
    | "onKeyUp"
    | "onLoad"
    | "onLoadStart"
    | "onLoadedData"
    | "onLoadedMetadata"
    | "onLostPointerCapture"
    | "onMouseDown"
    | "onMouseEnter"
    | "onMouseLeave"
    | "onMouseMove"
    | "onMouseOut"
    | "onMouseOver"
    | "onMouseUp"
    | "onPause"
    | "onPlay"
    | "onPlaying"
    | "onPointerCancel"
    | "onPointerDown"
    | "onPointerEnter"
    | "onPointerLeave"
    | "onPointerMove"
    | "onPointerOut"
    | "onPointerOver"
    | "onPointerUp"
    | "onProgress"
    | "onRateChange"
    | "onScroll"
    | "onSeeked"
    | "onSeeking"
    | "onSelect"
    | "onStalled"
    | "onSuspend"
    | "onTimeUpdate"
    | "onToggle"
    | "onTouchCancel"
    | "onTouchEnd"
    | "onTouchMove"
    | "onTouchStart"
    | "onTransitionEnd"
    | "onVolumeChange"
    | "onWaiting"
    | "onWheel" ) as event ->
      let event =
        String.sub event 2 (String.length event - 2) |> String.lowercase
      in
      Some (`On event)
  | "onDoubleClick" ->
      classify "onDblClick"
  | ( "classList"
    | "onChange"
    | "onChangeOpt"
    | "onCheck"
    | "onCheckOpt"
    | "onInput"
    | "onInputOpt" ) as name ->
      (* These map to helper functions under Tea.Html *)
      Some (`Call name)
  | "style" ->
      Some `Style
  | "" | "children" ->
      None
  | property ->
      failwith ("Unrecognized property: " ^ property)


(* <button onClick=Increment />  ->  div([onClick(Increment)], []) *)
(* <button onClick=callback />  ->  div([onCB("click", "", callback)], []) *)
let map_event = function
  | {pexp_desc = Pexp_construct _; _} as value ->
      [%expr Vdom.EventHandlerMsg [%e value]]
  | value ->
      [%expr Vdom.EventHandlerCallback ("", [%e value])]


let map_property (kind, value) =
  let {pexp_loc; _} = value in
  match kind with
  | `Attribute name ->
      [%expr Vdom.Attribute ("", [%e str_const ~pexp_loc name], [%e value])]
  | `Int_attribute name ->
      [%expr
        Vdom.Attribute
          ("", [%e str_const ~pexp_loc name], string_of_int [%e value])]
  | `Bool_attribute name ->
      [%expr
        if [%e value]
        then Vdom.Attribute ("", [%e str_const ~pexp_loc name], "true")
        else Vdom.NoProp]
  | `Property name ->
      [%expr Vdom.RawProp ([%e str_const ~pexp_loc name], [%e value])]
  | `Bool_property name ->
      [%expr
        if [%e value]
        then
          Vdom.RawProp
            ([%e str_const ~pexp_loc name], [%e str_const ~pexp_loc name])
        else Vdom.NoProp]
  | `Int_property name ->
      [%expr
        Vdom.RawProp ([%e str_const ~pexp_loc name], string_of_int [%e value])]
  | `Char_property name ->
      [%expr
        Vdom.RawProp ([%e str_const ~pexp_loc name], String.make 1 [%e value])]
  | `On_off_property name ->
      [%expr
        if [%e value]
        then Vdom.RawProp ([%e str_const ~pexp_loc name], "on")
        else Vdom.RawProp ([%e str_const ~pexp_loc name], "off")]
  | `Empty_bool_property name ->
      [%expr
        if [%e value]
        then Vdom.RawProp ([%e str_const ~pexp_loc name], "")
        else Vdom.NoProp]
  | `On name ->
      let name = str_const ~pexp_loc name in
      [%expr Vdom.Event ([%e name], [%e map_event value], ref None)]
  | `Call name ->
      Exp.mk
        (Pexp_apply
           ( Exp.mk
             @@ Pexp_ident
                  { txt = Ldot (Ldot (Lident "Tea", "Html"), name)
                  ; loc = pexp_loc }
           , [("", value)] ))
  | `Style ->
      [%expr Vdom.Style [%e value]]


let map_args args =
  List.fold_right
    (fun (name, value) properties ->
      let name =
        if String.length name > 0 && name.[0] = '?'
        then
          let name = String.sub name 1 (String.length name - 1) in
          `Optional name
        else `Regular name
      in
      match name with
      | `Optional name ->
        ( match classify name with
        | None ->
            properties
        | Some kind ->
            let property =
              [%expr
                match [%e value] with
                | Some value ->
                    [%e map_property (kind, [%expr value])]
                | None ->
                    Vdom.NoProp]
            in
            [%expr [%e property] :: [%e properties]] )
      | `Regular name ->
        ( match classify name with
        | None ->
            properties
        | Some kind ->
            [%expr [%e map_property (kind, value)] :: [%e properties]] ) )
    args
    [%expr []]


let rewrite_const = function
  | {pexp_desc = Pexp_constant (Const_string _); _} as const ->
      [%expr Vdom.Text [%e const]]
  | {pexp_desc = Pexp_constant (Const_char _); _} as const ->
      [%expr Vdom.Text (String.make 1 [%e const])]
  | {pexp_desc = Pexp_constant (Const_int _); _} as const ->
      [%expr Vdom.Text (string_of_int [%e const])]
  | {pexp_desc = Pexp_constant (Const_float _); _} as const ->
      [%expr Vdom.Text (string_of_float [%e const])]
  | {pexp_desc = Pexp_constant (Const_int32 _); _} as const ->
      [%expr Vdom.Text (Int32.to_string [%e const])]
  | {pexp_desc = Pexp_constant (Const_int64 _); _} as const ->
      [%expr Vdom.Text (Int64.to_string [%e const])]
  | {pexp_desc = Pexp_constant (Const_nativeint _); _} as const ->
      [%expr Vdom.Text (Nativeint.to_string [%e const])]
  | expr ->
      expr


let rec map_children = function
  | { pexp_desc =
        Pexp_construct
          ( ({txt = Lident "::"; _} as cons)
          , Some ({pexp_desc = Pexp_tuple tuple; _} as a) ); _ } as e ->
      let tuple =
        match tuple with
        | [({pexp_desc = Pexp_constant _; _} as car); cdr] ->
            [rewrite_const car; map_children cdr]
        | [car; cdr] ->
            [car; map_children cdr]
        | x ->
            x
      in
      { e with
        pexp_desc =
          Pexp_construct (cons, Some {a with pexp_desc = Pexp_tuple tuple}) }
  | x ->
      x


let extract_tea_properties props =
  List.fold_left
    (fun (ns, key, unique, props) prop ->
      match prop with
      | "ns", ns | "namespace", ns ->
          (ns, key, unique, props)
      | "key", key ->
          (ns, key, unique, props)
      | "unique", unique ->
          (ns, key, unique, props)
      | prop ->
          (ns, key, unique, prop :: props) )
    ([%expr ""], [%expr ""], [%expr ""], [])
    props


let mapper =
  { default_mapper with
    expr =
      (fun mapper e ->
        match e with
        | { pexp_attributes = [({txt = "JSX"; _}, PStr [])]
          ; pexp_desc =
              Pexp_apply
                ({pexp_desc = Pexp_ident {txt = Lident html_tag; _}; _}, args)
          ; pexp_loc } ->
            let html_tag =
              { pexp_desc = Pexp_constant (Const_string (html_tag, None))
              ; pexp_loc
              ; pexp_attributes = [] }
            in
            let ns, key, unique, args = extract_tea_properties args in
            let properties = map_args args in
            let children =
              args
              |> List.find (fun (name, _) -> name = "children")
              |> snd
              |> map_children
            in
            [%expr
              Vdom.Node
                ( [%e ns]
                , [%e html_tag]
                , [%e key]
                , [%e unique]
                , [%e properties]
                , [%e default_mapper.expr mapper children] )]
        | { pexp_attributes = [({txt = "JSX"; _}, PStr [])]
          ; pexp_desc =
              Pexp_apply
                ( ( { pexp_desc =
                        Pexp_ident
                          ({txt = Ldot (module', "createElement"); _} as ident); _
                    } as expr )
                , args )
          ; pexp_loc } ->
            let args =
              args
              |> filter_map (function
                     | "children", children ->
                         (* Map children to the last argument *)
                         Some ("", default_mapper.expr mapper children)
                     | ( ""
                       , { pexp_desc =
                             Pexp_construct ({txt = Lident "()"; _}, None); _
                         } ) ->
                         None
                     | x ->
                         Some x )
            in
            Pexp_apply
              ( { expr with
                  pexp_desc =
                    Pexp_ident {ident with txt = Ldot (module', "view")} }
              , args )
            |> Exp.mk ~loc:pexp_loc
        | e ->
            default_mapper.expr mapper e ) }


let () = run_main (fun _argv -> mapper)
