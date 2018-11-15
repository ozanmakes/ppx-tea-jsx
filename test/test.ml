module Web = struct module Node = struct type event_cb
                                         type event end end
module Vdom =
  struct
    type 'msg eventHandler =
      | EventHandlerCallback of string* (Web.Node.event -> 'msg option)
      | EventHandlerMsg of 'msg
    type 'msg eventCache =
      {
      handler: Web.Node.event_cb;
      cb: (Web.Node.event -> 'msg option) ref;}
    type 'msg property =
      | NoProp
      | RawProp of string* string
      | Attribute of string* string* string
      | Data of string* string
      | Event of string* 'msg eventHandler* 'msg eventCache option ref
      | Style of (string* string) list
    type 'msg properties = 'msg property list
    type 'msg t =
      | Node of string* string* string* string* 'msg properties* 'msg t list
      | Text of string
  end
module Tea =
  struct
    module Html =
      struct let onChange x = failwith (("yo")[@reason.raw_literal "yo"]) end
  end

  module User =struct
    
    end
type msg =
  | Increment
  | Decrement
  | Reset
  | Set of int
let view model =
  ((User.createElement ~name:(("Ozan")[@reason.raw_literal "Ozan"])
      ?age:((Some (27))[@explicit_arity ]) ~children:[] ())[@JSX ])
