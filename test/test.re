module Web = {
  module Node = {
    type event_cb;
    type event;
  };
};

module Vdom = {
  type eventHandler('msg) =
    | EventHandlerCallback(string, Web.Node.event => option('msg))
    | EventHandlerMsg('msg);

  type eventCache('msg) = {
    handler: Web.Node.event_cb,
    cb: ref(Web.Node.event => option('msg)),
  };

  type property('msg) =
    | NoProp
    | RawProp(string, string)
    | Attribute(string, string, string)
    | Event(string, eventHandler('msg), ref(option(eventCache('msg))))
    | Style(list((string, string)));

  type properties('msg) = list(property('msg));

  type t('msg) =
    | Node(string, string, string, string, properties('msg), list(t('msg)))
    | Text(string);
};

module Tea = {
  module Html = {
    let text: string => Vdom.t('msg) = _ => Obj.magic();
    let noNode = Obj.magic();
    let classList: list((string, bool)) => Vdom.property('msg) =
      _ => Obj.magic();
    let onChange = _ => Obj.magic();
    let onChangeOpt = _ => Obj.magic();
  };
};
type msg =
  | Increment
  | Set(int);

module Picture = {
  let view = (~className=?, _children) =>
    <img
      ?className
      src="/media/examples/grapefruit-slice-332-332.jpg"
      alt="Grapefruit slice atop a pile of other slices"
    />;
};

module User = {
  let view = (~classList=?, ~age=?, ~name, _children) => {
    let age =
      Tea.Html.(
        switch (age) {
        | Some(age) => <span> "( " {text(string_of_int(age))} " )" </span>
        | None => noNode
        }
      );
    let className = Some("large");
    let pictures = [
      <Picture ?className />,
      <Picture className="medium" />,
      <Picture />,
    ];
    let buttons =
      <> <button key="key" /> <button unique="unique" /> <button /> </>;

    <div ?classList>
      "Hi "
      {Tea.Html.text(" " ++ name ++ " ")}
      age
      <span> ...pictures </span>
      <div> ...buttons </div>
      <textbox ns="xul" />
      <foo namespace="bar" />
    </div>;
  };
};

let _view = _model =>
  <div id="root" className="blue" style=[("width", "50%")]>
    <a href="https://example.com" classList=[("active", true)]> "Hello" </a>
    <button disabled=true accesskey='s' onClick=Increment />
    <input
      maxlength=50
      autocomplete=true
      defaultValue="20"
      onChange={value => Set(int_of_string(value))}
    />
    <input
      value="20"
      onChangeOpt={
        value =>
          try (value->int_of_string->Set->Some) {
          | _ => None
          }
      }
    />
    <video controls=true autoplay=false width=250>
      <source src="/media/examples/flower.webm" type'="video/webm" />
      <source src="/media/examples/flower.mp4" type_="video/mp4" />
      "Sorry, your browser doesn't support embedded videos."
    </video>
    <ol start=70 />
    5
    5.0
    5L
    5l
    5n
    '5'
    <User name="Ozan" />
  </div>;

let _view = model => {
  let onChangeOpt = value =>
    try (value->int_of_string->Set->Some) {
    | _ => None
    };
  <input value=model onChangeOpt />;
};
