# ppx-tea-jsx &nbsp;&nbsp; [![Version 0.6.0][npm-img]][npm]

[npm-img]: https://img.shields.io/npm/v/ppx-tea-jsx.svg
[npm]: https://www.npmjs.com/package/ppx-tea-jsx

[Reason](https://reasonml.github.io) JSX syntax support for
[BuckleScript-TEA](https://github.com/OvermindDL1/bucklescript-tea) library.

## Installation

1. Install via npm or Yarn:

```sh
yarn add --dev ppx-tea-jsx
```

2. Add the PPX to your `bsconfig.json`:

```json
{
  "ppx-flags": ["ppx-tea-jsx/ppx"]
}
```

## Usage

You can pass JSX expressions to any function that expects `Vdom.t`.

### HTML elements

```reason
let view = _model =>
  <div id="root" className="blue">
    <button style=[("width", "20px")] classList=[("active", true)] />
  </div>;
```

```reason
<video controls=true autoplay=false width=250>
  <source src="/media/examples/flower.mp4" type_="video/mp4" />
  "Sorry, your browser doesn't support embedded videos."
</video>
```

#### Literals

```reason
<a href="https://example.com"> "Addition: " 5 '+' 25.5 </a>
```

#### Punning

```reason
let id = "fifty";
let className = None;

let _ = <div id ?className />;
```

#### Children spread

```reason
let buttons_list_fn = () => [<button />, <button />, <button />];

let buttons_fragment = <> <button /> <button /> <button /> </>;

let view = _model =>
  <div>
    <span> ...{buttons_list_fn()} </span>
    <span> ...buttons_fragment </span>
  </div>;
```

#### Events

You can trigger messages:

```reason
<button accesskey='s' onClick=Increment />
```

And return `option(msg)` from callbacks:

```reason
<button
  disabled=true
  onClick={
    e => {
      e##preventDefault();
      Some(Reset);
    }
  }
/>
```

```reason
<input onChange={value => Set(int_of_string(value))} />
```

```reason
let view = model => {
  let onChangeOpt = value =>
    try (value->int_of_string->Set->Some) {
    | _ => None
    };
  <input value=model onChangeOpt />;
};
```

### Modules (Capitalized Tag)

`<Foo bar="baz"> "Hi" </Foo>` is mapped to `Foo.view(~bar="baz", [text("Hi")])`.

```reason
type person = {
  name: string,
  age: option(int),
};

module OnlyChild = {
  let view = children => <div className="border-5"> children </div>;
};

module Person = {
  let view = (~className=?, ~name, ~age=?, children) => {
    open Tea.Html;
    let age =
      age
      ->Belt.Option.mapWithDefault(noNode, x =>
          <span> " (" x->string_of_int->text ")" </span>
        );

    <div ?className> <span> ...children </span> {text(name)} age </div>;
  };
};

let view = model => {
  let img =
    <img
      src="/media/examples/grapefruit-slice-332-332.jpg"
      alt="Grapefruit slice atop a pile of other slices"
    />;

  <div>
    <div> <OnlyChild> ...img </OnlyChild> </div>
    <Person className="user-l" name={model.name} age=?{model.age}>
      <strong> "Hi " </strong>
    </Person>
  </div>;
};
```
