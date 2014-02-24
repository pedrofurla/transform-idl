
An hand-written [WebIdl](http://www.w3.org/TR/WebIDL/) parser - using [Parsec3](http://hackage.haskell.org/package/parsec3).

The purpose of this project is to one day provide tools to transform WebIdl (maybe OMG's idl too?) into bindings/stubs for [altjs](altjs.org) languages. My main targets are Haskell based ones (like [Fay](http://fay-lang.org) and [PureScript](http://purescript.readthedocs.org/)) and [ScalaJs](http://www.scala-js.org/).

The idl directory currently contains IDLs for [Web Audio](http://www.w3.org/TR/webaudio/) and [WebGL](http://www.khronos.org/registry/webgl/specs/latest/1.0/).

The ExtractIdl.hs is the code I used to extract the idl source from the Web Audio spec Html page. Unfortunately, W3C's specs don't currently maintain idl code in a homogeneous structure across its htmls, so it requires specific code for each spec.

Many parts of WebIdl are still not implemented:

 * Extended attributes are only a list of string
 * Literals are typed but also only strings
 * no `enums`, `exceptions`.
 * Most of these are easy to implement and likely to be done soon.
 * Basically, currently only a subset of WebIdl commom to Web Audio and WebGL is implemented.

There are still *many* opportunities for abstractions/code-reuse that were simply ignored - check Parser.hs, Ast.hs and PrettyPrint.hs and you will see what I mean.
