# Elm Overview
**Author**: [Chi-chi Wang](https://github.com/chichiwang)

A high level investigation into the Elm Programming Language. This is meant to be a profile of the language itself more than an investigation of the syntax and operations.

Notes ported over from Google Docs, built between November 5, 2018 and November 6, 2018.

## Table of Contents
* [Profile](#profile)
* [History](#history)
* [Features](#features)
* [Drawbacks/Limitations](#drawbackslimitations)
* [Resources](#resources)

## Profile
* Domain-specific programming language for creating web browser based user interfaces
* Purely functional
* Emphasis on
  * Usability
  * Performance
  * Robustness
* Utilizes static type-checking
  * Advertises “no runtime exceptions in practice”

## History
* Initially designed by Evan Czaplicki as his thesis in 2012
* Evan Czaplicki joined Prezi in 2013 to work on Elm
* 2016: started the Elm Software Foundation

## Features
* First-class functions
* Supports [partial application](https://en.wikipedia.org/wiki/Partial_application)
* Immutable values
* Stateless functions
* Static typing (with [type inference](https://en.wikipedia.org/wiki/Type_inference))
* Renders HTML through a virtual DOM
* Interoperates with other code by using “JavaScript as a service”
* Utilizes a module system
* Uses an abstraction called *ports* to communicate with JavaScript
  * Allows values to flow in and out of Elm programs
* Utilizes a library *elm-html* that a programmer can use to write HTML and CSS within Elm
* Automatically enforced package semver
* Debug mode with built in time travel

## Drawbacks/Limitations
* Has no support for [higher-kinded types](https://en.wikipedia.org/wiki/Kind_(type_theory)) unlike Haskell or Purescript
* Client-side only
* Small community/ecosystem
* Hasn’t hit version 1.0

## Resources
* [Wikipedia](https://en.wikipedia.org/wiki/Elm_(programming_language))
* [https://blog.realkinetic.com/elm-changed-my-mind-about-unpopular-languages-190a23f4a834](https://blog.realkinetic.com/elm-changed-my-mind-about-unpopular-languages-190a23f4a834)
