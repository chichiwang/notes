# React State Sharing Options
Below are some options on how to share state between the host and remote modules, or even between remote modules, using various libraries. The repository containing example implementations of state sharing through these libraries can be found [here](https://github.com/module-federation/practical-module-federation/tree/master/chapter5)

## [React](https://reactjs.org/)
When using state in React, only one instance of React can be used on a given page. React will issue a warning about using multiple copies of React in the same application if state is accessed by different React library instances.

Sharing the React library from the same remote a module/component is shared from, and marking it as a singleton will alleviate this issue.

## [Redux](https://redux.js.org/)
To share state across applications using Redux, share the [provider](https://react-redux.js.org/api/provider) generated via [react-redux](https://react-redux.js.org/) and leverate the [connect](https://react-redux.js.org/api/connect) method exposed by the library.

## [MobX](https://mobx.js.org/README.html)
Sharing state across applications using MobX requires creating a [store](https://mobx.js.org/observable-state.html) in the host application, wrapping the `App` and `Header` in the host in an observer and then, in the remote, also wrapping the shared component in an observer.

## [Recoil](https://recoiljs.org/)
When sharing state across applications using Recoil, share the [atoms](https://recoiljs.org/docs/api-reference/core/atom) between applications. Then use the [useRecoilState](https://recoiljs.org/docs/api-reference/core/useRecoilState) hook in place of React's _useState_ hook.

## [RXJS](https://rxjs.dev/)
RXJS will have to be shared from the remote (although if only [subjects](https://rxjs.dev/api/index/class/Subject) are shared, this may not be required). Subjects can then be shared for host applications to subscribe to.

| [Previous: Resilient Sharing of React Components](../04/README.md) | [Table of Contents](../README.md#table-of-contents) | [Next: Resilient Function and Data Sharing](../06/README.md) |
