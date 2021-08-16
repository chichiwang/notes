# Resilient Sharing of React Components
To start we will visualize a connection between a `Home Application` and a `Nav Application`. The `Home Application` has a runtime dependency on the `Header` module found in the `Nav Application`:

![Federated applications example](./example-federated-applications.JPG)
<br /><br />

In this example, the `Home Application` will attempt to fetch the `Header` module from the `Nav Application` at runtime. However, any runtime connection can fail. In the case that `Nav Application` is unreachable at runtime the page will be broken and the console will display the following error:

```console
Uncaught ReferenceError: nav is not defined
while loading "Header" from webpack/container/reference/nav 
```

The code in `Home Application` looks like:
```JSX
<React.Suspense fallback={<div />}>
  <Header />
</React.Suspense> 
```

[Suspense](https://reactjs.org/docs/concurrent-mode-suspense.html) is insufficient, an [error boundary](https://reactjs.org/docs/error-boundaries.html) is required to make the application more resilient to these runtime failures.

```JSX
<ErrorBoundary>
  <React.Suspense fallback={<div />}>
    <Header />
  </React.Suspense>
</ErrorBoundary>
```

A `FederatedWrapper` class can be created to handle both a slow load and an error during component execution:
```JSX
class FederatedWrapper extends React.Component {
  // ... Same as in the ErrorBoundary class

  render() {
    if (this.state.hasError) {
      return this.props.error || <div>Something went wrong.</div>;
    }

    return (
      <React.Suspense fallback={this.props.delayed || <div />}>
        {this.props.children}
      </React.Suspense>
    );
  }
}
```

Now the usage of the `Header` component in `Home Application` can simply be wrapped in this new `FederatedWrapper` component that will handle both _error_ and _delayed_ scenarios.
```JSX
<FederatedWrapper
  error={<div>Temporary Header</div>}
  delayed={<div>Loading header...</div>}
>
  <Header />
</FederatedWrapper>
```

## Higher Order Error Handling
Leveraging the `FederatedWrapper` component, a higher order component can be created that accepts a lazy component and returns a component with the same API:
```JSX
const wrapComponent = (Component) => ({ error, delayed, ...props }) => (
  <FederatedWrapper error={error} delayed={delayed}>
    <Component {...props} />
  </FederatedWrapper>
);
```

`wrapComponent` accepts a React component as argument and returns a new component that accepts the properties for _error_ and _delayed_ and passes any additional properies to the wrapped component.

Using this, a `Header` component can be created that encapsulates all of the lazy and error-handling behavior:
```JSX
const Header = wrapComponent(React.lazy(() => import("nav/Header")));

const App = () => (
  <div>
    <Header />
    <div>Hi there, I'm React from React.</div>
  </div>
);
```

## Bootstrapping Applications
An important safety tip is to _bootstrap_ an application. Bootstrapping means that the entrypoint file for an application should be a file whose job it is to asynchronously load the main application.

A non-bootstrapped application whose entrypoint is `index.js` may look like this:
```JSX
// index.js
import ReactDOM from "react-dom";
import Header from "nav/Header";

ReactDOM.render(<Header />, document.getElementById("app"));
```

In this example, Webpack would not have time to load the `nav` remote before the code execuites which can result in a runtime error.

A bootstrapped application, by comparison:
```JSX
// index.js
import("./App");

// App.jsx
import ReactDOM from “react-dom";
ReactDOM.render(<div>Hello world</div>, document.getElementById("app"));
```

This example gives Webpack the opportunity to process the remaining imports before executing the app and will avoid potential race conditions on importing all of the code.

| [Previous: How Module Federation Works](../03/README.md) | [Table of Contents](../README.md#table-of-contents) | [Next: React State Sharing Options](../05/README.md) |
