# Resilient Function and Data Sharing
One of the most important aspects of Module Federation is that anything that can be represented as a Javascript module can be shared, including:
* **Primitives**: numbers, strings, dates, etc.
* **Arrays**: Standard Javascript arrays
* **Objects**: Regular objects and transpiled JSON
* **Functions**
* **Classes**

## Primitive Values
Primitive values can be shared by a remote easily in a module:

```javascript
export default 'single value';
```

In the Webpack configuration for this remote, the module would be exposed. The host application consuming this remote module could then leverage it:

```javascript
const SingleValue = () => {
   const [singleValue, singleValueSet] = React.useState(null);

   React.useEffect(() => {
     import("myRemote/singleValue")
       .then(({ default: value }) => singleValueSet(value))
       .catch((err) => console.error(`Error getting single value: ${err}`));
   }, []);

   return <div>Single value: {singleValue}.</div>;
};
```

Inside the `useEffect` hook, this code imports the single value module from the remote aliased `myRemote` and sets it into a state object `singleValue`. If an error occurs in import, that error is sent to the console.

In some cases synchronous imports may be used, but to build resilient code the remotes should be imported asynchronously and errors should be handled in case the remote import fails.

## Functions
Below is an example of a module that exports a function, exposed by a remote:

```javascript
export default (msg) => console.log(`Analytics msg: ${msg}`);
```

The host application can then consume the function exposed by this module like this:
```javascript
const analyticsFunc = import("myRemote/analyticsFunc");
const sendAnalytics = (msg) => {
  analyticsFunc
    .then(({ default: analyticsFunc }) => analyticsFunc(msg))
    .catch((err) => console.log(`Error sending analytics value: ${msg}`));
};
```

The `import` statement returns a promise object that is then assigned to `analyticsFunc`. When the promise object resolves, it remains resolved and the `sendAnalytics` function that utilizes it will not need to wait beyond the first time the module gets resolved.

A higher order function can be created to import remote functions as well:

```javascript
const createAsyncFunc = (promise) => (...args) =>
 promise
  .then(({ default: func }) => func(...args))
  .catch((err) =>
    console.log(`Error sending analytics value: ${JSON.stringify(args)}`)
  );
```

This higher order function can then be passed an import statement to fetch a particular remote function, and returns a function to call to execute it:

```javascript
const sendAnalytics = createAsyncFunc(import("myRemote/analyticsFunc"));
```

## Classes
A remote can define a module that shares a class:

```javascript
class MyClass {
  constructor(value) {
    this.value = value;
  }

  logString() {
    console.log(`Logging ${this.value}`);
  }
}

export default MyClass;
```

A host can then import the class for use:
```javascript
const classExport = import("myRemote/classExport");

const newClassObject = (...args) =>
  classExport
    .then(({ default: classRef }) => {
      return new classRef(...args);
    })
    .catch((err) => console.log(`Error getting class: ${err}`));
```

The above example can be invoked in this way:
```javascript
newClassObject("initial value").then((theObject) => {
  theObject.logString();
});
```

| [Previous: React State Sharing Options](../05/README.md) | [Table of Contents](../README.md#table-of-contents) | Next |
