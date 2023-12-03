# Chapter 6: Limiting Scope Exposure
This chapter looks at how and why different levels of scope (functions and blocks) should be used to organize a program's variables, specifically to reduce scope over-exposure.

## Sections
* [Least Exposure](#least-exposure)

[◂ Return to Table of Contents](../README.md)

## Least Exposure
Software engineering articulates a fundamental discipline, typically applied to software security, called [The Principle of Least Privilege](https://en.wikipedia.org/wiki/Principle_of_least_privilege) (PoLP). A variation of this principle applies to this discussion, typically labeled as _Least Exposure_ (PoLE).

PoLP expresses a defensive posture to software architecture: components of a system should be designed to function with least privilege, least access, least exposure. If each piece of a system is connected with minimum-necessary capabilities, the overall system is stronger (from a security standpoint) - compromise or failure of one component has minimized impact on the rest of the system.

PoLP focuses on system-level component design, PoLE focuses on a lower level: this chapter applies it to how scopes interact with each other.

Placing all of a program's variables in a global scope, where they are exposed to all parts of the program via scope, introduces three main hazards:
* **Naming Collisions**: Common names for variables will likely be re-used throughout various parts of a program - storing them in the global scope will lead to naming collisions that can introduce bugs as different parts of the program use the same variable without awareness of each other.
* **Unexpected Behavior**: Exposing variables/functions whose usage is otherwise _private_ allows developers to use these values in ways that were never intended. This can violate expected behaviors and cause bugs. It can also allow malicious actors to circumvent designed limitations or do things never intended possible with the software.
* **Unintended Dependency**: Exposing variables/functions unnecessary invites developers to use and depend on these otherwise _private_ pieces. This creates a refactoring hazard in the future since it couples these global values to a number of difficult-to-trace components within the program.

PoLE, as applied to variable/function scoping, says to default to exposing the bare minimum necessary, keeping everything else as private as possible. Declare variables in as small and as deeply nested scopes as required.

Consider the following:

```javascript
function diff(x,y) {
  if (x > y) {
    let tmp = x;
    x = y;
    y = tmp;
  }

  return y - x;
}

diff(3,7);      // 4
diff(7,5);      // 2
```

In this simple example it doesn't seem to matter whether `tmp` belongs to the function-scope. It definitely should not be a global variable. Following the PoLE principle `tmp` should be as hidden in scope as possible so it is block-scoped to the `if`-block using a `let` declaration.

[▲ Return to Sections](#sections)

| [Previous: Chapter 5 - The (Not So) Secret Lifecycle of Variables](../05/README.md) | [Table of Contents](../README.md#table-of-contents) |
