## How we should work

Let's work in very small batches in this project.

I'll give you auto-write commit, but then I want you to stop early and often, at each "major" and logical step. I'll review your code, probably make adjustments, and git commit, then we'll carry on.

Always run `npm run test:all` before getting back to me (and make sure teh code compiles!). If it's good, add a "tada" emoji.

In case of troubles, always refer to the working approach here: ~/code/explore/purescript/ps-fullstack3

## Goal

The goal is to replace my Elm+Haskell app (a work in progress), with a fullstack PureScript solution.

This should provide less friction when crossing the frontend/backend worlds.


## JS code style

- always favor using arrow functions

## PureScript code style

- When porting over the haskell code, we don't need to add "prefixes" to record types.
- Use `Decimal` type for currency amounts with exactly 2 decimal places (e.g., "123.45") instead of storing as cents in BigInt. This avoids precision issues and makes the code more readable.
- Convert from string representations (like OFX amounts) to Decimal as early as possible in the parsing pipeline.

## Dependency management

To maintain clarity about why JavaScript dependencies are needed in this PureScript project, we use a `dependencies:comments` section in `package.json`. This is a non-standard but useful convention that documents:

- **Why** each dependency is needed
- **Which PureScript package** requires it
- **What functionality** it provides

Example:
```json
{
  "dependencies": {
    "sqlite3": "^5.1.7"
  },
  "dependencies:comments": {
    "sqlite3": [
      "Database backend for storing financial data",
      "Used by the server-side PureScript code via the 'node-sqlite3' package"
    ]
  }
}
```

This approach helps future maintainers understand the dependency graph and avoid accidentally removing required packages.

Make sure to update these "comments" as we add or remove JS dependencies.