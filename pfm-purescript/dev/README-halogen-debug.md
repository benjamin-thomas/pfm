# Halogen Debug

Visual state debugging for PureScript Halogen applications. Get beautiful, colored diff output in your browser console to track state changes in real-time.

## Features

- 🎨 **Beautiful colored console output** - Green for additions, red for deletions, yellow for changes
- 🔍 **Precise diff algorithm** - Only shows actual changes, ignores object property reordering
- ⏸️ **Pause/resume functionality** - Stop logging to examine diffs without console scrolling
- 👁️ **Visual overlay indicator** - Small overlay shows debug status
- 🎯 **Zero overhead when unused** - Complete NOOP if never activated
- 📦 **Standalone script** - No dependencies, works with any PureScript Halogen app

## Quick Start

### 1. Add the script tag

```html
<script src="https://unpkg.com/halogen-debug@latest/dist/halogen-debug.js"></script>
```

### 2. Wrap your handleAction

```purescript
handleAction' :: forall o m. MonadAff m => Action -> HalogenM State Action () o m Unit
handleAction' action = do
  oldState <- H.get
  result <- handleAction action
  newState <- H.get
  liftEffect $ logStateDiff { action, oldState, newState }
  pure result
```

### 3. Use handleAction' instead of handleAction

That's it! State changes will now be logged with beautiful visual diffs.

## Console Commands

Once activated, these commands are available in your browser console:

- `dt()` - Toggle debug pause/resume
- `dp()` - Pause debug logging
- `dr()` - Resume debug logging  
- `ds()` - Show debug status

## Keyboard Shortcuts

- **Ctrl+F12** - Toggle pause/resume (only works when page is focused, not console)

## Example Output

```
🔍 [DEBUG/handleAction]
▶ action: UpdateSearchQuery
▶ oldState: {searchForm: {query: "hel", filters: []}, ...}
▶ newState: {searchForm: {query: "hello", filters: []}, ...}
Found 1 state change!
~ searchForm.query
  old: "hel"
  new: "hello"
```

## Visual Overlay

When debugging is active, a small overlay appears in the top-right corner:
- 🟢 **"▶️ DEBUG ACTIVE"** - Normal logging mode
- 🔴 **"🛑 DEBUG PAUSED"** - Logging paused for examination

## Advanced Usage

### Custom FFI

If you need more control, you can call the function directly:

```javascript
// From PureScript FFI
logStateDiff({
  action: yourAction,
  oldState: previousState,
  newState: currentState
})();
```

### Production Builds

For production, simply remove the script tag or wrap it in a development-only condition:

```html
<!-- Only load in development -->
<script>
  if (location.hostname === 'localhost' || location.hostname === '127.0.0.1') {
    document.write('<script src="./halogen-debug.js"><\/script>');
  }
</script>
```

## Browser Support

Works in all modern browsers with ES6 support. Tested in:
- Chrome 70+
- Firefox 65+
- Safari 12+
- Edge 79+

## License

MIT

## Contributing

This library was extracted from a real PureScript application. Contributions welcome!