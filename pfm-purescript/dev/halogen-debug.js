/**
 * Halogen Debug - Visual State Debugging for PureScript Halogen
 * 
 * Usage:
 * 1. Add script tag: <script src="https://unpkg.com/halogen-debug@latest/dist/halogen-debug.js"></script>
 * 2. Wrap your handleAction with state logging:
 * 
 *   handleAction' action = do
 *     oldState <- H.get
 *     result <- handleAction action
 *     newState <- H.get
 *     liftEffect $ logStateDiff { action, oldState, newState }
 *     pure result
 */

(function () {
  'use strict';

  // State management
  let debugPaused = false;
  let debugEverUsed = false;
  let debugOverlay = null;

  // Create visual debug overlay
  const createDebugOverlay = () => {
    if (debugOverlay) return;

    debugOverlay = document.createElement('div');
    debugOverlay.style.cssText = `
      position: fixed;
      top: 10px;
      right: 10px;
      z-index: 9999;
      background: rgba(0, 0, 0, 0.8);
      color: white;
      padding: 8px 12px;
      border-radius: 4px;
      font-family: monospace;
      font-size: 12px;
      font-weight: bold;
      pointer-events: none;
      transition: opacity 0.2s;
      user-select: none;
    `;
    document.body.appendChild(debugOverlay);
    updateDebugOverlay();
  };

  // Update overlay visual state
  const updateDebugOverlay = () => {
    if (!debugOverlay) return;
    debugOverlay.textContent = debugPaused ? '🛑 DEBUG PAUSED' : '▶️ DEBUG ACTIVE';
    debugOverlay.style.background = debugPaused ? 'rgba(220, 53, 69, 0.8)' : 'rgba(40, 167, 69, 0.8)';
  };

  // Debug controls
  const debugPause = () => {
    debugPaused = true;
    console.log("🛑 Debug logging paused - examine your diffs!");
    updateDebugOverlay();
  };

  const debugResume = () => {
    debugPaused = false;
    console.log("▶️ Debug logging resumed");
    updateDebugOverlay();
  };

  const debugStatus = () => {
    console.log(debugPaused ? "🛑 Debug is PAUSED" : "▶️ Debug is ACTIVE");
  };

  const debugToggle = () => {
    if (!debugEverUsed) return;
    if (debugPaused) {
      debugResume();
    } else {
      debugPause();
    }
  };

  // Diff algorithm
  const createDiff = (left, right, path = '') => {
    const changes = [];

    if (typeof left !== typeof right) {
      changes.push({
        type: 'type-change',
        path,
        oldValue: left,
        newValue: right
      });
      return changes;
    }

    if (left === right) return changes;

    if (Array.isArray(left) && Array.isArray(right)) {
      const maxLen = Math.max(left.length, right.length);
      for (let i = 0; i < maxLen; i++) {
        const newPath = `${path}[${i}]`;
        if (i >= left.length) {
          changes.push({
            type: 'added',
            path: newPath,
            newValue: right[i]
          });
        } else if (i >= right.length) {
          changes.push({
            type: 'deleted',
            path: newPath,
            oldValue: left[i]
          });
        } else {
          changes.push(...createDiff(left[i], right[i], newPath));
        }
      }
    } else if (typeof left === 'object' && left !== null && right !== null) {
      const allKeys = new Set([...Object.keys(left), ...Object.keys(right)]);

      for (const key of allKeys) {
        const newPath = path ? `${path}.${key}` : key;

        if (!(key in left)) {
          changes.push({
            type: 'added',
            path: newPath,
            newValue: right[key]
          });
        } else if (!(key in right)) {
          changes.push({
            type: 'deleted',
            path: newPath,
            oldValue: left[key]
          });
        } else {
          const subChanges = createDiff(left[key], right[key], newPath);
          changes.push(...subChanges);
        }
      }
    } else {
      changes.push({
        type: 'changed',
        path,
        oldValue: left,
        newValue: right
      });
    }

    return changes;
  };

  // Main logging function - exposed globally for PureScript FFI
  const logStateDiff = ({ action, oldState, newState }) => {
    // Activate debug overlay on first use
    if (!debugEverUsed) {
      debugEverUsed = true;
      createDebugOverlay();
      console.log("🔍 Halogen Debug activated! Use dt() to toggle pause/resume");
    }

    if (debugPaused) return; // Skip logging when paused

    console.group("[DEBUG/handleAction]");
    console.log("%c  action: ", "color: #007cba; font-weight: bold", action);
    console.log('%c  oldState:', 'color: #dc3545; font-weight: bold', oldState);
    console.log('%c  newState:', 'color: #28a745; font-weight: bold', newState);

    // Calculate and display diff
    const changes = createDiff(oldState, newState);

    if (changes.length === 0) {
      console.log("✅ No state changes");
      console.groupEnd();
      return;
    }

    const pluralize = (n, singular, plural) => n === 1 ? singular : plural;
    console.group('%cFound ' + changes.length + ' state ' + pluralize(changes.length, 'change', 'changes') + '!', 'color: #666; font-style: italic');

    changes.forEach(change => {
      const path = change.path || 'root';

      switch (change.type) {
        case 'added':
          console.log(
            '%c+ ' + path + ':',
            'background: #d4edda; color: #155724; padding: 2px 4px; border-left: 3px solid #28a745; font-weight: bold',
            change.newValue
          );
          break;

        case 'deleted':
          console.log(
            '%c- ' + path + ':',
            'background: #f8d7da; color: #721c24; padding: 2px 4px; border-left: 3px solid #dc3545; font-weight: bold',
            change.oldValue
          );
          break;

        case 'changed':
          console.log(
            '%c~ ' + path + ':',
            'background: #fff3cd; color: #856404; padding: 2px 4px; border-left: 3px solid #ffc107; font-weight: bold',
            `${change.oldValue} → ${change.newValue}`
          );
          break;

        case 'type-change':
          console.log(
            '%c⚠ ' + path + ' (type changed):',
            'background: #e7e5fb; color: #6f42c1; padding: 2px 4px; border-left: 3px solid #6f42c1; font-weight: bold',
            `${change.oldValue} → ${change.newValue}`
          );
          break;
      }
    });

    console.groupEnd(); // Close the changes group
    console.groupEnd(); // Close the main debug group
  };

  // Console shortcuts (always available)
  const exposeConsoleCommands = () => {
    window.dt = debugToggle;        // "debug toggle"
    window.dp = debugPause;         // "debug pause"
    window.dr = debugResume;        // "debug resume"
    window.ds = debugStatus;        // "debug status"
  };

  // Keyboard shortcut (only works when page is focused)
  const setupKeyboardShortcuts = () => {
    document.addEventListener('keydown', (e) => {
      if (e.ctrlKey && e.key === 'F12') {
        e.preventDefault();
        debugToggle();
      }
    });
  };

  // Initialize when DOM is ready
  const initialize = () => {
    // Expose main function globally for PureScript FFI
    window.logStateDiff = (data) => () => logStateDiff(data);

    // Expose console commands
    exposeConsoleCommands();

    // Setup keyboard shortcuts
    setupKeyboardShortcuts();

    console.log("🔍 Halogen Debug loaded. Use logStateDiff in your handleAction wrapper to activate.");
  };

  // Auto-initialize
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initialize);
  } else {
    initialize();
  }

})();