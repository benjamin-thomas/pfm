/**
 * Halogen Inspector - Visual State Inspection for PureScript Halogen
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
  let inspectorStopped = true;  // Start stopped initially
  let inspectorEverUsed = false;
  let inspectorOverlay = null;
  
  // Position management
  const POSITIONS = ['tr', 'br', 'bl', 'tl'];
  let currentPosition = localStorage.getItem('halogen-inspector-position') || 'tr';
  
  // Ensure valid position
  if (!POSITIONS.includes(currentPosition)) {
    currentPosition = 'tr';
  }
  
  // Snapshot functionality
  let lastState = null;
  let startState = null;
  let endState = null;
  let accumulatedActions = [];

  // Get position styles
  const getPositionStyles = (position) => {
    const positions = {
      'tr': { top: '10px', right: '10px' },
      'br': { bottom: '10px', right: '10px' },
      'bl': { bottom: '10px', left: '10px' },
      'tl': { top: '10px', left: '10px' }
    };
    return positions[position];
  };

  // Cycle through positions
  const cyclePosition = () => {
    const currentIndex = POSITIONS.indexOf(currentPosition);
    const nextIndex = (currentIndex + 1) % POSITIONS.length;
    currentPosition = POSITIONS[nextIndex];
    
    // Save to localStorage
    localStorage.setItem('halogen-inspector-position', currentPosition);
    
    // Update position
    updateInspectorPosition();
  };

  // Update position without changing other styles
  const updateInspectorPosition = () => {
    if (!inspectorOverlay) return;
    
    // Clear all position properties
    inspectorOverlay.style.top = '';
    inspectorOverlay.style.right = '';
    inspectorOverlay.style.bottom = '';
    inspectorOverlay.style.left = '';
    
    // Apply new position
    const position = getPositionStyles(currentPosition);
    Object.assign(inspectorOverlay.style, position);
  };

  // Create visual debug overlay
  const createInspectorOverlay = () => {
    if (inspectorOverlay) return;

    inspectorOverlay = document.createElement('div');
    inspectorOverlay.style.cssText = `
      position: fixed;
      z-index: 9999;
      background: rgba(0, 0, 0, 0.8);
      color: white;
      padding: 8px 12px;
      border-radius: 4px;
      font-family: monospace;
      font-size: 12px;
      font-weight: bold;
      pointer-events: auto;
      transition: all 0.3s ease;
      user-select: none;
      cursor: pointer;
    `;
    
    // Click handler to cycle position
    inspectorOverlay.addEventListener('click', (e) => {
      e.stopPropagation();
      cyclePosition();
    });
    
    document.body.appendChild(inspectorOverlay);
    updateInspectorPosition();
    updateInspectorOverlay();
  };

  // Update overlay visual state
  const updateInspectorOverlay = () => {
    if (!inspectorOverlay) return;
    inspectorOverlay.textContent = inspectorStopped ? '🛑 INSPECTOR STOPPED' : '▶️ INSPECTOR ACTIVE';
    inspectorOverlay.style.background = inspectorStopped ? 'rgba(220, 53, 69, 0.8)' : 'rgba(40, 167, 69, 0.8)';
  };

  // Inspector controls
  const inspectorStop = () => {
    inspectorStopped = true;
    endState = lastState;
    
    console.group("🔍 Inspector Session Summary");
    
    // Actions summary - always show
    console.log(`📊 ${accumulatedActions.length} actions occurred during session:`);
    accumulatedActions.forEach((action, index) => {
      console.log(`  ${index + 1}.`, action);
    });
    
    // State changes summary - defensive programming
    if (!startState) {
      console.error("❌ Cannot show state changes: startState is null");
      console.groupEnd();
      return;
    }
    
    if (!endState) {
      console.error("❌ Cannot show state changes: endState is null");
      console.groupEnd();
      return;
    }
    
    const changes = createDiff(startState, endState);
    if (changes.length > 0) {
      console.group(`📈 Accumulated state changes (${changes.length} total):`);
      changes.forEach((change, index) => {
        const path = change.path || 'root';
        const num = `${index + 1}.`;
        switch (change.type) {
          case 'added':
            console.log(`%c+ ${num} ${path}:`, 'color: #28a745; font-weight: bold', change.newValue);
            break;
          case 'deleted':
            console.log(`%c- ${num} ${path}:`, 'color: #dc3545; font-weight: bold', change.oldValue);
            break;
          case 'changed':
            console.log(`%c~ ${num} ${path}:`, 'color: #ffc107; font-weight: bold', `${change.oldValue} → ${change.newValue}`);
            break;
          case 'type-change':
            console.log(`%c⚠ ${num} ${path}:`, 'color: #6f42c1; font-weight: bold', `${change.oldValue} → ${change.newValue} (type changed)`);
            break;
        }
      });
      console.groupEnd();
    } else {
      console.log("✅ No accumulated state changes");
    }
    
    console.groupEnd();
    console.log("🛑 Inspector session stopped - showing summary!");
    updateInspectorOverlay();
  };

  const inspectorStart = () => {
    inspectorStopped = false;
    startState = lastState; // Capture current state immediately
    accumulatedActions = [];
    console.log("▶️ Inspector session started");
    console.log("📸 Current state captured as session start");
    updateInspectorOverlay();
  };

  const inspectorStatus = () => {
    console.log(inspectorStopped ? "🛑 Inspector is STOPPED" : "▶️ Inspector is ACTIVE");
  };

  const inspectorToggle = () => {
    // Always allow toggle since overlay is always visible now
    if (inspectorStopped) {
      inspectorStart();
    } else {
      inspectorStop();
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
    // Always update lastState to track current state (zero performance impact)
    lastState = newState;
    
    if (inspectorStopped) return; // Early exit if stopped
    
    // Accumulate actions during active session
    accumulatedActions.push(action);
    
    // Activate inspector overlay on first use
    if (!inspectorEverUsed) {
      inspectorEverUsed = true;
      createInspectorOverlay();
      console.log("🔍 Halogen Inspector activated! Use Ctrl+F12 to start/stop inspection sessions");
    }

    console.group("[INSPECTOR/handleAction]");
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
    // Expose for keyboard shortcut access
    window.inspectorToggle = inspectorToggle;
  };

  // Keyboard shortcut (only works when page is focused)
  const setupKeyboardShortcuts = () => {
    document.addEventListener('keydown', (e) => {
      if (e.ctrlKey && e.key === 'F12') {
        e.preventDefault();
        inspectorToggle();
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

    // Create overlay immediately to show it's loaded
    createInspectorOverlay();

    console.log("🔍 Halogen Inspector loaded. Use logStateDiff in your handleAction wrapper to activate. Click inspector to reposition (tr → br → bl → tl).");
  };

  // Auto-initialize
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initialize);
  } else {
    initialize();
  }

})();