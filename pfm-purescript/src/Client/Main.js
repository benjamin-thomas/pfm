export const toggleTheme = () => () => {
  const isDarkTheme = document.documentElement.classList.toggle('dark-theme');
  localStorage.setItem('COLOR_THEME', isDarkTheme ? 'dark' : 'light');
};

export const dialogShow = (dialogId) => () => {
  const dialog = document.getElementById(dialogId);
  if (dialog) {
    dialog.showModal();
  }
};

export const dialogClose = (dialogId) => () => {
  const dialog = document.getElementById(dialogId);
  if (dialog) {
    dialog.close();
  }
};

// Convert Unix timestamp to datetime-local format (YYYY-MM-DDTHH:mm)
export const formatUnixToDateTimeLocal = (unixTimestamp) => {
  // console.log("formatUnixToDateTimeLocal", unixTimestamp);
  const date = new Date(unixTimestamp * 1000); // Convert seconds to milliseconds
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const day = String(date.getDate()).padStart(2, '0');
  const hours = String(date.getHours()).padStart(2, '0');
  const minutes = String(date.getMinutes()).padStart(2, '0');
  return `${year}-${month}-${day}T${hours}:${minutes}`;
};

// Parse datetime-local string (YYYY-MM-DDTHH:mm) to Unix timestamp
export const parseDateTimeLocal = (dateTimeStr) => {
  const date = new Date(dateTimeStr);
  return Math.floor(date.getTime() / 1000); // Convert milliseconds to seconds
};

// Native browser confirmation dialog
export const confirmDialog = (message) => () => {
  return window.confirm(message);
};

// Get current datetime in datetime-local format (YYYY-MM-DDTHH:mm)
export const getCurrentDateTimeLocal = () => {
  const now = new Date();
  const year = now.getFullYear();
  const month = String(now.getMonth() + 1).padStart(2, '0');
  const day = String(now.getDate()).padStart(2, '0');
  const hours = String(now.getHours()).padStart(2, '0');
  const minutes = String(now.getMinutes()).padStart(2, '0');
  return `${year}-${month}-${day}T${hours}:${minutes}`;
};

export const getScrollY = () => window.scrollY;

// Restore scroll Y position
export const restoreScrollY = (scrollY) => () => {
  // Use requestAnimationFrame to ensure DOM is updated before scrolling
  requestAnimationFrame(() => {
    window.scrollTo(0, scrollY);
  });
};

// SSE functionality using Halogen subscriptions
export const setupSSEConnection = (apiBaseUrl) => (callback) => () => {
  console.log('[SSE/JS] Setting up connection to:', apiBaseUrl + '/events');

  const eventSource = new EventSource(apiBaseUrl + '/events');

  eventSource.onopen = () => {
    console.log('[SSE/JS] Connection opened');
  };

  // Handle all SSE messages through onmessage (no specific event types)
  eventSource.onmessage = (event) => {
    console.log('[SSE/JS] Received message:', event.data);
    callback(event.data)();
  };

  eventSource.onerror = (error) => console.error('[SSE/JS] Error:', error);


  return eventSource;
};

// Reusable debounce utility - prevents a function from being called too frequently
const debounce = (fn, delayMs) => {
  let timeoutId = null;

  return () => {
    // Cancel any pending execution
    if (timeoutId !== null) {
      clearTimeout(timeoutId);
    }

    // Schedule the function to run after the delay
    timeoutId = setTimeout(fn, delayMs);
  };
};

// Scroll position tracking with debounced updates
export const setupScrollTracking = (callback) => () => {
  // Create a debounced version of our scroll handler
  const debouncedScrollHandler = debounce(() => {
    callback(window.scrollY)();
  }, 100); // Wait 100ms after scrolling stops before updating

  // Start listening for scroll events
  window.addEventListener('scroll', debouncedScrollHandler);

  // Send the initial scroll position immediately
  callback(window.scrollY)();

  // Return cleanup function
  return () => {
    window.removeEventListener('scroll', debouncedScrollHandler);
  };
};

export const clearConsole = () => {
  // console.clear();
  console.log("\n\n\n\n\n\n\n");
};

export const logStateDiff = ({ action, oldState, newState }) => () => {
  console.group("[DEBUG/handleAction]");
  // console.group("Data")
  console.log("%c  action: ", "color: #007cba; font-weight: bold", action)
  console.log('%c  oldState:', 'color: #dc3545; font-weight: bold', oldState);
  console.log('%c  newState:', 'color: #28a745; font-weight: bold', newState);
  // console.groupEnd()

  // Simple diff implementation
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

  const changes = createDiff(oldState, newState);

  if (changes.length === 0) {
    return;
  }

  // console.group(`%c🔍 ${label}`, 'color: #007cba; font-weight: bold; font-size: 16px');
  const pluralize = (n, singular, plural) => n === 1 ? singular : plural;
  // console.log(label);
  console.log('%cFound ' + changes.length + ' state ' + pluralize(changes.length, 'change', 'changes') + '!', 'color: #666; font-style: italic');
  // console.log('');

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
        console.group(
          '%c~ ' + path,
          'background: #fff3cd; color: #856404; padding: 2px 4px; border-left: 3px solid #ffc107; font-weight: bold'
        );
        console.log('%c  old:', 'color: #dc3545; font-weight: bold', change.oldValue);
        console.log('%c  new:', 'color: #28a745; font-weight: bold', change.newValue);
        console.groupEnd();
        break;

      case 'type-change':
        console.group(
          '%c⚠ ' + path + ' (type changed)',
          'background: #e7e5fb; color: #6f42c1; padding: 2px 4px; border-left: 3px solid #6f42c1; font-weight: bold'
        );
        console.log('%c  old:', 'color: #dc3545; font-weight: bold', change.oldValue);
        console.log('%c  new:', 'color: #28a745; font-weight: bold', change.newValue);
        console.groupEnd();
        break;
    }
  });

  console.groupEnd();
  console.groupEnd()
};

