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

