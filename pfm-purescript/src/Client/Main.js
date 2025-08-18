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
export const setupSSE_Connection = (apiBaseUrl) => (callback) => () => {
  const eventSource = new EventSource(apiBaseUrl + '/events');

  // Send the SSE messages to the Halogen app
  eventSource.onmessage = (event) => callback(event.data)();
  eventSource.onerror = (error) => console.error('[SSE/JS] Error:', error);

  return eventSource;
};

const debounce = (fn, delayMs) => {
  let timeoutId = null;

  return () => {
    // Cancel any pending execution
    if (timeoutId !== null) {
      clearTimeout(timeoutId);
    }

    // Schedule the function to run after the specified delay
    timeoutId = setTimeout(fn, delayMs);
  };
};

export const setupScrollTracking = (callback) => () => {
  const sendScroll = (scrollY) => callback(scrollY)();

  // Send the initial scroll position immediately
  sendScroll(window.scrollY);

  const onScroll = debounce(() => sendScroll(window.scrollY), 100);
  window.addEventListener('scroll', onScroll);

  // Return cleanup function
  return () => window.removeEventListener('scroll', onScroll);
};

// Use the global logStateDiff from halogen-debug.js
export const logStateDiff = ({ action, oldState, newState }) => window.logStateDiff({ action, oldState, newState });

// Focus an element by CSS selector
export const focusElementBySelector = (selector) => () => {
  const element = document.querySelector(selector);
  if (element) {
    element.focus();
  }
};


