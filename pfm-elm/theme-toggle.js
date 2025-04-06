// Theme toggle functionality
document.addEventListener('DOMContentLoaded', function() {
  // Check for saved theme preference or use default
  const savedTheme = localStorage.getItem('theme');
  if (savedTheme === 'dark') {
    document.documentElement.classList.add('dark-theme');
  }
});

// This function will be called from Elm when the theme toggle button is clicked
function setupThemeToggle(app) {
  // Store the Elm app instance
  window.app = app;
  
  // Subscribe to the toggleTheme port from Elm
  if (app.ports && app.ports.toggleTheme) {
    app.ports.toggleTheme.subscribe(function() {
      const isDarkTheme = document.documentElement.classList.toggle('dark-theme');
      // Save the theme preference
      localStorage.setItem('theme', isDarkTheme ? 'dark' : 'light');
    });
  } else {
    console.error("toggleTheme port not found. Make sure it's defined in your Elm app.");
  }
}
