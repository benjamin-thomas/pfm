// Theme effects - handle DOM side effects for theming

export const applyTheme = (isDarkMode: boolean): void => {
  if (isDarkMode) {
    document.documentElement.classList.add('dark-theme');
  } else {
    document.documentElement.classList.remove('dark-theme');
  }
};