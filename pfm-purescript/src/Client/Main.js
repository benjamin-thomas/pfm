export const toggleTheme = () => () => {
    const isDarkTheme = document.documentElement.classList.toggle('dark-theme');
    localStorage.setItem('COLOR_THEME', isDarkTheme ? 'dark' : 'light');
};