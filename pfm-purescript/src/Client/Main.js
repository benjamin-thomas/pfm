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