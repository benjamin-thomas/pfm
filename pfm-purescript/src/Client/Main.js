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