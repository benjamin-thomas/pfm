
export const unsafeStringify = (value) => {
    return JSON.stringify(value, null, 4);
};

const getElementByIdExn = (id) => {
    const el = document.getElementById(id);
    if (!el) throw new Error(`Element not found: ${id}`);
    return el;
};

export const dialogShow = (id) => () => {
    getElementByIdExn(id).showModal();
};

export const dialogClose = (id) => () => {
    console.log("Closing dialog...", { id });
    getElementByIdExn(id).close();
};

export const padRight = (n) => (char) => (str) => {
    return str.padEnd(n, char);
};

export const dateFmtForUser = (instant) => {
    return new Intl.DateTimeFormat("fr", {
        dateStyle: "medium",
        timeStyle: "short",
    }).format(instant);
};

export const dateFmtForSave = (str) => {
    const local = new Date(str);
    return local.getTime();
};

export const dateFmtForInput = (instant) => {
    const date = new Date(instant);
    const offset = date.getTimezoneOffset() * 60000;
    const localDate = new Date(instant - offset);
    return localDate.toISOString().slice(0, 16);
};
