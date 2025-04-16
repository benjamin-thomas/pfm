
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

export const dateFmt = (instant) => {
    // return new Date(instant).toISOString()
    return new Intl.DateTimeFormat("fr", {
        dateStyle: "medium",
        timeStyle: "short",
    }).format(instant);
};
