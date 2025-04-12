// noinspection JSUnresolvedReference
import {Elm} from './Main.elm';

import '../main.css';

document.addEventListener('DOMContentLoaded', function () {
    const savedTheme = localStorage.getItem('theme');
    if (savedTheme === 'dark') {
        document.documentElement.classList.add('dark-theme');
    }
});

// Initialize the Elm application
const app = Elm.Main.init({
    node: document.getElementById('app')
});

// Set up event listeners
document.addEventListener("keyup", (event) => {
    if (event.key === "Escape") {
        app.ports["escapePressed"].send(null);
    } else if (event.key === "Enter") {
        app.ports["enterPressed"].send(null);
    }
});

app.ports["consoleLogRaw"].subscribe((x) => {
    if (!x.title) {
        console.log(`[APP]`, x);
    } else {
        if (x.data === null) {
            console.log(`[APP] ${x.title}`);
        } else {
            console.log(`[APP] ${x.title}`, x.data);
        }
    }
});

app.ports["toggleTheme"].subscribe(function () {
    const isDarkTheme = document.documentElement.classList.toggle('dark-theme');
    localStorage.setItem('theme', isDarkTheme ? 'dark' : 'light');
});

app.ports["showDialog"].subscribe(() => {
    /*
     * We use requestAnimationFrame because we must wait for the dialog from the Elm app to be rendered before
     * trying to show it.
     */
    requestAnimationFrame(() => {
        const dialog = document.getElementById("transaction-dialog");
        if (dialog) {
            dialog.showModal();
        } else {
            console.error("Dialog not found");
        }
    });
});

app.ports["closeDialog"].subscribe(() => {
    const dialog = document.getElementById("transaction-dialog");
    if (dialog) {
        dialog.close();
    }
});
