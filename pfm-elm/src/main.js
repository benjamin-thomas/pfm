if (import.meta.env.DEV) {
    import('elm-debug-transformer').then(ElmDebugger => {
        ElmDebugger.register({theme: "dark"});
        console.log('[BOOT] Elm debugger transformer activated');
    });
}

// noinspection JSUnresolvedReference
import { Elm } from './Main.elm';

import '../main.css';


const die = (msg) => {
    throw new Error(msg);
}

const getDialogExn = () => document.getElementById("transaction-dialog") || die("Dialog not found");

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
        getDialogExn().showModal();
    });
});

app.ports["closeDialog"].subscribe(() => {
    getDialogExn().close();
});
