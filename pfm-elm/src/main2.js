// noinspection JSUnresolvedReference
import {Elm} from './Main2.elm';

import '../main2.css';

document.addEventListener('DOMContentLoaded', function () {
    const savedTheme = localStorage.getItem('theme');
    if (savedTheme === 'dark') {
        document.documentElement.classList.add('dark-theme');
    }
});

const app = Elm.Main2.init({
    node: document.getElementById('app')
});

app.ports["showDialog"].subscribe(() => {
    /*
     * We use requestAnimationFrame because we must wait for the dialog from the Elm app to be rendered before
     * trying to show it.
     */
    requestAnimationFrame(() => {
        const dialog = document.getElementById("myDialog");
        if (dialog) {
            dialog.showModal();
        } else {
            console.error("Dialog not found");
        }
    });
});

app.ports["closeDialog"].subscribe(() => {
    const dialog = document.getElementById("myDialog");
    if (dialog) {
        dialog.close();
    }
});
