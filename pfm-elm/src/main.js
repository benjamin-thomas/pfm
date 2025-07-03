const COLOR_SCHEME_OVERRIDE = 'prefers-color-scheme-override';

const getSysColorScheme = () => {
    return window.matchMedia('(prefers-color-scheme: dark)').matches ? "dark" : "light";
};

const sysColorScheme = getSysColorScheme();
const userColorScheme = localStorage.getItem(COLOR_SCHEME_OVERRIDE);

if (import.meta.env.DEV) {
    /*
     NOTE
     There's a dedicated vite plugin, but capped at vite v4 currently.
     So, there doesn't seem to be an advantage to using that plugin, apart from not having to define this code block.
     There's no need to track the loaded state of the registry here either, there's no "double registry".
    */
    import('elm-debug-transformer').then((transformer) => {
        transformer.register({ theme: sysColorScheme }); // must always use the system color scheme to match chrome devtools
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
    if (userColorScheme === 'dark') {
        document.documentElement.classList.add('dark-theme');
    } else if (userColorScheme === 'light') {
        document.documentElement.classList.remove('dark-theme');
    } else {
        // If no manual preference, use system preference
        if (sysColorScheme === 'dark') {
            document.documentElement.classList.add('dark-theme');
        }

        // Listen for changes to the system preference
        window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', event => {
            // Don't change if user has set a preference manually during running session. 
            if (localStorage.getItem(COLOR_SCHEME_OVERRIDE)) return;

            if (event.matches) {
                document.documentElement.classList.add('dark-theme');
            } else {
                document.documentElement.classList.remove('dark-theme');
            }
        });
    }
});

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
    // Store user's manual preference
    localStorage.setItem(COLOR_SCHEME_OVERRIDE, isDarkTheme ? 'dark' : 'light');
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

const debounce = (func, delay) => {
    let timeout;
    return (...args) => {
        clearTimeout(timeout);
        timeout = setTimeout(() => func(...args), delay);
    };
};

window.addEventListener("scroll", debounce(() => {
    app.ports["rcvScrollY"].send(window.scrollY);
}, 100));

app.ports["restoreScrollY"].subscribe((scrollY) => {
    // Using two RAFs ensures DOM has settled
    window.requestAnimationFrame(() => {
        window.requestAnimationFrame(() => {
            console.log(`[APP] Restoring scroll position: ${scrollY}px`);
            window.scrollTo(0, scrollY);
            console.log(`[APP] Restored scroll position: ${scrollY}px`);
        });
    });
});

document.addEventListener("keyup", (event) => {
    switch (event.key) {
        case "Enter":
            app.ports["enterPressed"].send(null);
            break;
        case "Escape":
            app.ports["escapePressed"].send(null);
            break;
    }
});
