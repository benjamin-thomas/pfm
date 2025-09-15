import {defineConfig} from 'vitest/config';

export default defineConfig({
    test: {
        testTimeout: 60000,
        environment: 'node',
        globals: true,
        typecheck: {
            enabled: true
        },
        include: ['src/**/*.{test,spec}.{js,ts}'],
        exclude: ['node_modules/**', 'dist/**'],
    },
    resolve: {
        alias: {
            '@shared': '../shared',
            '@src': './src',
        },
    },
});