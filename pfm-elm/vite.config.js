import { defineConfig } from 'vite';
import { plugin as elmPlugin } from 'vite-plugin-elm';

export default defineConfig({
  plugins: [elmPlugin()],
  server: {
    port: 3000
  },
  build: {
    rollupOptions: {
      input: {
        main: '/index.html',
        main2: '/index2.html'
      }
    }
  }
});
