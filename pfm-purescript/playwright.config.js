export default {
  testDir: './tests/e2e',
  timeout: 30000,
  retries: 2,
  use: {
    baseURL: 'http://localhost:1234',
    headless: true,
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
  projects: [
    {
      name: 'chromium',
      use: {
        channel: 'chrome',
        viewport: { width: 1280, height: 720 }
      },
    },
  ],
  webServer: [
    {
      command: 'spago build && node -e "import(\'./output/Server.Main/index.js\').then(m => m.main())"',
      port: 8080,
      reuseExistingServer: !process.env.CI,
    },
    {
      command: 'spago build && parcel serve --port=1234 index.html',
      port: 1234,
      reuseExistingServer: !process.env.CI,
    },
  ],
};