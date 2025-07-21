export default {
  testDir: './test/e2e',
  timeout: 30000,
  retries: 2,
  use: {
    baseURL: 'http://localhost:4001',
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
      port: 8081,
      reuseExistingServer: !process.env.CI,
    },
    {
      command: 'spago build && parcel serve --port=4001 index.html',
      port: 4001,
      reuseExistingServer: !process.env.CI,
    },
  ],
};