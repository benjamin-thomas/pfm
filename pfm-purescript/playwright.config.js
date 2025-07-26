export default {
  testDir: './test/e2e',
  timeout: 30000,
  retries: 2,
  use: {
    baseURL: 'http://localhost:4002', // Test client port
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
      command: 'spago build && APP_ENV=test ./manage/server', // Uses APP_ENV=test
      port: 8082, // Test server port
      reuseExistingServer: !process.env.CI,
    },
    {
      command: 'npm run client:test', // Test client
      port: 4002, // Test client port
      reuseExistingServer: !process.env.CI,
    },
  ],
};