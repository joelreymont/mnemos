import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: '.',
  testMatch: '*.e2e.ts',
  timeout: 60000,
  retries: 0,
  workers: 1, // VS Code tests must run serially
  reporter: 'list',
  use: {
    trace: 'on-first-retry',
  },
});
