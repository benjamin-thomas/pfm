import { test, expect } from '@playwright/test';

test.describe('PFM PureScript App', () => {
  test('should display transactions and allow dark mode toggle', async ({ page }) => {
    // Navigate to the app
    await page.goto('http://localhost:1234');

    // Check that the app loads
    await expect(page.locator('h1')).toContainText('PFM - PureScript');

    // Check that transactions section is visible
    await expect(page.locator('h2')).toContainText('Transactions');

    // Check that mock transactions are displayed
    await expect(page.locator('.transaction-item')).toHaveCount(810);
    await expect(page.locator('.transaction-item').nth(1)).toContainText('APPLE.COM');

    // Test dark mode toggle
    const themeToggle = page.locator('.theme-toggle');
    await expect(themeToggle).toBeVisible();

    // Click to enable dark mode
    await themeToggle.click();

    // Check that dark theme class was added to document
    await expect(page.locator('html')).toHaveClass(/dark-theme/);

    // Click again to disable dark mode
    await themeToggle.click();

    // Check that dark theme class was removed
    await expect(page.locator('html')).not.toHaveClass(/dark-theme/);
  });

  test('should display transaction amounts with correct styling', async ({ page }) => {
    await page.goto('http://localhost:1234');

    // Check positive amount
    const positiveAmount = page.locator('.transaction-item__amount--positive').first();
    await expect(positiveAmount).toContainText('18.2€');

    // FIXME: Check negative amount 
    // const negativeAmount = page.locator('.transaction-item__amount--negative').first();
    // await expect(negativeAmount).toContainText('$-125.5');
  });
});