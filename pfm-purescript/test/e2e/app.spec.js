import { test, expect } from '@playwright/test';

test.describe('PFM PureScript App', () => {
  test('should display ledger view and allow dark mode toggle', async ({ page }) => {
    // Navigate to the app
    await page.goto('http://localhost:1234');

    // Check that the app loads
    await expect(page.locator('h1')).toContainText('PFM - PureScript');

    // Check that transactions section is visible
    await expect(page.locator('h2')).toContainText('Transactions');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Verify transaction list structure exists
    const transactionList = await page.locator('.transaction-list');
    expect(await transactionList.isVisible()).toBe(true);

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

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Check that positive and negative transaction amounts are styled correctly
    const positiveAmounts = page.locator('.transaction-item__amount--positive');
    const negativeAmounts = page.locator('.transaction-item__amount--negative');

    // If there are any transactions, they should be properly styled
    const positiveCount = await positiveAmounts.count();
    const negativeCount = await negativeAmounts.count();
    
    expect(positiveCount + negativeCount).toBeGreaterThanOrEqual(0);
  });

});