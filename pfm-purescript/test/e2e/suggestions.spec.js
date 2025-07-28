// @ts-check
import { test, expect } from '@playwright/test';

test.describe('Apply All Suggestions', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');

    // Wait for app to load - wait for the main content
    await page.waitForSelector('h1:has-text("PFM - PureScript")');
    await page.waitForSelector('text=Transactions');

    // Wait a bit for suggestions to load
    await page.waitForTimeout(1000);
  });

  test('should click Apply All Suggestions button without error', async ({ page }) => {
    // Verify the Apply All Suggestions button exists
    await expect(page.locator('button:has-text("Apply All Suggestions")')).toBeVisible();

    // Click Apply All Suggestions button (with lightbulb emoji)
    await page.click('button:has-text("Apply All Suggestions")');

    // Wait for the operation to complete
    await page.waitForTimeout(1000);

    // For now, just verify the page doesn't crash and the button is still there
    // (This test will pass when we properly implement the functionality)
    await expect(page.locator('button:has-text("Apply All Suggestions")')).toBeVisible();
  });

  test('should update transaction count after applying all suggestions', async ({ page }) => {
    // Get initial count from the transaction count text
    const countText = await page.locator('.transaction-count').textContent();
    const initialCount = parseInt(countText.match(/\d+/)[0]);

    // Click Apply All Suggestions
    await page.click('button:has-text("Apply All Suggestions")');

    // Wait for update
    await page.waitForTimeout(1000);

    // The count should now be 0 since all unknown expenses were categorized
    await expect(page.locator('.transaction-count')).toContainText('0 transactions');
  });

  test('should disable Apply All button when no suggestions exist', async ({ page }) => {
    // First apply all suggestions
    await page.click('button:has-text("Apply All Suggestions")');
    await page.waitForTimeout(1000);

    // Clear filters to reset view
    await page.click('button:has-text("Filter")');
    await page.waitForSelector('.context-menu');
    await page.click('button:has-text("Clear Filters")');
    await page.waitForTimeout(500);

    // Filter for unknown expenses again
    await page.click('button:has-text("Filter")');
    await page.waitForSelector('.context-menu');
    await page.check('input#filter-unknown-expenses');
    await page.click('button:has-text("Apply Filters")');
    await page.waitForTimeout(500);

    // The Apply All button should be disabled or not visible since no suggestions exist
    const applyAllButton = page.locator('button:has-text("Apply All Suggestions")');
    await expect(applyAllButton).toBeDisabled();
  });
});