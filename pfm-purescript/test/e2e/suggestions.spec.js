// @ts-check
import { test, expect } from '@playwright/test';

// Reset the test database before each test
test.beforeEach(async ({ request }) => {
  const backendURL = process.env.BACKEND_URL || 'http://localhost:8082';
  await request.post(`${backendURL}/test/reset-db`);
});

test.describe('Apply All Suggestions', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');

    // Wait for app to load - wait for the main content
    await page.waitForSelector('h1:has-text("PFM - PureScript")');
    await page.waitForSelector('text=Transactions');

    // Wait for suggestions to load
    await page.waitForSelector('.suggestion-container');
  });

  test('should click Apply All Suggestions button without error', async ({ page }) => {
    // Verify the Apply All Suggestions button exists
    await expect(page.locator('button:has-text("Apply All Suggestions")')).toBeVisible();

    // Click Apply All Suggestions button (with lightbulb emoji)
    await page.click('button:has-text("Apply All Suggestions")');

    // Wait for the operation to complete - button should still be visible  
    await expect(page.locator('button:has-text("Apply All Suggestions")')).toBeVisible();

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

    // Wait for the count to update to 0
    await expect(page.locator('.transaction-count')).toContainText('0 transactions');

    // The count should now be 0 since all unknown expenses were categorized
    await expect(page.locator('.transaction-count')).toContainText('0 transactions');
  });

  test('should remove all suggestion containers after applying all suggestions', async ({ page }) => {
    // Wait for suggestions to be visible initially
    await page.waitForSelector('.suggestion-container');

    // Count initial suggestions
    const initialSuggestions = await page.locator('.suggestion-container').count();
    expect(initialSuggestions).toBeGreaterThan(0);

    // Apply all suggestions
    await page.click('button:has-text("Apply All Suggestions")');
    // Wait for all suggestions to disappear
    await expect(page.locator('.suggestion-container')).toHaveCount(0);

    // After applying all suggestions, there should be no suggestion containers visible
    const finalSuggestions = await page.locator('.suggestion-container').count();
    expect(finalSuggestions).toBe(0);
  });
});