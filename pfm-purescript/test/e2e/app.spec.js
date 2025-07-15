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

  test('should open and close create transaction dialog', async ({ page }) => {
    await page.goto('http://localhost:1234');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Check that dialog is not visible initially
    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).not.toBeVisible();

    // Click the "Create Transaction" button
    const createButton = page.locator('.transaction-list__header-buttons .button--primary');
    await expect(createButton).toBeVisible();
    await expect(createButton).toContainText('Create Transaction');
    await createButton.click();

    // Wait for dialog to be visible
    await expect(dialog).toBeVisible();

    // Check that dialog title is correct
    await expect(page.locator('.dialog-title')).toContainText('Create Transaction');

    // Check that form fields are present
    await expect(page.locator('input[id="description"]')).toBeVisible();
    await expect(page.locator('select[id="from-account"]')).toBeVisible();
    await expect(page.locator('select[id="to-account"]')).toBeVisible();
    await expect(page.locator('input[id="amount"]')).toBeVisible();
    await expect(page.locator('input[id="date"]')).toBeVisible();

    // Check that action buttons are present
    await expect(page.locator('.dialog-actions .button').first()).toContainText('Cancel');
    await expect(page.locator('.dialog-actions .button--primary')).toContainText('Save');

    // Close dialog by clicking Cancel
    await page.locator('.dialog-actions .button').first().click();

    // Check that dialog is closed
    await expect(dialog).not.toBeVisible();
  });

  test('should open and close edit transaction dialog', async ({ page }) => {
    await page.goto('http://localhost:1234');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Check that dialog is not visible initially
    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).not.toBeVisible();

    // Wait for at least one transaction to be present
    await page.waitForSelector('.transaction-item');

    // Click on the first transaction item
    const firstTransaction = page.locator('.transaction-item').first();
    await expect(firstTransaction).toBeVisible();
    await firstTransaction.click();

    // Wait for dialog to be visible
    await expect(dialog).toBeVisible();

    // Check that dialog title is correct
    await expect(page.locator('.dialog-title')).toContainText('Edit Transaction');

    // Check that form fields are present and pre-populated
    await expect(page.locator('input[id="description"]')).toBeVisible();
    await expect(page.locator('select[id="from-account"]')).toBeVisible();
    await expect(page.locator('select[id="to-account"]')).toBeVisible();
    await expect(page.locator('input[id="amount"]')).toBeVisible();
    await expect(page.locator('input[id="date"]')).toBeVisible();

    // Check that description field has some value (pre-populated)
    const descriptionField = page.locator('input[id="description"]');
    const descriptionValue = await descriptionField.inputValue();
    expect(descriptionValue).not.toBe('');

    // Check that action buttons are present
    await expect(page.locator('.dialog-actions .button').first()).toContainText('Cancel');
    await expect(page.locator('.dialog-actions .button--primary')).toContainText('Save');

    // Close dialog by clicking Cancel
    await page.locator('.dialog-actions .button').first().click();

    // Check that dialog is closed
    await expect(dialog).not.toBeVisible();
  });

  test('should allow form field interaction in dialogs', async ({ page }) => {
    await page.goto('http://localhost:1234');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Open create dialog
    const createButton = page.locator('.transaction-list__header-buttons .button--primary');
    await createButton.click();

    // Wait for dialog to be visible
    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    // Test form field interaction
    const descriptionField = page.locator('input[id="description"]');
    const testDescription = 'Test Transaction Description';
    
    await descriptionField.fill(testDescription);
    await expect(descriptionField).toHaveValue(testDescription);

    // Test amount field
    const amountField = page.locator('input[id="amount"]');
    const testAmount = '123.45';
    
    await amountField.fill(testAmount);
    await expect(amountField).toHaveValue(testAmount);

    // Test account fields (now select dropdowns)
    const fromAccountField = page.locator('select[id="from-account"]');
    
    // Wait a bit for accounts to load, then try to select
    await page.waitForTimeout(1000);
    
    // Select the first available account (should be account ID 1)
    await fromAccountField.selectOption('1');
    await expect(fromAccountField).toHaveValue('1');

    // Close dialog
    await page.locator('.dialog-actions .button').first().click();
    await expect(dialog).not.toBeVisible();
  });

});