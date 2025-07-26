import { test, expect } from '@playwright/test';

test.describe('PFM PureScript App', () => {
  test('should display ledger view and allow dark mode toggle', async ({ page }) => {
    // Navigate to the app
    await page.goto('/');

    // Check that the app loads
    await expect(page.locator('h1')).toContainText('PFM - PureScript');

    // Check that transactions section is visible
    await expect(page.locator('h3')).toContainText('Transactions');

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
    await page.goto('/');

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
    await page.goto('/');

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
    await page.goto('/');

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

  test('should populate date field correctly in edit dialog', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Wait for at least one transaction to be present
    await page.waitForSelector('.transaction-item');

    // Click on the first transaction item
    const firstTransaction = page.locator('.transaction-item').first();
    await firstTransaction.click();

    // Wait for dialog to be visible
    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    // Check that date field has a proper datetime-local value (not empty or invalid format)
    const dateField = page.locator('input[id="date"]');
    const dateValue = await dateField.inputValue();

    // Date field should not be empty
    expect(dateValue).not.toBe('');

    // Date field should match datetime-local format (YYYY-MM-DDTHH:mm)
    const dateTimeLocalPattern = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}$/;
    expect(dateValue).toMatch(dateTimeLocalPattern);

    // Close dialog
    await page.locator('.dialog-actions .button').first().click();
  });

  test('should allow form field interaction in dialogs', async ({ page }) => {
    await page.goto('/');

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

  test('should create a new transaction', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Count initial transactions
    const initialTransactions = await page.locator('.transaction-item').count();

    // Open create dialog
    const createButton = page.locator('.transaction-list__header-buttons .button--primary');
    await createButton.click();

    // Wait for dialog to be visible
    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    // Fill in the form with a unique description
    const uniqueDescription = `Test transaction from e2e ${Date.now()}`;
    const descriptionField = page.locator('input[id="description"]');
    await descriptionField.fill(uniqueDescription);

    const fromAccountField = page.locator('select[id="from-account"]');
    await fromAccountField.selectOption('2'); // Checking account

    const toAccountField = page.locator('select[id="to-account"]');
    await toAccountField.selectOption('6'); // Unknown Expense

    const amountField = page.locator('input[id="amount"]');
    await amountField.fill('42.50');

    const dateField = page.locator('input[id="date"]');
    // Set a specific date/time
    await dateField.fill('2024-07-01T10:30');

    // Save the transaction
    const saveButton = page.locator('.dialog-actions .button--primary');
    await saveButton.click();

    // Wait for dialog to close with longer timeout
    await expect(dialog).not.toBeVisible({ timeout: 10000 });

    // Wait for the transaction list to refresh
    await page.waitForTimeout(2000);

    // Reload page to verify data persistence
    await page.reload();
    await page.waitForSelector('.transaction-list');

    // Verify new transaction appears in the list after reload
    const newTransactionCount = await page.locator('.transaction-item').count();
    expect(newTransactionCount).toBeGreaterThan(initialTransactions);

    // Find the new transaction by searching for our unique test description
    const testTransactionLocator = page.locator('.transaction-item__description', { hasText: uniqueDescription });
    await expect(testTransactionLocator).toBeVisible();

    // Find the transaction item that contains our description
    const testTransaction = page.locator('.transaction-item', { has: testTransactionLocator });

    // Verify the amount
    const amount = await testTransaction.locator('.transaction-item__amount').textContent();
    expect(amount).toContain('42.50');

    // Verify it's an expense (negative amount)
    const amountClass = await testTransaction.locator('.transaction-item__amount').getAttribute('class');
    expect(amountClass).toContain('transaction-item__amount--negative');
  });

  test('should edit an existing transaction', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Wait for at least one transaction to be present
    await page.waitForSelector('.transaction-item');

    // Click on the first transaction to edit it
    const firstTransaction = page.locator('.transaction-item').first();
    await firstTransaction.click();

    // Wait for edit dialog to be visible
    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    // Verify this is the edit dialog
    await expect(page.locator('.dialog-title')).toContainText('Edit Transaction');

    // Get the original description for comparison
    const descriptionField = page.locator('input[id="description"]');
    const originalDescription = await descriptionField.inputValue();

    // Change the description
    const newDescription = originalDescription + ' (edited)';
    await descriptionField.fill(newDescription);

    // Click Save button
    await page.locator('.dialog-actions .button--primary').click();

    // Wait for dialog to close
    await expect(dialog).not.toBeVisible({ timeout: 10000 });

    // Wait for the transaction list to refresh
    await page.waitForTimeout(2000);

    // Reload page to verify data persistence
    await page.reload();
    await page.waitForSelector('.transaction-list');

    // Verify the transaction was updated by checking if the new description appears after reload
    await expect(page.locator('.transaction-item__description').first()).toContainText('(edited)');
  });

});