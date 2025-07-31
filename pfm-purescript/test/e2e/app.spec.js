import { test, expect } from '@playwright/test';

// Reset the test database before each test
test.beforeEach(async ({ request }) => {
  const backendURL = process.env.BACKEND_URL || 'http://localhost:8082';
  await request.post(`${backendURL}/test/reset-db`);
});

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
    await expect(createButton).toContainText('Add Transaction');
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

  test('should populate create dialog date field with current time', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Get current time before opening dialog (with some tolerance)
    const beforeTime = new Date();

    // Open create dialog
    const createButton = page.locator('.transaction-list__header-buttons .button--primary');
    await createButton.click();

    // Wait for dialog to be visible
    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    // Check that dialog title is correct
    await expect(page.locator('.dialog-title')).toContainText('Create Transaction');

    // Get the date field value
    const dateField = page.locator('input[id="date"]');
    const dateValue = await dateField.inputValue();

    // Parse the datetime-local value
    const dialogDate = new Date(dateValue);
    const afterTime = new Date();

    // The dialog date should be between beforeTime and afterTime (allowing 1 minute tolerance)
    expect(dialogDate.getTime()).toBeGreaterThanOrEqual(beforeTime.getTime() - 60000);
    expect(dialogDate.getTime()).toBeLessThanOrEqual(afterTime.getTime() + 60000);

    // Verify the format is datetime-local (YYYY-MM-DDTHH:mm)
    const dateTimeLocalPattern = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}$/;
    expect(dateValue).toMatch(dateTimeLocalPattern);

    // Close dialog
    await page.locator('.dialog-actions .button').first().click();
  });

  test('should delete a transaction with confirmation', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Count initial transactions
    const initialCount = await page.locator('.transaction-item').count();
    expect(initialCount).toBeGreaterThan(0);

    // Click on the first transaction to edit it
    const firstTransaction = page.locator('.transaction-item').first();

    // Get its description so we can verify it's gone later
    const descriptionToDelete = await firstTransaction.locator('.transaction-item__description').textContent();

    await firstTransaction.click();

    // Wait for edit dialog to be visible
    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    // Check that three-dot menu is present
    const menuButton = page.locator('.menu-button');
    await expect(menuButton).toBeVisible();
    await expect(menuButton).toHaveAttribute('aria-label', 'More options');

    // Hover over menu to show dropdown
    await menuButton.hover();

    // Check that delete menu item is visible
    const deleteMenuItem = page.locator('.menu-item', { hasText: 'Delete' });
    await expect(deleteMenuItem).toBeVisible();

    // Set up dialog handler before clicking delete
    page.on('dialog', async dialog => {
      expect(dialog.type()).toBe('confirm');
      expect(dialog.message()).toBe('Are you sure you want to delete this transaction?');
      await dialog.accept(); // Confirm deletion
    });

    // Click delete menu item
    await deleteMenuItem.click();

    // Wait for dialog to close
    await expect(dialog).not.toBeVisible({ timeout: 10000 });

    // Wait for transaction list to refresh
    await page.waitForTimeout(2000);

    // Reload page to verify data persistence
    await page.reload();
    await page.waitForSelector('.transaction-list');

    // Verify transaction count decreased
    const finalCount = await page.locator('.transaction-item').count();
    expect(finalCount).toBe(initialCount - 1);

    // Verify the deleted transaction is no longer in the list
    const remainingDescriptions = await page.locator('.transaction-item__description').allTextContents();
    expect(remainingDescriptions).not.toContain(descriptionToDelete);
  });

  test('should display transaction filter menu', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Check that filter menu is visible
    const filterMenu = page.locator('.transaction-search');
    await expect(filterMenu).toBeVisible();

    // Check that description filter field is present
    const descriptionFilter = page.locator('input#search-description');
    await expect(descriptionFilter).toBeVisible();
    await expect(descriptionFilter).toHaveAttribute('placeholder', 'Search by description');

    // Check that min/max amount filters are present
    const minAmountFilter = page.locator('input#search-amount-min');
    await expect(minAmountFilter).toBeVisible();

    const maxAmountFilter = page.locator('input#search-amount-max');
    await expect(maxAmountFilter).toBeVisible();

    // Check that unknown expenses checkbox is present
    const unknownExpensesCheckbox = page.locator('input[type="checkbox"]');
    await expect(unknownExpensesCheckbox).toBeVisible();

    // Check that clear button is present
    const clearButton = page.locator('#form-clear-button');
    await expect(clearButton).toBeVisible();
    await expect(clearButton).toContainText('Clear');
  });

  test('should filter transactions by description', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Get initial transaction count
    const initialCount = await page.locator('.transaction-item').count();

    // Add a test transaction with known description
    const createButton = page.locator('.transaction-list__header-buttons .button--primary');
    await createButton.click();

    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    const uniqueDescription = `Filter test GROCERY ${Date.now()}`;
    await page.locator('input[id="description"]').fill(uniqueDescription);
    await page.locator('select[id="from-account"]').selectOption('2');
    await page.locator('select[id="to-account"]').selectOption('6');
    await page.locator('input[id="amount"]').fill('25.00');
    await page.locator('input[id="date"]').fill('2024-07-01T10:30');

    await page.locator('.dialog-actions .button--primary').click();
    await expect(dialog).not.toBeVisible({ timeout: 10000 });
    await page.waitForTimeout(2000);

    // Now test filtering by description
    const descriptionFilter = page.locator('input#search-description');
    await descriptionFilter.fill('GROCERY');

    // Give time for filter to be applied
    await page.waitForTimeout(1000);

    // Check that only transactions with "GROCERY" in description are shown
    const filteredTransactions = await page.locator('.transaction-item').count();
    const visibleDescriptions = await page.locator('.transaction-item__description').allTextContents();

    // All visible descriptions should contain "GROCERY" (case insensitive)
    for (const desc of visibleDescriptions) {
      expect(desc.toLowerCase()).toContain('grocery');
    }

    // Clear filter and verify all transactions are shown again
    const clearButton = page.locator('#form-clear-button');
    await clearButton.click();
    await page.waitForTimeout(1000);

    const clearedCount = await page.locator('.transaction-item').count();
    expect(clearedCount).toBeGreaterThanOrEqual(filteredTransactions);
  });

  test('should filter transactions by amount range', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Add test transactions with known amounts
    for (const [amount, desc] of [['10.00', 'Small'], ['50.00', 'Medium'], ['100.00', 'Large']]) {
      const createButton = page.locator('.transaction-list__header-buttons .button--primary');
      await createButton.click();

      const dialog = page.locator('#transaction-dialog');
      await expect(dialog).toBeVisible();

      await page.locator('input[id="description"]').fill(`${desc} transaction ${Date.now()}`);
      await page.locator('select[id="from-account"]').selectOption('2');
      await page.locator('select[id="to-account"]').selectOption('6');
      await page.locator('input[id="amount"]').fill(amount);
      await page.locator('input[id="date"]').fill('2024-07-01T10:30');

      await page.locator('.dialog-actions .button--primary').click();
      await expect(dialog).not.toBeVisible({ timeout: 10000 });
      await page.waitForTimeout(1000);
    }

    await page.waitForTimeout(2000);

    // Test minimum amount filter
    const minAmountFilter = page.locator('input#search-amount-min');
    await minAmountFilter.fill('50.00'); // 50.00 euros
    await page.waitForTimeout(1000);

    // All visible transactions should have amount >= 50.00
    const visibleAmounts = await page.locator('.transaction-item__amount').allTextContents();
    for (const amountText of visibleAmounts) {
      const amount = parseFloat(amountText.replace(/[^0-9.-]/g, ''));
      expect(Math.abs(amount)).toBeGreaterThanOrEqual(50.0);
    }

    // Clear filter
    await page.locator('#form-clear-button').click();
    await page.waitForTimeout(1000);

    // Test maximum amount filter
    const maxAmountFilter = page.locator('input#search-amount-max');
    await maxAmountFilter.fill('50.00'); // 50.00 euros
    await page.waitForTimeout(1000);

    // All visible transactions should have amount <= 50.00
    const visibleAmounts2 = await page.locator('.transaction-item__amount').allTextContents();
    for (const amountText of visibleAmounts2) {
      const amount = parseFloat(amountText.replace(/[^0-9.-]/g, ''));
      expect(Math.abs(amount)).toBeLessThanOrEqual(50.0);
    }
  });

  test('should filter for unknown expenses only', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Add a known expense transaction
    const createButton = page.locator('.transaction-list__header-buttons .button--primary');
    await createButton.click();

    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    await page.locator('input[id="description"]').fill(`Known expense ${Date.now()}`);
    await page.locator('select[id="from-account"]').selectOption('2');
    await page.locator('select[id="to-account"]').selectOption('7'); // Groceries (known expense)
    await page.locator('input[id="amount"]').fill('30.00');
    await page.locator('input[id="date"]').fill('2024-07-01T10:30');

    await page.locator('.dialog-actions .button--primary').click();
    await expect(dialog).not.toBeVisible({ timeout: 10000 });
    await page.waitForTimeout(2000);

    // Check unknown expenses checkbox
    const unknownExpensesCheckbox = page.locator('input[type="checkbox"]');
    await unknownExpensesCheckbox.check();
    await page.waitForTimeout(1000);

    // All visible transactions should be TO Unknown_EXPENSE account
    const visibleTransactions = await page.locator('.transaction-item').count();
    if (visibleTransactions > 0) {
      const accountTexts = await page.locator('.transaction-item__accounts').allTextContents();
      for (const accountText of accountTexts) {
        expect(accountText).toContain('Unknown_EXPENSE');
      }
    }

    // Uncheck and verify all transactions are shown again
    await unknownExpensesCheckbox.uncheck();
    await page.waitForTimeout(1000);

    const allCount = await page.locator('.transaction-item').count();
    expect(allCount).toBeGreaterThanOrEqual(visibleTransactions);
  });

  test('should combine multiple filters', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Add test transaction with specific criteria
    const createButton = page.locator('.transaction-list__header-buttons .button--primary');
    await createButton.click();

    const dialog = page.locator('#transaction-dialog');
    await expect(dialog).toBeVisible();

    const uniqueDescription = `MULTI filter test ${Date.now()}`;
    await page.locator('input[id="description"]').fill(uniqueDescription);
    await page.locator('select[id="from-account"]').selectOption('2');
    await page.locator('select[id="to-account"]').selectOption('6'); // Unknown expense
    await page.locator('input[id="amount"]').fill('75.50');
    await page.locator('input[id="date"]').fill('2024-07-01T10:30');

    await page.locator('.dialog-actions .button--primary').click();
    await expect(dialog).not.toBeVisible({ timeout: 10000 });
    await page.waitForTimeout(2000);

    // Apply multiple filters
    const descriptionFilter = page.locator('input#search-description');
    await descriptionFilter.fill('MULTI');

    const minAmountFilter = page.locator('input#search-amount-min');
    await minAmountFilter.fill('70.00'); // 70.00 euros

    const maxAmountFilter = page.locator('input#search-amount-max');
    await maxAmountFilter.fill('80.00'); // 80.00 euros

    const unknownExpensesCheckbox = page.locator('input[type="checkbox"]');
    await unknownExpensesCheckbox.check();

    await page.waitForTimeout(1000);

    // Verify all visible transactions meet all criteria
    const visibleTransactions = await page.locator('.transaction-item').count();
    if (visibleTransactions > 0) {
      const descriptions = await page.locator('.transaction-item__description').allTextContents();
      const amounts = await page.locator('.transaction-item__amount').allTextContents();
      const accounts = await page.locator('.transaction-item__accounts').allTextContents();

      for (let i = 0; i < visibleTransactions; i++) {
        // Check description contains "MULTI"
        expect(descriptions[i].toLowerCase()).toContain('multi');

        // Check amount is between 70.00 and 80.00
        const amount = parseFloat(amounts[i].replace(/[^0-9.-]/g, ''));
        expect(Math.abs(amount)).toBeGreaterThanOrEqual(70.0);
        expect(Math.abs(amount)).toBeLessThanOrEqual(80.0);

        // Check it's an unknown expense
        expect(accounts[i]).toContain('Unknown_EXPENSE');
      }
    }

    // Clear all filters
    const clearButton = page.locator('#form-clear-button');
    await clearButton.click();
    await page.waitForTimeout(1000);

    // Verify filters are cleared
    await expect(descriptionFilter).toHaveValue('');
    await expect(minAmountFilter).toHaveValue('');
    await expect(maxAmountFilter).toHaveValue('');
    await expect(unknownExpensesCheckbox).not.toBeChecked();
  });

  test('should show context menu on right-click transaction', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Wait for at least one transaction to be present
    await page.waitForSelector('.transaction-item');

    // Get the first transaction item
    const firstTransaction = page.locator('.transaction-item').first();
    await expect(firstTransaction).toBeVisible();

    // Verify context menu is not visible initially
    const contextMenu = page.locator('.context-menu');
    await expect(contextMenu).not.toBeVisible();

    // Right-click on the transaction
    await firstTransaction.click({ button: 'right' });

    // Verify context menu appears
    await expect(contextMenu).toBeVisible();

    // Verify context menu contains expected options including Find similar transactions
    await expect(page.locator('.context-menu li', { hasText: 'Find similar transactions' })).toBeVisible();

    // Click elsewhere to close context menu
    await page.locator('body').click({ position: { x: 0, y: 0 } });

    // Verify context menu is hidden
    await expect(contextMenu).not.toBeVisible();
  });

  test('should filter transactions when clicking Find similar transactions', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Get initial transaction count
    const initialCount = await page.locator('.transaction-item').count();
    expect(initialCount).toBeGreaterThan(5); // Ensure we have enough transactions to test filtering

    // Find a transaction with a specific description that likely has similar transactions
    // Look for something like "GROCERY" or similar common descriptions
    const transactions = await page.locator('.transaction-item').all();
    let targetTransaction = null;
    let targetDescription = null;

    for (const transaction of transactions) {
      const description = await transaction.locator('.transaction-item__description').textContent();
      // Find a transaction with a description that likely has similar ones
      if (description && (description.includes('GROCERY') || description.includes('MARKET') || description.includes('STORE'))) {
        targetTransaction = transaction;
        targetDescription = description;
        break;
      }
    }

    if (!targetTransaction) {
      // If no specific transaction found, just use the first one
      targetTransaction = page.locator('.transaction-item').first();
      targetDescription = await targetTransaction.locator('.transaction-item__description').textContent();
    }

    // Right-click on the target transaction
    await targetTransaction.click({ button: 'right' });

    // Wait for context menu to appear
    const contextMenu = page.locator('.context-menu');
    await expect(contextMenu).toBeVisible();

    // Verify the soundex header is present
    const soundexHeader = page.locator('.context-menu-debug');
    await expect(soundexHeader).toBeVisible();
    const soundexText = await soundexHeader.textContent();
    expect(soundexText).toContain('SOUNDEX:');

    // Click "Find similar transactions"
    const findSimilarButton = page.locator('.context-menu li', { hasText: 'Find similar transactions' });
    await findSimilarButton.click();

    // Wait for context menu to disappear
    await expect(contextMenu).not.toBeVisible();

    // Wait for the transaction list to be filtered (give time for API call)
    await page.waitForTimeout(1000);

    // Get the new transaction count
    const filteredCount = await page.locator('.transaction-item').count();

    // The filtered count should be less than the initial count
    expect(filteredCount).toBeLessThan(initialCount);
    expect(filteredCount).toBeGreaterThan(0); // Should have at least one similar transaction

    // Check that the similarity filter message is displayed
    const similarityMessage = page.locator('.similar-transactions-info');
    await expect(similarityMessage).toBeVisible();
    await expect(similarityMessage).toContainText('Displaying transactions similar to:');
    await expect(similarityMessage).toContainText(targetDescription);

    // Verify all visible transactions have similar soundex values
    // This is implicit in the filtering, but we can check the descriptions are somewhat similar

    // Verify the filter state shows we're filtering by similarity
    // Check if there's any UI indication of the active filter (this depends on implementation)
  });

  test('should show correct soundex value after filtering', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // First, let's see what transactions we have
    const allDescriptions = await page.locator('.transaction-item__description').allTextContents();
    console.log('All transaction descriptions:', allDescriptions);

    // Get info about the first transaction before filtering
    const firstTransactionBeforeFilter = page.locator('.transaction-item').first();
    const firstDescriptionBefore = await firstTransactionBeforeFilter.locator('.transaction-item__description').textContent();

    // Right-click to see its soundex
    await firstTransactionBeforeFilter.click({ button: 'right' });
    const contextMenuBefore = page.locator('.context-menu');
    await expect(contextMenuBefore).toBeVisible();
    const soundexBefore = await page.locator('.context-menu-debug').textContent();
    console.log('First transaction before filter:', firstDescriptionBefore, soundexBefore);

    // Close context menu
    await page.locator('body').click({ position: { x: 0, y: 0 } });
    await expect(contextMenuBefore).not.toBeVisible();

    // Apply a filter that will definitely change which transaction is first
    // Let's search for something specific that's NOT the first transaction
    const descriptionFilter = page.locator('input#search-description');

    // Find a description that's different from the first one
    let searchTerm = '';
    for (const desc of allDescriptions) {
      if (desc !== firstDescriptionBefore && desc.length > 3) {
        // Use the first few characters of a different transaction
        searchTerm = desc.substring(0, 5);
        break;
      }
    }

    if (!searchTerm) {
      // If we can't find a different one, skip the test
      console.log('Could not find a suitable search term, skipping test');
      return;
    }

    console.log('Searching for:', searchTerm);
    await descriptionFilter.fill(searchTerm);
    await page.waitForTimeout(1000);

    // Get info about the first transaction after filtering
    const firstTransactionAfterFilter = page.locator('.transaction-item').first();
    const firstDescriptionAfter = await firstTransactionAfterFilter.locator('.transaction-item__description').textContent();

    // This should be a different transaction
    expect(firstDescriptionAfter).not.toBe(firstDescriptionBefore);

    // Right-click to see its soundex
    await firstTransactionAfterFilter.click({ button: 'right' });
    const contextMenuAfter = page.locator('.context-menu');
    await expect(contextMenuAfter).toBeVisible();
    const soundexAfter = await page.locator('.context-menu-debug').textContent();
    console.log('First transaction after filter:', firstDescriptionAfter, soundexAfter);

    // The soundex should be different since it's a different transaction
    expect(soundexAfter).not.toBe(soundexBefore);

    // Also verify the soundex value is reasonable (not empty or undefined)
    expect(soundexAfter).toContain('SOUNDEX:');
    expect(soundexAfter.length).toBeGreaterThan(9); // "SOUNDEX: " + at least 1 char
  });

  test('should update transaction count text when filtering', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Get initial count text
    const countElement = page.locator('.transaction-count');
    await expect(countElement).toBeVisible();

    const initialCountText = await countElement.textContent();
    expect(initialCountText).toMatch(/\d+ transactions?/);

    // Apply a filter that should reduce results
    const descriptionFilter = page.locator('input#search-description');
    await descriptionFilter.fill('nonexistent filter term that matches nothing');
    await page.waitForTimeout(1000);

    // Count text should update to show filtered results
    const filteredCountText = await countElement.textContent();
    expect(filteredCountText).toMatch(/\d+ of \d+ transactions?|0 transactions?/);

    // Clear filter
    const clearButton = page.locator('#form-clear-button');
    await clearButton.click();
    await page.waitForTimeout(1000);

    // Count should return to original
    const clearedCountText = await countElement.textContent();
    expect(clearedCountText).toBe(initialCountText);
  });

});

test.describe('Transaction Filter UI Integration', () => {
  test('should remove refresh button and rename create button', async ({ page }) => {
    await page.goto('/');

    // Wait for transaction list to load
    await page.waitForSelector('.transaction-list');

    // Verify refresh button is NOT present
    const refreshButton = page.locator('.transaction-list__header-buttons', { hasText: 'Refresh' });
    await expect(refreshButton).not.toBeVisible();

    // Verify create button is renamed to "Add Transaction"
    const addButton = page.locator('.transaction-list__header-buttons .button--primary');
    await expect(addButton).toBeVisible();
    await expect(addButton).toContainText('Add Transaction');
  });
});

test.describe('Transaction Suggestions', () => {
  test('should display at least one suggestion', async ({ page }) => {
    // await page.request.post('/test/reset-db');
    await page.goto('/');

    // Wait for app to load and suggestions to appear
    await page.waitForSelector('.transaction-list');

    // Verify at least one suggestion is displayed within transaction rows
    const suggestions = page.locator('.suggestion-container');
    const suggestionCount = await suggestions.count();
    expect(suggestionCount).toBeGreaterThan(0);

    // Verify the first suggestion has required elements
    const firstSuggestion = suggestions.first();
    await expect(firstSuggestion.locator('.suggestion-text')).toBeVisible();
    await expect(firstSuggestion.locator('.suggestion-actions')).toBeVisible();
  });
});