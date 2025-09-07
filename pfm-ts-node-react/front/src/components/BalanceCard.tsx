import React from 'react';
import type { AccountBalanceRead } from '@shared/types';
import styles from './BalanceCard.module.css';

type BalanceCardProps = {
  balance: AccountBalanceRead;
};

export const BalanceCard: React.FC<BalanceCardProps> = ({ balance }) => {
  const formatAmount = (cents: number): string => {
    const intPart = Math.floor(Math.abs(cents) / 100);
    const decPart = Math.abs(cents) % 100;
    const decStr = decPart < 10 ? `0${decPart}` : decPart.toString();
    const sign = cents < 0 ? '-' : '';
    return `${sign}${intPart}.${decStr} €`;
  };

  const getColorAccent = (categoryName: string): string => {
    if (categoryName === 'Assets') {
      return '#3498db';
    } else if (categoryName === 'Expenses') {
      return '#e74c3c';
    } else {
      return '#9b59b6';
    }
  };

  return (
    <div
      className={styles.card}
      style={{ borderLeftColor: getColorAccent(balance.categoryName) }}
    >
      <div className={styles.category}>
        {balance.categoryName}
      </div>
      <div className={styles.account}>
        {balance.accountName}
      </div>
      <div className={styles.amount}>
        {formatAmount(balance.accountBalance)}
      </div>
    </div>
  );
};