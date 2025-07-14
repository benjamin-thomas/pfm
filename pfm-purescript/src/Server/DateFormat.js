export const formatUnixTimestamp = (unixTimestamp) => {
  const date = new Date(unixTimestamp * 1000);
  return date.toISOString().split('T')[0]; // Returns YYYY-MM-DD format
};