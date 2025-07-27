export const setElementStyle = (property) => (value) => (element) => () => {
  element.style[property] = value;
};

export const addClassName = (className) => (element) => () => {
  element.classList.add(className);
};

export const removeClassName = (className) => (element) => () => {
  element.classList.remove(className);
};

export const getClientX = (event) => {
  return event.clientX || 0;
};

export const getClientY = (event) => {
  return event.clientY || 0;
};

export const setElementTextContent = (element) => (text) => () => {
  element.textContent = text;
};