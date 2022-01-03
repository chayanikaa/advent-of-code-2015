// JS is built for this problem
// Thinking about solving something like this with Haskell is a bit...

const fs = require('fs');

const input = fs.readFileSync('../inputs/12.txt');
const parsedObj = JSON.parse(input);

let sum = 0;

function addNums1(obj) {
  if (typeof obj === 'number') {
    sum = sum+obj;
  }
  if (obj == null) { // null or undefined
    return;
  }
  if (Array.isArray(obj)) {
    return obj.forEach(addNums1);
  }
  if (typeof obj === 'object') {
    return Object.values(obj).forEach(addNums1);
  }
}

addNums1(parsedObj);
console.log('1', sum);

let sum2 = 0;

function addNums2(obj) {
  if (typeof obj === 'number') {
    sum2 = sum2+obj;
  }
  if (obj == null) { // null or undefined
    return;
  }
  if (Array.isArray(obj)) {
    return obj.forEach(addNums2);
  }
  if (typeof obj === 'object') {
    const values = Object.values(obj);
    if (values.find(val => val === "red")) return;
    return values.forEach(addNums2);
  }
}

addNums2(parsedObj);
console.log('2', sum2);