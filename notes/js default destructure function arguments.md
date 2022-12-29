
```javascript
f({ a = {}, b = false } = {}) {
  console.log(a, b);
}

f(); // {} false
f({ a: 5 }); // 5 false
f({ b: 10 }); // {} 10
```

```javascript
const { c: { a, b } } = { c: { a: 1, b: 2 } };
```
