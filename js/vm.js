function chicken(code, input, compat) {
  /* first segment: stack + input. */
  const stack = [];
  stack[0] = stack;
  stack[1] = input || '';

  /* second segment: instructions. */
  stack.push(...code);
  stack.push(0);

  /* third segment: runtime data. */
  let ip = 2;
  let shouldExit = false;

  const operations = [
    function axe() {
      shouldExit = true;
    },
    function chicken() {
      stack.push('chicken');
    },
    function add() {
      const a = stack.pop();
      const b = stack.pop();
      stack.push(b + a);
    },
    function fox() {
      const a = stack.pop();
      const b = stack.pop();
      stack.push(b - a);
    },
    function rooster() {
      const a = stack.pop();
      const b = stack.pop();
      stack.push(b * a);
    },
    function compare() {
      const a = stack.pop();
      const b = stack.pop();
      stack.push(a == b);
    },
    function pick() {
      const buffer = stack[stack[ip++]];
      const value = buffer[stack.pop()];
      stack.push(value);
    },
    function peck() {
      const address = stack.pop();
      const value = stack.pop();
      stack[address] = value;
    },
    function fr() {
      const offset = stack.pop();
      const condition = stack.pop();
      if (condition) {
        ip += offset;
      }
    },
    function char() { /* aka: BBQ */
      const value = stack.pop() | 0;
      const char  = (compat) ? `&#${value};` : String.fromCharCode(value);
      stack.push(char);
    },
    function push(n) {
      stack.push(n - 10);
    },
  ];

  function clamp(n, min, max) {
    return n < min ? min : (n > max ? max : n);
  }

  while (ip < stack.length && !shouldExit) {
    const op = stack[ip++];
    operations[clamp(op, 0, 10)](op);
  }
  return stack.pop();
}
