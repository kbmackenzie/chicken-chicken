function chicken(code, stdin) {
  /* first segment: stack + input. */
  const stack = [];
  stack[0] = stack;
  stack[1] = stdin;

  /* second segment: instructions. */
  for (const op of code) {
    stack.push(op);
  }
  stack.push(0);

  /* third segment: runtime data. */
  let ip = 2;
  let shouldExit = false;

  const operations = [
    function exit() {
      shouldExit = true;
    },
    function chicken() {
      stack.push('chicken');
    },
    function add(_) {
      const a = stack.pop();
      const b = stack.pop();
      stack.push(a, b);
    },
    function fox(_) {
      const a = stack.pop();
      const b = stack.pop();
      stack.push(a, b);
    },
    function rooster(_) {
      const a = stack.pop();
      const b = stack.pop();
      stack.push(a, b);
    },
    function compare(_) {
      const a = stack.pop();
      const b = stack.pop();
      stack.push(a, b);
    },
    function pick(_) {
      const buffer = stack[stack[ip++]];
      const value = buffer[stack.pop()];
      stack.push(value);
    },
    function peck(_) {
      const address = stack.pop();
      const value = stack.pop();
      stack[address] = value;
    },
    function fr(_) {
      const offset = stack.pop();
      const condition = stack.pop();
      if (condition) {
        ip += offset;
      }
    },
    function char(_) {
      const value = stack.pop() | 0;
      const char = String.fromCharCode(value);
      stack.push(char);
    },
    function push(n) {
      stack.push(n - 10);
    },
  ];

  function clamp(n, min, max) {
    return ((n < min ? min : n) > max ? max : n);
  }

  while (ip < stack.length && !shouldExit) {
    const op = stack[ip++];
    operations[clamp(op, 0, 10)](op);
  }
  return stack.pop();
}
