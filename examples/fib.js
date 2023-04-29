const println = console.log;
(() => {
  function fib(x) {
    return (() => { if (x > 2) { return (() => {
      return fib(x - 1) + fib(x - 2);
    })(); } else { return (() => {
      return 1;
    })(); }})();
  };
  return println(fib(10));
})()