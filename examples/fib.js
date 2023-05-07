const ti_println = console.log;
(() => {
  function __ti_Map() {
  }
  function ti_Map() {
    return new __ti_Map();
  }
  let ti_m = ti_Map();
  ti_m["xy"] = 123;
  ti_m["yx"] = 321;
  ti_println(ti_m);
  })()