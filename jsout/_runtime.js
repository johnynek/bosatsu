// Bosatsu JS Runtime
// Note: Using 'var' for all declarations to create true globals accessible from generated code
// GCD using Euclidean algorithm
var _gcd = (a, b) => {
  a = Math.abs(a);
  b = Math.abs(b);
  while (b !== 0) {
    const t = b;
    b = a % b;
    a = t;
  }
  return a;
};

// int_loop(i, state, fn) - countdown loop with accumulator
// fn(i, state) returns [newI, newState]
// continues while newI > 0 AND newI < i (ensures progress)
var _int_loop = (i, state, fn) => {
  let _i = i;
  let _state = state;
  while (_i > 0) {
    const result = fn(_i, _state);
    const newI = result[0];
    const newState = result[1];
    // Update state regardless
    _state = newState;
    // Check if we should continue
    if (newI <= 0 || newI >= _i) {
      // Reached 0 or no progress, return new state
      return _state;
    }
    _i = newI;
  }
  return _state;
};

// Convert Bosatsu string list to JS string
var _bosatsu_to_js_string = (bstr) => {
  let result = '';
  let current = bstr;
  while (current[0] === 1) {
    result += current[1];
    current = current[2];
  }
  return result;
};

// Convert JS string to Bosatsu string list
var _js_to_bosatsu_string = (str) => {
  let result = [0]; // Empty list
  for (let i = str.length - 1; i >= 0; i--) {
    result = [1, str[i], result];
  }
  return result;
};

// concat_String - takes a Bosatsu list of strings and concatenates
var _concat_String = (strList) => {
  let result = '';
  let current = strList;
  while (current[0] === 1) {
    result += _bosatsu_to_js_string(current[1]);
    current = current[2];
  }
  return _js_to_bosatsu_string(result);
};

// int_to_String
var _int_to_String = (n) => _js_to_bosatsu_string(String(n));

// string_to_Int - returns Option: [0] for None, [1, value] for Some
var _string_to_Int = (bstr) => {
  const str = _bosatsu_to_js_string(bstr);
  const n = parseInt(str, 10);
  return isNaN(n) ? [0] : [1, n];
};

// char_to_String - char is already a single-char string
var _char_to_String = (c) => [1, c, [0]];

// trace - log message and return value
var _trace = (msg, value) => {
  console.log(_bosatsu_to_js_string(msg));
  return value;
};

// cmp_String - compare two Bosatsu strings, return 0 (LT), 1 (EQ), or 2 (GT)
var _cmp_String = (a, b) => {
  const sa = _bosatsu_to_js_string(a);
  const sb = _bosatsu_to_js_string(b);
  return sa < sb ? 0 : (sa === sb ? 1 : 2);
};

// partition_String - split string on first occurrence of separator
// Returns tuple of (before, sep, after) or (original, empty, empty) if not found
// partition_String - returns Option[(String, String)]
// None if sep is empty or not found, Some((before, after)) otherwise
var _partition_String = (str, sep) => {
  const s = _bosatsu_to_js_string(str);
  const sp = _bosatsu_to_js_string(sep);
  // Empty separator returns None
  if (sp.length === 0) return [0];
  const idx = s.indexOf(sp);
  if (idx === -1) return [0]; // Not found: None
  // Found: Some((before, after))
  return [1, [
    _js_to_bosatsu_string(s.substring(0, idx)),
    _js_to_bosatsu_string(s.substring(idx + sp.length))
  ]];
};

// rpartition_String - returns Option[(String, String)]
// None if sep is empty or not found, Some((before, after)) otherwise
var _rpartition_String = (str, sep) => {
  const s = _bosatsu_to_js_string(str);
  const sp = _bosatsu_to_js_string(sep);
  // Empty separator returns None
  if (sp.length === 0) return [0];
  const idx = s.lastIndexOf(sp);
  if (idx === -1) return [0]; // Not found: None
  // Found: Some((before, after))
  return [1, [
    _js_to_bosatsu_string(s.substring(0, idx)),
    _js_to_bosatsu_string(s.substring(idx + sp.length))
  ]];
};

// range(n) - generate list [0, 1, 2, ..., n-1]
var range = (n) => {
  let result = [0];
  for (let i = n - 1; i >= 0; i--) {
    result = [1, i, result];
  }
  return result;
};

// foldl_List(list, init, fn) - left fold over a list
var foldl_List = (list, init, fn) => {
  let acc = init;
  let current = list;
  while (current[0] === 1) {
    acc = fn(acc, current[1]);
    current = current[2];
  }
  return acc;
};

// flat_map_List(list, fn) - flatMap over a list
var flat_map_List = (list, fn) => {
  let result = [0];
  let current = list;
  // First collect all results in reverse order
  let reversed = [0];
  while (current[0] === 1) {
    const mapped = fn(current[1]);
    // Prepend mapped items to reversed
    let m = mapped;
    while (m[0] === 1) {
      reversed = [1, m[1], reversed];
      m = m[2];
    }
    current = current[2];
  }
  // Now reverse to get correct order
  current = reversed;
  while (current[0] === 1) {
    result = [1, current[1], result];
    current = current[2];
  }
  return result;
};

// Bosatsu/Prog external functions
// Prog is represented as a thunk that takes an environment and returns [result_type, value_or_error]
// result_type: 0 = success, 1 = error
var Bosatsu_Prog$pure = a => _env => [0, a];
var Bosatsu_Prog$raise_error = e => _env => [1, e];
var Bosatsu_Prog$read_env = env => [0, env];
var Bosatsu_Prog$flat_map = (prog, fn) => env => {
  const result = prog(env);
  if (result[0] === 0) {
    return fn(result[1])(env);
  }
  return result; // propagate error
};
var Bosatsu_Prog$recover = (prog, fn) => env => {
  const result = prog(env);
  if (result[0] === 1) {
    return fn(result[1])(env);
  }
  return result;
};
var Bosatsu_Prog$apply_fix = (a, fn) => {
  const fixed = x => fn(fixed)(x);
  return fixed(a);
};
var Bosatsu_Prog$remap_env = (p, f) => env => p(f(env));
var Bosatsu_Prog$println = str => _env => { console.log(_bosatsu_to_js_string(str)); return [0, []]; };
var Bosatsu_Prog$print = str => _env => { process.stdout.write(_bosatsu_to_js_string(str)); return [0, []]; };
var Bosatsu_Prog$read_stdin_utf8_bytes = n => _env => [0, [0]]; // Return empty string for now
