// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const map = (prog, fn) => Bosatsu_Prog$flat_map(prog, (_slots => res => Bosatsu_Prog$pure(_slots[0](res)))([fn]));
const map_err = (prog, fn) => Bosatsu_Prog$recover(prog, (_slots => res => Bosatsu_Prog$raise_error(_slots[0](res)))([fn]));
const with_env = (p, env) => Bosatsu_Prog$remap_env(p, (_slots => a => _slots[0])([env]));
const ignore_env = p => Bosatsu_Prog$remap_env(p, a => []);
const _await = p => (_slots => fn => Bosatsu_Prog$flat_map(_slots[0], fn))([p]);
const recursive = fn => (_slots => a => Bosatsu_Prog$apply_fix(a, _slots[0]))([fn]);
const unit = Bosatsu_Prog$pure([]);
const count_down = a => Bosatsu_Prog$apply_fix(a, loop => (_slots => i => (() => {
    const _anon0 = (i < 0) ? [0] : (i === 0) ? [1] : [2];
    return (_anon0[0] === 1) ? Bosatsu_Prog$println(_js_to_bosatsu_string("\ndone")) : (_anon0[0] === 0) ? Bosatsu_Prog$println(_js_to_bosatsu_string("\ndone")) : Bosatsu_Prog$flat_map(Bosatsu_Prog$print(_concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(_int_to_String(i), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(", "), [0])))), (_slots => a_1 => _slots[0]((-1) + _slots[1]))([_slots[0],
              i]));
  })())([loop]));
const to_run = (_a0 => [_a0])(Bosatsu_Prog$flat_map(Bosatsu_Prog$read_env, args => Bosatsu_Prog$ignore_env((() => {
        const arg_str = foldl_List(args, _js_to_bosatsu_string(""), (s, item) => (_bosatsu_to_js_string(s) === "") ? item : _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(s, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(", "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(item, [0])))));
        return Bosatsu_Prog$flat_map(Bosatsu_Prog$recover(Bosatsu_Prog$read_stdin_utf8_bytes(1000), (() => {
              const prog = Bosatsu_Prog$println(_js_to_bosatsu_string("<failed to read stdin"));
              return (_slots => a => Bosatsu_Prog$flat_map(_slots[0], a_1 => Bosatsu_Prog$pure(_js_to_bosatsu_string(""))))([prog]);
            })()), (_slots => stdin => Bosatsu_Prog$flat_map(Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("found stdin: "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(stdin, [0])))), (_slots => a_2 => Bosatsu_Prog$flat_map(Bosatsu_Prog$count_down(10), (_slots => a_3 => Bosatsu_Prog$flat_map(Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string("args = "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(_slots[0], [0])))), a_4 => Bosatsu_Prog$pure(0)))([_slots[0]])))([_slots[0]])))([arg_str]));
      })())));
export {map};