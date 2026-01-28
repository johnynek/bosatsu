// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const ack1 = n => (n === 0) ? n => n + 1 : (() => {
    let _anon1;
    return (() => {
      (() => {
        _anon1 = n - 1;
        return true;
      })();
      return (() => {
        const n_prev = _anon1;
        return (() => {
          const ack_p = ack1(n_prev);
          return (_slots => m => _slots[0]((m === 0) ? (n => n + 1)(0) : (() => {
                let _anon0;
                return (() => {
                  (() => {
                    _anon0 = m - 1;
                    return true;
                  })();
                  return (() => {
                    const m_prev = _anon0;
                    return inner(m_prev);
                  })();
                })();
              })()))([ack_p]);
        })();
      })();
    })();
  })();
const ack = (n, m) => Ackermann$ack1(n)(m);
export {ack1};