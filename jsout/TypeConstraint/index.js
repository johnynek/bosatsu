// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const refl = (_a0 => [_a0])(x => x);
const refl_sub = (_a0 => [_a0])(x => x);
const refl_sup = (_a0 => [_a0])(x => x);
const substitute = (eq, fa) => (() => {
  const cast = eq[0];
  return cast(fa);
})();
const widen = (s, fa) => (() => {
  const sub = s[0];
  return sub(fa);
})();
const narrow = (s, fa) => (() => {
  const sup = s[0];
  return sup(fa);
})();
const sub_to_sup = sub => (() => {
  const _anon0 = (() => {
    const sub0 = sub[0];
    return sub0((_a0 => [_a0])(TypeConstraint$refl_sup));
  })();
  return _anon0[0];
})();
const sup_to_sub = sup => (() => {
  const _anon1 = (() => {
    const sup0 = sup[0];
    return sup0((_a0 => [_a0])(TypeConstraint$refl_sub));
  })();
  return _anon1[0];
})();
const eq_to_sub = eq => (() => {
  const cast = eq[0];
  return cast(TypeConstraint$refl_sub);
})();
const eq_to_sup = eq => (() => {
  const cast = eq[0];
  return cast(TypeConstraint$refl_sup);
})();
const cast = (s, a) => (() => {
  const _anon2 = (() => {
    const cast = s[0];
    return cast((_a0 => [_a0])(a));
  })();
  return _anon2[0];
})();
const upcast = (s, a) => (() => {
  const _anon3 = (() => {
    const sub = s[0];
    return sub((_a0 => [_a0])(a));
  })();
  return _anon3[0];
})();
const downcast = (s, a) => (() => {
  const _anon6 = (() => {
    const _anon5 = (() => {
      const _anon4 = (() => {
        const sup0 = s[0];
        return sup0((_a0 => [_a0])(TypeConstraint$refl_sub));
      })();
      return _anon4[0];
    })();
    return (() => {
      const sub = _anon5[0];
      return sub((_a0 => [_a0])(a));
    })();
  })();
  return _anon6[0];
})();
const compose_sub = (first, second) => (() => {
  const sub = second[0];
  return sub(first);
})();
const compose_sup = (first, second) => (() => {
  const sup = second[0];
  return sup(first);
})();
const flip_eq = eq => (() => {
  const _anon7 = (() => {
    const cast = eq[0];
    return cast((_a0 => [_a0])(TypeConstraint$refl));
  })();
  return _anon7[0];
})();
const compose_eq = (first, second) => (() => {
  const cast = second[0];
  return cast(first);
})();
const ignore = ((_a0, _a1, _a2, _a3, _a4, _a5) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4,
  _a5])(TypeConstraint$refl_sub, TypeConstraint$refl_sub, TypeConstraint$refl_sub, TypeConstraint$refl_sub, TypeConstraint$refl_sub, TypeConstraint$refl_sub);
export {refl};