# Prog is an ADT with the following values:
# Pure(a) => (0, a)
# Raise(e) => (1, e)
# FlatMap(p, f) => (2, p, f)
# Recover(p, f) => (3, p, f)
# ApplyFix(a, f) => (4, a, f)
# ReadEnv() => (5, )
# RemapEnv(f, p) => (6, f, p)

import sys

def pure(a): return (0, a)
def raise_error(e): return (1, e)
def flat_map(p, f): return (2, p, f)
def recover(p, f): return (3, p, f)
def apply_fix(a, f): return (4, a, f)
read_env = (5,)
def remap_env(p, f): return (6, p, f) 
# this is a thunk we run
def effect(f): return (7, f)

def println(s): return effect(lambda: print(s))

def py_to_bosatsu_list(pylist):
  result = (0,)
  l = len(pylist)
  for idx in range(l):
    result = (1, pylist[l - idx - 1], result)
  return result

_args = None
def do_get_args():
  global _args
  if _args is None:
    _args = py_to_bosatsu_list(sys.argv[1:])

  return _args

get_args = effect(do_get_args)

def step_fix(p):
  arg = p[1]
  fixfn = p[2]
  # this is just apply_fix(a, fixfn)
  fixed = lambda a: (4, a, fixfn)
  return fixfn(fixed)(arg)

# p: Prog[Unit, String, Int]
def run(arg):
  # the stack ADT:
  done = (0,)
  def fmstep(fn, stack): return (1, fn, stack)
  def recstep(fn, stack): return (2, fn, stack)
  def restore(env, stack): return (3, env, stack)

  env = ()
  stack = done
  while True:
    prog_tag = arg[0]
    if prog_tag == 2:
      # flat_map (first because it is most common)
      stack = fmstep(arg[2], stack)
      arg = arg[1]
    elif prog_tag == 0:
      # pure
      item = arg[1]
      stack_tag = stack[0]
      if stack_tag == 0:
        #done, the result must be an int
        sys.exit(item)
      elif stack_tag == 1:
        #fmstep
        fn = stack[1]
        arg = fn(item)
        stack = stack[2]
      elif stack_tag == 2:
        # recstep, but this isn't an error
        stack = stack[2]
      elif stack_tag == 3:
        # restore
        env = stack[1]
        stack = stack[2]
    elif prog_tag == 7:
      # effect
      arg = pure(arg[1]())
    elif prog_tag == 1:
      # raise
      err = arg[1]
      stack_tag = stack[0]
      if stack_tag == 0:
        #done, the top error must be a string
        raise Exception(err) 
      elif stack_tag == 1:
        #fmstep, but this is an error, just pop
        stack = stack[2]
      elif stack_tag == 2:
        # recstep we can recover
        recfn = stack[1]
        arg = recfn(err)
        stack = stack[2]
      elif stack_tag == 3:
        # restore
        env = stack[1]
        stack = stack[2]
    elif prog_tag == 3:
      # recover
      stack = recstep(arg[2], stack)
      arg = arg[1]
    elif prog_tag == 4:
      # apply_fix
      arg = step_fix(arg)
    elif prog_tag == 5:
      # read_env
      arg = pure(env)
    elif prog_tag == 6:
      # remap_env
      stack = restore(env, stack)
      env = arg[2](env)
      arg = arg[1]