def just_bvalue(cnt):
  return ",".join(["BValue"] * cnt)

def bvalue_arg(cnt):
  return ", ".join("BValue arg%i" % i for i in range(cnt))

def fn_decls(size):
  """
  BValue alloc_closure2(size_t size, BValue* data, BClosure2 fn);
  BValue value_from_pure_fn2(BPureFn2 fn);
  BValue call_fn2(BValue fn, BValue arg0, BValue arg1);
  """
  alloc_c = "BValue alloc_closure{size}(size_t size, BValue* data, BClosure{size} fn);".format(size = size)
  from_fn = "BValue value_from_pure_fn{size}(BPureFn{size} fn);".format(size = size)
  call_fn = "BValue call_fn{size}(BValue fn, {args});".format(size = size, args = bvalue_arg(size))
  return "\n".join([alloc_c, from_fn, call_fn])

def struct_impl(size):
  template = """DEFINE_RC_STRUCT(Struct{size},{arg_decls});

void free_struct{size}(Struct{size}* s) {{
    {releases}
    free(s);
}}
BValue alloc_struct{size}({arg_params}) {{
    Struct{size}* rc = malloc(sizeof(Struct{size}));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_struct{size};
    {assigns}
    return (BValue)rc;
}}"""
  arg_decls = "".join("BValue _{i};".format(i = i) for i in range(size))
  arg_params = ", ".join("BValue b{i}".format(i = i) for i in range(size))
  releases = "\n    ".join("release_value(s->_{i});".format(i = i) for i in range(size))
  assigns = "\n    ".join("rc->_{i} = b{i};".format(i = i) for i in range(size))
  return template.format(size = size, arg_decls = arg_decls, arg_params = arg_params, releases = releases, assigns = assigns)

def enum_impl(size):
  template = """DEFINE_RC_ENUM(Enum{size},{arg_decls});

void free_enum{size}(Enum{size}* s) {{
    {releases}
    free(s);
}}
BValue alloc_enum{size}(ENUM_TAG tag, {arg_params}) {{
    Enum{size}* rc = malloc(sizeof(Enum{size}));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_enum{size};
    rc->tag = tag;
    {assigns}
    return (BValue)rc;
}}"""
  arg_decls = "".join("BValue _{i};".format(i = i) for i in range(size))
  arg_params = ", ".join("BValue b{i}".format(i = i) for i in range(size))
  releases = "\n    ".join("release_value(s->_{i});".format(i = i) for i in range(size))
  assigns = "\n    ".join("rc->_{i} = b{i};".format(i = i) for i in range(size))
  return template.format(size = size, arg_decls = arg_decls, arg_params = arg_params, releases = releases, assigns = assigns)

def function_impl(size):
  define = "" if size == 1 else "DEFINE_RC_STRUCT(Closure{size}Data, BClosure{size} fn; size_t slot_len;);".format(size = size)
  template = """
{define}

BValue alloc_closure{size}(size_t size, BValue* data, BClosure{size} fn) {{
    Closure{size}Data* rc = malloc(closure_data_size(size));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of({cast_to_1}rc);
    for (size_t i = 0; i < size; i++) {{
      closure_data[i] = data[i];
    }}
    return (BValue)rc;
}}

BValue call_fn{size}(BValue fn, {arg_params}) {{
  if (IS_STATIC_VALUE(fn)) {{
    BPureFn{size} pfn = (BPureFn{size})TO_POINTER(fn);
    return pfn({just_args});
  }}
  else {{
    // this must be a closure:
    Closure{size}Data* rc = (Closure{size}Data*)fn;
    BValue* data = closure_data_of({cast_to_1}rc);
    return rc->fn(data, {just_args});
  }}
}}

BValue value_from_pure_fn{size}(BPureFn{size} fn) {{
  return (BValue)(((uintptr_t)fn) | STATIC_VALUE_TAG);
}}"""
  cast_to_1 = "" if size == 1 else "(Closure1Data*)"
  arg_params = ", ".join("BValue arg{i}".format(i = i) for i in range(size))
  just_args = ", ".join("arg{i}".format(i = i) for i in range(size))
  return template.format(size = size, cast_to_1 = cast_to_1, just_args = just_args, arg_params = arg_params, define = define)

def print_impls():
  print("// STRUCTS")
  for i in range(1, 32):
    # there is no struct1 at runtime
    print(struct_impl(i+1))
    print("")
  print("// ENUMS")
  for i in range(32):
    print(enum_impl(i+1))
    print("")
  print("// FUNCTIONS")
  for i in range(32):
    print(function_impl(i+1))
    print("")

def print_headers():
  print("//FUNCTION typedefs")
  for i in range(32):
    print("typedef BValue (*BClosure%i)(BValue*,%s);" % (i + 1, just_bvalue(i + 1)))
  for i in range(32):
    print("typedef BValue (*BPureFn%i)(%s);" % (i + 1, just_bvalue(i + 1)))
  print("")
  print("//STRUCTS")
  for i in range(1, 32):
    print("BValue alloc_struct%i(%s);" % (i + 1, bvalue_arg(i + 1)))
  print("")
  print("//ENUMS")
  for i in range(32):
    print("BValue alloc_enum%i(ENUM_TAG variant, %s);" % (i + 1, bvalue_arg(i + 1)))
  print("")
  print("//FUNCTIONS")
  for i in range(32):
    print(fn_decls(i+1))
    print()

if __name__ == "__main__":
  import sys
  arg = sys.argv[1] if len(sys.argv) > 1 else None
  if arg == "impls":
    print_impls()
  elif arg == "headers":
    print_headers()
  else:
    print("unknown args: %s" % sys.argv)
    sys.exit(1)