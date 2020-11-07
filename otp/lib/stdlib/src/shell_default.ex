defmodule :m_shell_default do
  use Bitwise
  import :io, only: [format: 1]

  def help() do
    format('** shell internal commands **~n')
    format('b()        -- display all variable bindings\n')
    format('e(N)       -- repeat the expression in query <N>\n')
    format('f()        -- forget all variable bindings\n')
    format('f(X)       -- forget the binding of variable X\n')
    format('h()        -- history\n')
    format('h(Mod)     -- help about module\n')
    format('h(Mod,Func)-- help about function in module\n')
    format('h(Mod,Func,Arity) -- help about function with arity in module\n')
    format('ht(Mod)    -- help about a module\'s types\n')
    format('ht(Mod,Type) -- help about type in module\n')
    format('ht(Mod,Type,Arity) -- help about type with arity in module\n')
    format('hcb(Mod)    -- help about a module\'s callbacks\n')
    format('hcb(Mod,CB) -- help about callback in module\n')
    format('hcb(Mod,CB,Arity) -- help about callback with arity in module\n')
    format('history(N) -- set how many previous commands to keep\n')
    format('results(N) -- set how many previous command results to keep\n')
    format('catch_exception(B) -- how exceptions are handled\n')
    format('v(N)       -- use the value of query <N>\n')
    format('rd(R,D)    -- define a record\n')
    format('rf()       -- remove all record information\n')
    format('rf(R)      -- remove record information about R\n')
    format('rl()       -- display all record information\n')
    format('rl(R)      -- display record information about R\n')
    format('rp(Term)   -- display Term using the shell\'s record information\n')
    format('rr(File)   -- read record information from File (wildcards allowed)\n')
    format('rr(F,R)    -- read selected record information from file(s)\n')
    format('rr(F,R,O)  -- read selected record information with options\n')
    format('** commands in module c **\n')
    :c.help()
    format('** commands in module i (interpreter interface) **\n')
    format('ih()       -- print help for the i module\n')
    true
  end

  def bi(i) do
    :c.bi(i)
  end

  def bt(pid) do
    :c.bt(pid)
  end

  def c(file) do
    :c.c(file)
  end

  def c(file, opt) do
    :c.c(file, opt)
  end

  def c(file, opt, filter) do
    :c.c(file, opt, filter)
  end

  def cd(d) do
    :c.cd(d)
  end

  def erlangrc(x) do
    :c.erlangrc(x)
  end

  def flush() do
    :c.flush()
  end

  def h(m) do
    :c.h(m)
  end

  def h(m, f) do
    :c.h(m, f)
  end

  def h(m, f, a) do
    :c.h(m, f, a)
  end

  def ht(m) do
    :c.ht(m)
  end

  def ht(m, f) do
    :c.ht(m, f)
  end

  def ht(m, f, a) do
    :c.ht(m, f, a)
  end

  def hcb(m) do
    :c.hcb(m)
  end

  def hcb(m, f) do
    :c.hcb(m, f)
  end

  def hcb(m, f, a) do
    :c.hcb(m, f, a)
  end

  def i() do
    :c.i()
  end

  def i(x, y, z) do
    :c.i(x, y, z)
  end

  def l(mod) do
    :c.l(mod)
  end

  def lc(x) do
    :c.lc(x)
  end

  def ls() do
    :c.ls()
  end

  def ls(s) do
    :c.ls(s)
  end

  def m() do
    :c.m()
  end

  def m(mod) do
    :c.m(mod)
  end

  def lm() do
    :c.lm()
  end

  def mm() do
    :c.mm()
  end

  def memory() do
    :c.memory()
  end

  def memory(type) do
    :c.memory(type)
  end

  def nc(x) do
    :c.nc(x)
  end

  def ni() do
    :c.ni()
  end

  def nl(mod) do
    :c.nl(mod)
  end

  def nregs() do
    :c.nregs()
  end

  def pid(x, y, z) do
    :c.pid(x, y, z)
  end

  def pwd() do
    :c.pwd()
  end

  def q() do
    :c.q()
  end

  def regs() do
    :c.regs()
  end

  def uptime() do
    :c.uptime()
  end

  def xm(mod) do
    :c.xm(mod)
  end

  def y(file) do
    :c.y(file)
  end

  def y(file, opts) do
    :c.y(file, opts)
  end

  def iaa(flag) do
    calli(:iaa, [flag])
  end

  def iaa(flag, fnk) do
    calli(:iaa, [flag, fnk])
  end

  def ist(flag) do
    calli(:ist, [flag])
  end

  def ia(pid) do
    calli(:ia, [pid])
  end

  def ia(x, y, z) do
    calli(:ia, [x, y, z])
  end

  def ia(pid, fnk) do
    calli(:ia, [pid, fnk])
  end

  def ia(x, y, z, fnk) do
    calli(:ia, [x, y, z, fnk])
  end

  def ib(mod, line) do
    calli(:ib, [mod, line])
  end

  def ib(mod, fnk, arity) do
    calli(:ib, [mod, fnk, arity])
  end

  def ibd(mod, line) do
    calli(:ibd, [mod, line])
  end

  def ibe(mod, line) do
    calli(:ibe, [mod, line])
  end

  def iba(m, l, action) do
    calli(:iba, [m, l, action])
  end

  def ibc(m, l, cond__) do
    calli(:ibc, [m, l, cond__])
  end

  def ic() do
    calli(:ic, [])
  end

  def ih() do
    calli(:help, [])
  end

  def ii(mod) do
    calli(:ii, [mod])
  end

  def ii(mod, op) do
    calli(:ii, [mod, op])
  end

  def il() do
    calli(:il, [])
  end

  def im() do
    calli(:im, [])
  end

  def ini(mod) do
    calli(:ini, [mod])
  end

  def ini(mod, op) do
    calli(:ini, [mod, op])
  end

  def inq(mod) do
    calli(:inq, [mod])
  end

  def ip() do
    calli(:ip, [])
  end

  def ipb() do
    calli(:ipb, [])
  end

  def ipb(mod) do
    calli(:ipb, [mod])
  end

  def iq(mod) do
    calli(:iq, [mod])
  end

  def ir(mod, line) do
    calli(:ir, [mod, line])
  end

  def ir(mod, fnk, arity) do
    calli(:ir, [mod, fnk, arity])
  end

  def ir(mod) do
    calli(:ir, [mod])
  end

  def ir() do
    calli(:ir, [])
  end

  def iv() do
    calli(:iv, [])
  end

  defp calli(f, args) do
    :c.appcall(:debugger, :i, f, args)
  end
end
