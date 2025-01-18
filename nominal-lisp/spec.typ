== nominal lisp
\

$
#let data = $bold("data")$
#let where = $bold("where")$
#let indent = h(0.5cm)
data "Term" where \
indent #[`V`] : AA -> "Term" \
indent #[`L`] : "List"("Term") -> "Term" \
indent #[`B`] : angle.l AA angle.r "Term" -> "Term" \
""$

Fundamental data types in the language:
- Symbol ($AA$)
- Pair
- Atom
- Binder

```
eval(env, symbol(s))     := lookup(env, s)
eval(env, pair(f, args)) :=
  let f_result = eval(env, f) in
  apply_or_special_form(f_result, args)
eval(env, atom(a))       := atom(a)
eval(env, binder(v, e))  := for v' # env,
                            binder(v', eval([v'/v]e))
```
