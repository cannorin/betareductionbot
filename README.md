Beta Reduction Bot
==================

[![Build Status](https://travis-ci.org/cannorin/betareductionbot.svg?branch=heroku)](https://travis-ci.org/cannorin/betareductionbot)

[@b_rdct](https://twitter.com/b_rdct)

## Usage

```@b_rdct <term>``` ... β-reduce given ```<term>``` and show the result

```@b_rdct -ni|-noimage <term>``` ... β-reduce given ```<term>``` and show the result without detailed image

```@b_rdct <name> := <term>``` ... assign given ```<term>``` to variable ```<name>```

```@b_rdct [-ni|-noimage] <name> :b= <term>``` ... β-reduce given ```<term>``` and assign the result to variable ```'<name>'```

```@b_rdct -sv|-showvars``` ... show declared variables 

## Syntax

    term := variable | application | abstraction | number | meta
    variable := [ 'a'..'z' 'A'..'Z' ]
    number := [ 0..9 ]+
    meta := '&' [ 'a'..'z' 'A'..'Z' ]+
    application := ( variable | abstraction | number | meta | '(' application ')' )2+
    abstraction := '(' ( 'λ' | '\' | '^' ) variable+ '.' term ')'

## Example

    you:    @b_rdct (\xyz.xz(yz))abc
    b_rdct: @you ac(bc)

## Numbers

Integers larger than 0 will be converted to Church integers.

    you:    @b_rdct 10
    b_rdct: @you (λfx.f(f(f(f(f(f(f(f(f(fx))))))))))

## Meta Variable Expressions

You can use variables declared by ```:=``` by adding "&" to its head.

    you:    @b_rdct x := &S &K &K
    b_rdct: @you x := (&S)(&K)(&K) (will expire in 30 minutes)
    you:    @b_rdct &x
    b_rdct: @you (λz.z)

β-reduction result can be assigned to variable using ```:b=``` instead.

    you:    @b_rdct x :b= &S &K &K
    b_rdct: @you x := (λz.z)

You can use these variables without declaring them below:

    s, S := (^xyz.xz(yz))
    k, K := (^xy.x)
    i, I := (^x.x)
    b, B := (^xyz.x(yz))
    c, C := (^xyz.xzy)
    w, W := (^xy.xyy)
    y, Y := (^f.(^x.f(xx))(^x.f(xx)))
    z, Z := (^f.(^x.f(^y.xxy))(^x.f(^y.xxy)))
    succ := (^nfx.f(nfx))
    plus := (^mnfx.mf(nfx))
    mult := (^mnf.m(nf))
    pow := (^xy.yx)
    pred := (^nfx.n(^gh.h(gf))(^u.x)(^u.u))
    true := (^xy.x)
    false := (^xy.y)
    and := (^pq.pq(^xy.y))
    or := (^pq.p(^xy.x)q)
    not := (^p.p(^xy.y)(^xy.x))
    ifthenelse := (^pxy.pxy)
    iszero := (^n.n(^x.(^pq.q))(^pq.p))
    cons := (^sbf.fsb)
    car := (^p.p(^xy.x))
    cdr := (^p.p(^xy.y))
    nil := (^xy.y)
    isnil := (^l.l(^htd.(^xy.y))(^xy.x))

Variables declared by users will expire in 30 minutes.

## Restrictions

* λ-variables are limited to 1 character, whereas meta variables can have long names.

* The computation times out after 30 seconds.

* Most of infinite loops will be detected and terminated.
