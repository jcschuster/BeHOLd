# BeHOLd

**BeHOLd** is a library implementing syntax for classical higher-order logic
(HOL). It extends the implementation of the simply-typed lambda calculus from
the library [**HOL**](https://hexdocs.pm/hol/readme.html) by logical constants.
**BeHOLd** should only be used together with
[**HOL**](https://hexdocs.pm/hol/readme.html) as custom term and type
construction is otherwise not supported. This module also includes a parser for
[TPTP TH0 syntax](https://doi.org/10.1007/s10817-017-9407-7).

An overview over the included modules is given in the documentation of the
[root module](https://hexdocs.pm/behold/BeHOLd.html).

A demonstration of the package which includes the implementation of classical
HOL and the parser can be found in
[this Livebook](https://hexdocs.pm/behold/demo.html).

This package was developed at the
[University of Bamberg](https://www.uni-bamberg.de/en/) with the
[Chair for AI Systems Engineering](https://www.uni-bamberg.de/en/aise/).

## Classical HOL

The type system of classical HOL is given by the following grammar, where it is
possible to introduce additional user-defined base types:

$$\alpha, \beta \coloneqq \iota \mid o \mid \alpha\to\beta$$

$o$ is the type for booleans, containing the values of $\top_o$ and $\bot_o$
while $\iota$ denotes the (nonempty) set of individuals. Note that type
construction is right-associative, i.e.,
$\alpha\to(\beta\to\gamma) = \alpha\to\beta\to\gamma$.

In addition to the simply-typed lambda calculus, classical HOL contains the
following constants with the usual interpretation:

$$
\top_o \quad \bot_o \quad \neg_{o\to o} \quad \lor_{o\to o\to o} \quad
\land_{o\to o\to o} \quad \supset_{o\to o\to o} \quad \equiv_{o\to o\to o}
$$

$$
=_{\alpha\to\alpha\to o} \quad \Pi_{(\alpha\to o)\to o} \quad \Sigma_{(\alpha\to o)\to o}
$$

Note that $=_{\alpha\to\alpha\to o}$ (short $=^\alpha$),
$\Pi_{(\alpha\to o)\to o}$ (short $\Pi^\alpha$) and
$\Sigma_{(\alpha\to o)\to o}$ (short $\Sigma^\alpha$) represent _families_ of
constants, i.e., there is one of each symbol for every type.

## Term Representation

The term representation is entirely handled by the
[`HOL`](https://hexdocs.pm/hol/readme.html) library and described
in detail in its [documentation](https://hexdocs.pm/hol/readme.html). To give a
short summary, terms are ensured to always be in $\beta\eta$-normal form, i.e.
maximally $\eta$-expanded and $\beta$-reduced until no $\beta$-reductions are
possible. Additionally, bound variables in $\lambda$-abstractions are named via
[de Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index), i.e.,
$(\lambda X. \lambda Y. X\text{ }Y)$ is represented as $(\lambda. \lambda. 2\text{ }1)$.

Internally, terms are represented by the data structure
[`HOL.Data.hol_term()`](https://hexdocs.pm/hol/HOL.Data.html#t:hol_term/0) as
records with accessor fields for the term's head, arguments, bound variables,
free variables, type and highest de Bruijn index. The term head and arguments
fields correspond to a _flattened_ representation of the term, e.g.
$((f\text{ }a)\text{ }b)$ is repesented as $(f\text{ }a\text{ }b)$ where $f$ is
the head and $a$ and $b$ are the arguments. This means, that a term's head is
always given as a
[`HOL.Data.declaration()`](https://hexdocs.pm/hol/HOL.Data.html#t:declaration/0)
of a constant, bound or free variable.

## TPTP Parsing

Classical HOL with simple types can be represented in the syntax of TPTP's
[TH0](https://doi.org/10.1007/s10817-017-9407-7). A parser for this syntax is
implemented with two different entry points. All connectives and features in
TH0 are supported.

The module [`BeHOLd.Parser`](https://hexdocs.pm/behold/BeHOLd.Parser.html)
handles simple formula strings like `"?[X : $o]: X => $true`.

The module [`BeHOLd.TPTP`](https://hexdocs.pm/behold/BeHOLd.TPTP.html) handles
file parsing for TPTP problem files and contains functionality to convert the
internal term representation back to a string in
[TH0](https://doi.org/10.1007/s10817-017-9407-7) format. Note that when parsing
or including files from the [TPTP problem library](https://tptp.org/TPTP/), a
environment variable `TPTP_ROOT` must be specified pointing to the root folder
of the TPTP library (this may require a reboot for Elixir to recognize).

## Installation

This package can be installed by adding `behold` to your list of dependencies
in `mix.exs`:

```elixir
def deps do
  [
    {:behold, "~> 1.1.0"}
  ]
end
```
