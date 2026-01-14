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
\quad \Pi_{(\alpha\to o)\to o} \quad \Sigma_{(\alpha\to o)\to o} \quad
=_{\alpha\to\alpha\to o}
$$

## Term Representation

The term representation is entirely handled by the
[`HOL`](https://hexdocs.pm/hol/readme.html) library and described
in detail in its [documentation](https://hexdocs.pm/hol/readme.html). To give a
short summary, terms are ensured to always be in $\beta\eta$-normal form, i.e.
maximally $\beta$-reduced and $\eta$-expanded. Additionally, bound variables in
$\lambda$-abstractions are named via
[De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index), i.e.,
$\lambda X. \lambda Y. X Y$ is $\alpha$-renamed to $\lambda 2. \lambda 1. 2 1$.

Internally, terms are represented by the data structure
[`HOL.Data.hol_term()`](https://hexdocs.pm/hol/HOL.Data.html#t:hol_term/0) as
records.

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
    {:behold, "~> 1.0.1"}
  ]
end
```
