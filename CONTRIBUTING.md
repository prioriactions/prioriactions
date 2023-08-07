Contributing to prioriactions
================

First of all, thanks for considering contributing to `prioriactions`. We
greatly appreciate your interest in improving and expanding the package.
`prioriactions` is the result of collaborative work between programmers
and experts in different disciplines. Since successful modelling
projects involve long-term investments and the participation of multiple
teams, we are open to further expanding the set of people contributing
to the project. All people are very much welcome to **contribute to
code**, **documentation**, **testing** and **suggestions**.

## Fixing typos and documentation

The contents of this website have been produced through the utilization
of the [pkgdown R package](https://pkgdown.r-lib.org/). This implies
that there’s no need for manual HTML coding; the website’s content is
automatically aggregated from various sources including code
documentation, vignettes, and Markdown files. If you’re familiar with
using [pkgdown](https://pkgdown.r-lib.org/), you’re welcome to suggest
modifications to enhance the documentation by submitting a file change.
If you’re not acquainted with [pkgdown](https://pkgdown.r-lib.org/),
feel free to raise an
[issue](https://github.com/prioriactions/prioriactions/issues).

## Report a bug

If you want to report a bug or suggest an enhancement, it’s a good idea
to file an issue to the `prioriactions` repository at GitHub. If you’ve
found a bug, please file an
[issue](https://github.com/prioriactions/prioriactions/issues) that
illustrates the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex).

## Ask a question

Using `prioriactions` and got stuck? Browse the documentation to see if
you can find a solution. Still stuck? Post your question as an
[issue](https://github.com/prioriactions/prioriactions/issues) on
GitHub. While we cannot offer user support, we’ll try our best to
address it, as questions often lead to better documentation or the
discovery of bugs.

If you want to ask a question in private, you can contact to the package
maintainer [José Salgado-Rojas](mailto:jose.salgroj@gmail.com).

## Propose an idea

Have an idea for a new feature in `prioriactions`? Take a look at the
documentation and issue list to see if it isn’t already included or
suggested. If not, suggest your idea as an
[issue](https://github.com/prioriactions/prioriactions/issues) on
GitHub. While we can’t promise to implement every idea, it certainly
helps to:

- Explain in detail how your proposed feature would work.
- Keep the scope as narrow as possible to ensure feasibility.
- See below if you want to contribute code for your idea as well.

## Code contributions

Before making contributions to the package R or C++ code, make sure
someone from the `prioriactions` team agrees that the change you suggest
is needed.

- Fork the package and clone onto your computer. If you haven’t done
  this before, we recommend using
  `usethis::create_from_github("prioriactions/prioriactions", fork = TRUE)`.

- Install all development dependences with
  `devtools::install_dev_deps()`, and then make sure the package passes
  R CMD check by running `devtools::check()`. If R CMD check doesn’t
  pass cleanly, it’s a good idea to ask for help before continuing.

- Create a Git branch for your pull request (PR). We recommend using
  `usethis::pr_init("brief-description-of-change")`.

- Make your changes, commit to git, and then create a PR by running
  `usethis::pr_push()`, and following the prompts in your browser. The
  title of your PR should briefly describe the change. The body of your
  PR should contain Fixes \#issue-number.

- For user-facing changes, add a bullet to the top of NEWS.md (i.e. just
  below the first header). Follow the style described in
  <https://style.tidyverse.org/news.html>.
