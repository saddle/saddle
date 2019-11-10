---
title: 'Saddle Documentation'
weight: 1
---

[![Build status](https://github.com/pityka/saddle/workflows/CI/badge.svg)](https://github.com/pityka/saddle/actions)
[![codecov](https://codecov.io/gh/pityka/saddle/branch/master/graph/badge.svg)](https://codecov.io/gh/pityka/saddle)
[![doc](https://img.shields.io/badge/api-scaladoc-green)](https://pityka.github.io/saddle/api/org/saddle/Frame.html)
[![doc](https://img.shields.io/badge/docs-green)](https://pityka.github.io/saddle)
[![mavne](https://img.shields.io/maven-central/v/io.github.pityka/saddle-core_2.12.svg)](https://repo1.maven.org/maven2/io/github/pityka/saddle-core_2.12/)

Saddle is a Scala data manipulation library inspired by R's data frames and Python's pandas. 
Saddle's [original repository](https://github.com/saddle/saddle) is abandoned and what you see here 
is a fork of that. 

Major changes in this fork:

- `saddle-core` has no java dependency
- Addition of `saddle-linalg`, an interface to native BLAS (thanks to netlib-java)
- Revised core APIs (e.g. no implicit conversions)
- Artifacts are published under io.github.pityka organization name

