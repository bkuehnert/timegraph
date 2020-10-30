# Timegraph

`:timegraph` is a Common Lisp utility for computing temporal relations between episodes via the [timegraph](#1) model.

A timegraph is a directed, acyclic graph whose vertices represent single points in time, and whose edges represent a ≤ relationship. Each episode corresponds to two points in the timegraph: One representing the beginning of the episode, and one representing the end of the episode (and the first coming before the second).

## Installation
The `:timegraph` system has no dependencies. The easiest way to install it is with quicklisp. Clone the repository to `~/quicklisp/local-projects/`, and then run `(ql:quickload :timegraph)` in the REPL.

To run the tests, first install `clunit2` (available on quicklisp), and then run `(asdf:test-system :timegraph)`. 

## How to use
The timegraph is interfaced via propositions. A proposition is a three-element list, for example `(e1 :equal e2)`. The first and third elements are symbols which represent episodes, and the second element is a condition. The available conditions, and their effects on episode `e1` and `e2` are:
* `:equal` - `e1` and `e2` start at the same time, and end at the same time.
* `:before` - `e1` starts before (or equal to) `e2` starts.
* `:after` - `e1` starts after (or ) `e2` starts.
* `:precond` - `e1` ends before `e2`starts.
* `:postcond` - `e1` starts after `e2` ends
* `:during` - `e1` starts before `e2`starts, and ends before `e2` ends.

**Note**: in the above list, any time you see "before" (or "after"), it's implied that this is a ≤ (or ≥) relation. 

There are two functions that operate on propositions:
```lisp
(assert-prop prop tg)
```
Asserts the proposition `prop` in the timegraph `tg`.
```lisp
(eval-prop prop tg)
```
Evaluates the proposition `prop` in the the timegraph `tg`. This returns `T` if the inference can be made, and `NIL` if the inference cannot be made. 
**Note**: Since the timegraph can only store ≤ relations, then we cannot assert any < relation. Thus, propositions cannot be proven false.

#### References
<a id="1">[1]</a>: Taugher J. 1983. An efficient representation for time information. M.<span></span>Sc. thesis, Department of Computing Science, University of Alberta, Edmonton, AB.
