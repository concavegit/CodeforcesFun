# CodeforcesFun

Category theory approaches to solving Codeforces problems where possible.
Most of the category-theoretic solutions to the problems will be more confusing than direct solutions.
I am having fun framing problems as universally as possible in order to solve more abstract problems.

## Mapping Code to Problem
The codeforces solutions are in `src/contest<contest_num>/<problem_letter>.hs`, and can be ran with `stack runghc <problem_letter.hs> < <problem_letter>.txt`, like

input:

```
cd src/contest2161 # contest at https://codeforces.com/contest/2161
stack runghc a.hs < a.txt # test problem https://codeforces.com/contest/2161/problem/A
```

output:

```
YES
YES
YES
YES
NO
NO
NO
YES
YES
NO
YES
```