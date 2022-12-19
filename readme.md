## ORIGINAL INPUT

```
Building for debugging...
[13/13] Linking SwiftFooPackageTests.xctest
Build complete! (1.78s)
Test Suite 'All tests' started at 2022-12-17 23:08:03.162
Test Suite 'debug.xctest' started at 2022-12-17 23:08:03.163
Test Suite 'QuickSpec' started at 2022-12-17 23:08:03.163
Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' started at 2022-12-17 23:08:03.163
Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' passed (0.001 seconds)
Test Case 'QuickSpec.SwiftFoo, Util, getInput, applies the function to the file contents if specified' started at 2022-12-17 23:08:03.164
Test Case 'QuickSpec.SwiftFoo, Util, getInput, applies the function to the file contents if specified' passed (0.0 seconds)
Test Case 'QuickSpec.SwiftFoo, Day1, part1, finds the total of the elve carrying the most calories' started at 2022-12-17 23:08:03.164
Test Case 'QuickSpec.SwiftFoo, Day1, part1, finds the total of the elve carrying the most calories' passed (0.0 seconds)
Test Case 'QuickSpec.SwiftFoo, Day1, part2, finds the sum of the top 3 elves' started at 2022-12-17 23:08:03.164
Test Case 'QuickSpec.SwiftFoo, Day1, part2, finds the sum of the top 3 elves' passed (0.0 seconds)
Test Suite 'QuickSpec' passed at 2022-12-17 23:08:03.164
         Executed 4 tests, with 0 failures (0 unexpected) in 0.001 (0.001) seconds
Test Suite 'debug.xctest' passed at 2022-12-17 23:08:03.164
         Executed 4 tests, with 0 failures (0 unexpected) in 0.001 (0.001) seconds
Test Suite 'All tests' passed at 2022-12-17 23:08:03.164
         Executed 4 tests, with 0 failures (0 unexpected) in 0.001 (0.001) seconds
```

## DESIRED OUTPUT

```
QuickSpec
  SwiftFoo
    Util
      getInput
        reads data from file [✔]
        applies the function to the file contents if specified [✔]
    Day1
      part1
        finds the total of the elf carrying the most calories [✔]
      part2
        finds the sum of the top 3 elves [✔]
```

```
data TestOutput a = Describe String [TestOutput a] 
                  | It String a
    deriving (Eq, Show, Functor, Foldable, Traversable)

data TestResult = Sucess | Failure String
    deriving  (Eq, Show)

type QuickTest = TestOutput TestResult
```
