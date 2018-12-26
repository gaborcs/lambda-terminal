# Tutorial: Intro to Lambda (draft)

Welcome to Lambda land.

## What is Lambda?

Lambda is an experimental programming language.
It's a functional language, similar in semantics to Haskell, but unlike Haskell and other languages that represent code as a set of text files, Lambda represents code as a data structure.
This enables a very different way of editing code.
Instead of using a regular text editor, you use an editor designed for Lambda, which is able to take advantage of the language design.
For example, when you view an expression definition in the editor, you can select any of its subexpressions to see their type for a better understanding of how things fit together.

## What is Lambda Terminal?

Lambda Terminal is an editor for Lambda.
Currently the only editor.
It's called Lambda Terminal because it runs in the terminal.

### Why does it run in the terminal?

The terminal allows us to optimize our interface for keyboard input without worrying about other input methods.
While relying exclusively on keyboard input makes your learning curve steeper, it leads to significant productivity gains once you get the hang of it, since you don't have to move your hand between the mouse and keyboard all the time.

With that said, a well-designed graphical UI would certainly have benefits.
It could add support for mouse and possibly even touch input while keeping the terminal version's efficient keyboard controls.
It could be more intuitive and more aesthetic.
But for now we'll keep things simple and stick with the terminal.

## Is it ready for production?

No. There are lots of rough edges and missing features. Also, we don't intend to maintain backwards compatibility at this point.

## So why should I learn it?

It offers a peek into what could be the future of programming. And it might blow your mind.

Are you ready? Let's get started.

## Setup

The easiest way to run Lambda Terminal is using Docker.
If you don't already have it, please [install it](https://docs.docker.com/install/).
Then you can run Lambda Terminal using the following command:
```
$ docker run --rm -it -v lambda-codebase:/codebase lambdaland/lambda-terminal
```

## Playing around

As you start Lambda Terminal for the first time, you will be greeted with an empty list of definitions.
Now you have three options: open a new expression definition by pressing `o`, open a new type definition by pressing `O` (capital), or quit by pressing `q`.
If you forget these (or any other) key bindings, you can always consult the [cheatsheet](https://github.com/lambdaland/lambda-terminal/blob/master/Cheatsheet.md).
Also, feel free to quit anytime, your definitions are always saved, so you get back to them when running Lambda Terminal with the above command again.

Let's open a new expression definition by pressing `o`.
You should now see an unnamed expression that is defined by an underscore.
That underscore represents a typed hole.
A typed hole is an expression that typechecks as any type but cannot be evaluated.
It is useful as a placeholder for code that is not written yet.

Let's replace the typed hole with something that can be evaluated.
Press `e` to edit the hole, type `42` and press `Enter`.
Congratulations, you've just written your first Lambda expression that can be evaluated.
You might notice `42: Integer` at the bottom of the terminal.
The part before the colon is the result of evaluating the expression.
Evaluating `42` results in `42`.
Shocking, I know.
The part after the colon shows the type of the expression.
`42` has type `Integer`.

So what can we do with 42?
We can apply functions to it.
We'll start with a simple one.
Press `(`, start writing `signum` and press `Tab` when it's selected in the autocomplete.
Pressing `Enter` instead of `Tab` won't work, since it would refer to a variable named signum, which isn't available here.
In case you mess it up, you can always press `e` to edit it or `u` to undo whatever you did (you can redo it by pressing `r`).
So when you're done you should see `signum 42`, which means the `signum` function is called with `42`.
You might also notice that `signum` is now highlighted compared to `42`, which means it is selected.
If your terminal's text color is gray, it might be hard to see which part is highlighted.
In this case, please consider adjusting your terminal's colors.
At the bottom you'll see the type of the selected expression, `λ Integer Integer`.
Whoa, there's a lambda!
`λ` is the type constructor for functions.
`λ Integer Integer` is the type of functions that take an `Integer` and return an `Integer`.

So `signum` takes an `Integer` and returns an `Integer`.
That means `signum 42` should be an `Integer`.
Let's verify that by selecting it.
Selection can be moved using either the arrow keys or the `ijkl` keys.
The `ijkl` keys work the same way as the arrow keys, but they are in a more convenient place if you're a touch typist.
Use the left arrow or the `j` key to select `signum`'s parent expression, `signum 42`.
You can use the right arrow or `l` to select a child expression, the up arrow or `i` to select the previous sibling and the down arrow or `k` to select the next sibling.
These controls might be weird at first, but don't worry, you'll get used to them.

As you select `signum 42`, you'll see that the result of evaluating it is `1` and its type is `Integer`, as we expected.
`signum` returns `1` for all positive integers, `0` for `0`, and `-1` for all negative integers.
Check it out for a couple of values by selecting `42`, pressing `e` to edit it, and moving the selection back to the root once you've inserted your desired number.

## Arithmetic expressions

In Lambda, arithmetic operators are functions too. For example, let's take a look at the `+` function.
Delete everything to get a clean slate by pressing `d` (multiple times if needed).
Press `e` to edit the typed hole that remains, type `+` and press `Tab`.
You should see that `+` has type `λ Integer (λ Integer Integer)`.
It's a function that takes an `Integer` and returns a function that takes an `Integer` and returns an `Integer`.
Functions in Lambda cannot take multiple arguments the way functions in most other languages can, so this trick is used instead.
It's called currying.

Time to use `+` for something.
Remember how we used `(` to apply a function (`signum`) to the selected value (`42`)?
This time we'll do the opposite, we'll call the selected function (`+`) with a value (`1`).
Press `)`, then type `1` and press `Enter`.
You should get `+ 1`, and if you select all of it, you'll see that it's type is `λ Integer Integer`.
This is a function that returns its argument incremented by `1`.
Let's call this resulting function with another value.
Press `)`, then type `2` and press `Enter`.
You should now see `+ 1 2`.
If you select all of it, you'll see that its result is `3` and its type is `Integer`.

In most languages you would write `1 + 2` instead of `+ 1 2`, but Lambda is different.
Arithmetic operators in Lambda are regular functions, and functions are displayed before the argument they are called with.
This might be weird at first, but it has a couple of advantages:
- You don't need to apply both arguments to a binary operator, you can get useful functions by applying only one: `+ 1` is a function that returns its argument incremented by `1`, `* 2` is a function that returns its argument multiplied by `2`.
- Functions can be passed to other functions. For example, you could pass `* 2` to a function that maps it over every element of a list. We'll see examples of this later.
- You don't need to think about precedence rules. All functions have the same precedence.
- The language is simpler.

Alright, what about more complex arithmetic expressions?
How about `1 + 2 * 3`?
That would look something like `+ 1 (* 2 3)` in Lambda.
Or, if we use the default wrapping style, the parentheses will be replaced by wrapping:
```
+ 1
  * 2 3
```
Let's input that expression now.
If you already have `+ 1 2`, then you should select `2`, apply `*` to it, then select `* 2` and call it with `3`.
When you're done you can select the whole expression and see that the result of evaluating it is 7.

This would be a good time to try cycling between wrapping styles using `Tab`. It shows that there are multiple ways to render expressions in Lambda.
It's the renderer's job to render expressions the best way it can, taking into account the user's preferences.

Alright, now that you've seen the basics, let's write something useful.
In the next sections, we'll learn more about the language by defining some useful functions and types.
You should go through them in the given order, because some of them build on each other.
Some of them are built-ins in other languages, but for Lambda we thought it would be a good learning opportunity to let you define them for yourself.
We hope you'll enjoy the exercise.

## `increment`
Remember that `+ 1` is a function that increments its argument by `1`?
That actually sounds like a useful function.
Let's define it as a reusable function.
Press `o` to open a new expression definition, then press `N` (capital, lowercase `n` is for renaming variables), type `increment` and press `Enter`.
Input `+ 1` the same way we did previously.

We now have an `increment` function that we could use from other definitions.
Let's try that out.
Press `o` to open a new expression definition.
Replace the typed hole with `increment` and call that with `2` using the usual editing commands.
You should now have `increment 2`, and if you select it all, you'll see that the result of evaluating it is `3` and it has type `Integer`, as expected.

So `increment 2` has the same result as `+ 1 2`.
It's not too surprising if you understand Lambda's syntax, as `increment` is defined to be `+ 1`.
So should you use `increment` or `+ 1` in your code?
Both have the same result, so you should choose the one that is more readable in the given context.
For incrementing a counter, `increment` might sound a bit more natural, but for mathematical formulas `+ 1` does.

## `square`

When we defined the `increment` function, we called the existing `+` function with one argument, and it resulted in the appropriate function.
That was a pretty elegant solution, but we can't do it in all cases.
Let's say we want to write a function that squares its argument.
Can we call the `*` function with something that does what we need?
No, we can't.
We'll learn a new language construct to achieve what we want.

Let's start with an empty expression definition, so either open a new one or delete everything in the current one.
Press `e` to edit, then `\` to insert a function.
You should see a `λ` appear, followed by a cursor, then an arrow, and finally a typed hole.
Now you need to input the name of the function's parameter.
Type `n`, then press `Enter`.
The cursor will shift to after the arrow.
This will be the function's body.
We want to square `n`, so the body should be `* n n`.
Of course writing that will be more than one step, so let's just insert `*` for now.
At this point, you should see `λ n -> *`.
That's actually a valid function, it takes an argument but ignores it and returns the `*` function.
It's not what we need though, so let's call `*` with `n`, then call `* n` with `n`.
You should now see `λ n -> * n n`, which is the squaring function we wanted.

Let's name the function `square`.
Remember, you can do that by pressing `N`.
Now let's try it out.
Press `o` to open a new expression definition, press `e` to edit the typed hole, start typing `square`, then select it from the autocomplete using `Tab`.
As you can see, the type of `square` is `λ Integer Integer`, so let's call it with an `Integer`.
This time choose whatever number you like, and make sure the square function gives the correct result for it.

## `factorial`

Now we'll define a more complex function.
The factorial function can be defined the following way in mathematics:
```
0! = 1
n! = n * (n - 1)!, if n > 0
```

The definition in Lambda is very similar to the above (shown with parentheses this time for easier comparison):
```
λ 0 -> 1
| n -> * n (factorial (- n 1))
```

The definition has two alternatives.
The first one says that if the argument is `0`, the result should be `1`.
If the argument isn't `0`, then we move on to the second alternative, which multiplies `n` with the factorial of `n` minus `1`.
We've introduced two new concepts in this example:
- Pattern matching: A function can have multiple alternatives, and each one consists of a pattern and an expression. The first one whose pattern matches the argument is used. Variables introduced in the pattern can be used in the expression of the alternative.
- Recursion: The `factorial` function calls itself in one of the alternatives, so it is a recursive function.

To enter the above definition, start with an empty expression definition and name it `factorial`.
Then you can press `e`, then `\`, enter `0` and then `1` to obtain `λ 0 -> 1`.
To add an alternative, you need to press `|`.
Now you can enter the above using the usual input methods.
Don't worry if instead of parentheses you see wrapping, it would be formatted the following way with the default wrapping style:
```
λ 0 -> 1
| n ->
  * n
    factorial
      - n 1
```

When you're done, open a new definition and call the `factorial` function with a non-negative integer.
It should work as expected for all non-negative integers.
What about negative integers?
You'll get this: `<eval timeout>: Integer`
The type inference algorithm infers that the result would be an `Integer`, but the evaluation times out.
Lambda Terminal tries to evaluate things automatically to make its usage more convenient, but it does so with a time limit to avoid wasting resources.
The time limit is short, it feels instant to us humans, but a computer is able to do a lot of calculations in that time.
However in this case the calculation times out, since calling `factorial` with a negative number would execute forever.
It would keep calling itself with lower numbers, getting further from 0 at every step.

Is `factorial`'s behavior acceptable?
If we know that we're passing it a non-negative number then it's fine.
But what if we pass it a number that could be negative?
In that case, executing forever is not a good idea.
We should write a safe version of factorial that handles negative numbers.
To be able to do that, let's first write a function that determines whether a number is negative.
Which brings us to...

## `Bool`

A function that determines whether a number is negative should return a boolean.
However, Lambda doesn't have a built-in boolean type.
That's because it doesn't need one.
Let's define it ourselves.
Press `O` (capital) to open a new type definition, then press `N` to name it `Bool`.
Now we have a type definition whose type constructor is named `Bool` and has no arguments (we'll learn about type constructor arguments later).
It also has no data constructors, which means we cannot create a value that has this type.
Let's add two data constructors, `True` and `False`, without arguments.
Press `a`, enter `True`, then press `Enter`, then do the same for `False`.
Now we have an appropriate boolean type, we can start writing functions for it.

## `isZero`
Let's start with a simple one, a function that takes an `Integer` and returns a `Bool` indicating if it is zero.
Open a new expression definition and name it `isZero`.
Press `\`, type `0` and press `Enter`, then type `True` and press `Tab`.
Press `|`, then just press `Enter` to insert a wildcard, then type `False` and press `Tab`.
You should now have this:
```
λ 0 -> True
| _ -> False
```
The wildcard pattern matches everything like a variable would, but it doesn't introduce a variable binding (you can't refer to `_` like you could to a variable, `_` would mean a typed hole when used as an expression).
The wildcard pattern is useful because you can see at a glance that the value won't be used, even in a complex expression.

Try the `isZero` function to prove to yourself that it's correct.
You may notice that it only accepts `Integer`s.
The wildcard pattern would accept any type, but `0` is an `Integer`, and because of the way the type inference algorithm works this is considered a function that only accepts `Integer`s.
That's probably a good thing by the way, because it doesn't make much sense to call it with another type, it would always return `False`.
If you call `isZero` with another type, it's probably not what you intended to do, and the typechecker will notify you of the error.

## `equal`
Let's write a function that checks whether two `Integer`s are equal.
Open a new definition and name it `equal`.
This function needs to take two `Integer`s, but functions in Lambda always take one argument.
Remember how the `+` function takes an `Integer` and returns a function that takes another `Integer`?
We'll do the same here.
The result will be whether the difference of the two arguments is zero.
Please input the following using the usual editing commands:
```
λ n ->
  λ m ->
    isZero
      - n m
```

## `isNegative`
Now we can write `isNegative`.
We'll need to check if the signum of the number is -1.
Open a new expression definition, name it `isNegative`, and input this expression:
```
λ n ->
  equal -1
    signum n
```

Okay, we've written a couple of `Bool`-producing functions.
How should we use them?

## `boolToInteger`
Let's define a function that converts a `Bool` to an `Integer`.
We can use pattern matching to do this easily:
```
λ True -> 1
| False -> 0
```
Pattern matching works well for defining such simple functions, but in other cases an if expression is more convenient...

## `if`
An if expression has three parts: a condition, a then branch and an else branch.
In Lambda, we can define an `if` function that takes these as arguments and returns the appropriate branch based on the condition:
```
λ condition ->
  λ then ->
    λ else ->
      match condition
        λ True -> then
        | False -> else
```

The `match` in the middle is not a function.
It means that the function below is called with `condition`.
So if `condition` matches `True`, it will return the `then` argument, otherwise it will return the `else` argument.
You can input in by starting with `condition`, then pressing `(` to apply a function to it, then pressing `\`.
As you press `\`, the editor will shift to this style of displaying the call.
This style resembles `match`/`case` expressions from some other languages, and it should make things pretty readable.

In most languages, calling a function requires evaluating all of its arguments.
That means `if` cannot be a function, because we want to avoid evaluating both of its branches.
Lambda is different, as it uses lazy evaluation, so only the appropriate branch will be evaluated.

## `OptionalInteger`

We wanted to write a safe factorial function that returns a special value for negative numbers instead of calculating forever.
But what should that special value be?
It could be -1.
That would be a reasonable choice, but it would need to be documented and users of the function will need to read that documentation to be able to use it safely.
It would be easy to forget.
Ideally the safe factorial function should return an optional integer.
If the argument is negative, it should return nothing, otherwise it should return just the integer we need.
Lambda doesn't have a built-in type for this though.
Why?
Because we can define it ourselves, obviously.

Press `O` to open a new type definition, then `N` and name it `OptionalInteger`.
Press `a` and add a data constructor called Nothing, then press `a` again to add one called `Just`.
As `Just` is selected, press `>` to give it a parameter, start typing `Integer`, then press `Tab` to choose it from the autocomplete.

## `safeFactorial`
We now have everything we need to define a safe factorial function.
Open a new expression definition and name it `safeFactorial`, then input the following:
```
λ n ->
  if
    isNegative n
    Nothing
    Just
      factorial n
```

## `Optional`
`OptionalInteger` is nice, but do we want to define an optional type for each type we need?
No.
There's a better way.
We can define an optional type that works for any type.
To do that, we'll replace `Integer` with a type variable.
A common name for type variables that can contain anything is `a`, so we'll use that name.

Press `g` until you get back to `OptionalInteger`.
Press `N` to rename it to `Optional`.
Make sure the type constructor (`Optional`) is selected, then press `>`, type `a` and press `Enter` to give it a parameter called `a`.
If you're done, select `Integer` (the parameter of `Just`) and press `e` to edit it.
Replace `Integer` with `a` and press `Enter`.

Now `Optional` type can be used for any type, not only `Integer`.
Press `G` to go forward until you reach `safeFactorial`, and observe that its type is now `λ Integer (Optional Integer)`.
We don't need to change anything, because the changes we made to the original `OptionalInteger` type didn't break anything, they just made the type useful in more situations.

## Conclusion
In this tutorial, we went through the basics of Lambda.
While you're probably missing some things from more mature languages and tools, you should now have a feel for where the project is headed.

Is this the future of programming?
What do you think?
