# What is this?

A Functional style CSS Preprocessor written in Haskell in the early concept stages.  It is heavily inspired by Sass, which feels like a natural extension of CSS.  None of this "let's get rid of semicolons and curly braces" nonsense.

# Why not use Clay?

It's true, Clay can programatically generate CSS and is also written in Haskell, but it fails to appeal to CSS authors because it looks absolutely nothing like CSS.  I want to be able to paste valid CSS into my code and have it just work.

# So why not use Sass or LESS?

Sass is written in Ruby, and LESS depends on JavaScript.  I don't know Ruby, and I'd rather not touch JavaScript if I can help it, so I can't contribute to either project in a meaningful way.

Both preprocessors have deficiencies that I'd like to solve.  I'm probably not ready to write something like this, but I'd like to at least give it a shot.

# Language Constructs

## Variable Declarations

The equal sign is used for declaring variables.  Sass uses a colon, but this causes confusion for new users when they want to store the name of a property in a variable.  Users are familiar with the idea of using the dollar sign to indicate variables from other languages.

```fcss
$foo = 10px;
$bar = 1px solid black;
```

Would like to see variables be immutable.  Redeclaring them causes the previous declaration to be shadowed.

## Looping

The preferred method for looping is via map.  I am expecting many users to have trouble grasping this, especially when the desire is to take advantage of the traditional loop counter.  Users will have to be introduced to the concept of creating ranges in Haskell (eg. `foo = [1..10]`) for the purpose of feeding to a mixin (or inline/lambda mixin?).
