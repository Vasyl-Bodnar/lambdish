# lambdish

Lambda Calculus Interpreter with Definitions

## Details

You can try it out at this minimalistic [online interpreter](https://vasyl-bodnar.github.io/lambdish/). 
Otherwise, clone this repo, `dune build`, and do whatever you want to the binaries or generated static page.

Here are some implementation details:

- Use `\` for λ symbol, dot separates argument from body `\x.x`
- "Multiple" arguments can be separated by dots `\x.y.x`, or using identical explicit form `\x.\y.x`
- Order can be decided by parenthesis `\x.y.z.x z(y z)`
- Arguments can be full identifiers `\war.peace.peace`
- As a result, applications occur by space and parenthesis `\I.love.you.I (love you)`
- Functions are hungry, so to apply them use parenthesis, i.e. `(\x.x) y`, since `\x.x y` => `(λx. x y)`
- Numbers are sugar for church numerals 
    ```
    2 => (λf. (λx. f (f (x))))    
    ```
- Strings are sugar for cons list of church numerals representing the letters in ascii 
    ```
    "Hello World" => (λf. f 72 (λf. f 101 (λf. f 108 (λf. f 108 (λf. f 111 (λf. f 32 (λf. f 87 (λf. f 111 (λf. f 114 (λf. f 108 (λf. f 100 (λx. (λx0. (λy. x0))))))))))))))
    ```
- Symbols are pure text, useful for testing since numbers and strings would be expanded
    ```
    I 'alpha => alpha
    ```
- Certain functions and operations are predefined, such as:
    - `S`, `K`, and `I`
    - `true`, `false`
    - `cons`, `nil`
    - `car`, `cdr`

- You can make your own definitions with `:=` operator:
    ```
    K := \x.y.x
    ```
