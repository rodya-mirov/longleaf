Formal Grammar
--

The grammar syntax is usual EBNF notation as I vaguely remember it (not gonna look it up).
For instance:

    Thing :=
        A Bunch Of Stuff
        | Other Stuff

Means there is a rule defining `Thing`, which is that it is either `A Bunch of Stuff` or `Other Stuff`,
where `A`, `Bunch`, `of`, `Stuff`, and `Other` are rules that are either elsewhere defined or considered
primitive. A list of primitives will come after the grammar. Note that the grammar definition is newline
sensitive, but the actual parser is whitespace insensitive. Any amount of whitespace is considered
equivalent to a single space, for the purpose of code intention.

    Statement :=
        AssignmentStatement
        | ExpressionStatement
    
    AssignmentStatement :=
        let LHS = TopExpr ;
    
    ExpressionStatement :=
        TopExpr ;

    TopExpr :=
        BooleanExpr

    BooleanExpr :=
        AddExpr
        | (AddExpr OR)+ AddExpr
        | (AddExpr AND)+ AddExpr
    
    AddExpr :=
        MulExpr
        | AddExpr AddOp MulExpr

    MulExpr :=
        UnaryExpr
        | MulExpr MulOp UnaryExpr
    
    UnaryExpr :=
        BaseExpr
        | UnaryOp BaseExpr

    BaseExpr :=
        ParenExpr
        | BlockExpr
        | FnCallExpr
        | FnDefnExpr
        | IfExpr
        | Number
        | Id
    
    ParenExpr :=
        \( TopExpr \)
    
    FnCallExpr :=
        Id \( TopExpr (, TopExpr)* (,)? \)
    
    FnDefnExpr :=
        \( Id (, Id)* (,)? \) -> BlockExpr
    
    BlockExpr :=
        \{ (Statement)* TopExpr \}

    IfExpr :=
        if \( TopExpr \) BlockExpr else BlockExpr

    BooleanOp :=
        and
        | or
    
    AddOp :=
        +
        | -

    MulOp :=
        *
        | /
    
    UnaryOp :=
        !
        | -

Note that the grammar is left recursive in order to derive correct associativity.
It is simple to eliminate this left recursion with standard tricks; e.g. that in:
    https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/Syntax.html/node8.html

Also note that the branching order is meaningful and intentional; that is, if the former
    branch "works" then it should be preferred over latter branches.

Also note that the parsing code should not be assumed to be "compiled from" the grammar;
    there are more efficient ways to (e.g.) parse expressions than a deep recursion.

Keywords (case sensitive):
    
    if, else, return, while, do, let, break, continue, goto

Not all keywords are used in the language.

Primitives:

    Number := any valid floating point literal (integer is fine; leading or trailing dot is fine)
    Id     := [A-Za-z]([A-Za-z0-9_])* except not keywords

