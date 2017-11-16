# simple-type-checker

  simple-type-checker performs type checking and type inference on a simple λ-calculus language. 
The program infers type for a given expression i.e. it either finds a type error in an expression, 
or, it infers a fully determined type. 

# expressions
the language has following expressions:
  Var(id: String) 
  Num(n: Int) 
  Bool(b: Boolean) 
  Lambda(binder: String, body: Exp) 
  Application(left: Exp, right: Exp) 
  Conditional(cond: Exp, conseq: Exp, alter: Exp) 
  Let(st: Stmt, body: Exp)

# statements
the language has following statements:
  Empty() 
  Assign(lhs: String, rhs: Exp) 
  Seq(left: Stmt, right: Stmt)

# types
expression can have following types:
  IntType() 
  BoolType() 
  VarType(id: String) 
  ArrowType(src: Type, dst: Type) 

