# simple-type-checker

  simple-type-checker performs type checking and type inference on a simple λ-calculus language. 
The program infers type for a given expression i.e. it either finds a type error in an expression, 
or, it infers a fully determined type. 

The language has following expressions, statements and types:

## expressions  

  Var(id: String)  
  Num(n: Int)   
  Bool(b: Boolean)   
  Lambda(binder: String, body: Exp)   
  Application(left: Exp, right: Exp)   
  Conditional(cond: Exp, conseq: Exp, alter: Exp)   
  Let(st: Stmt, body: Exp)  

## statements
 
  Empty()   
  Assign(lhs: String, rhs: Exp)   
  Seq(left: Stmt, right: Stmt)  

## types

  IntType()   
  BoolType()   
  VarType(id: String)   
  ArrowType(src: Type, dst: Type)   

