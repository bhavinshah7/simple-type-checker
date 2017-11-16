package com.tc

class TypeChecker {

  sealed trait Exp
  case class Var(id: String) extends Exp
  case class Num(n: Int) extends Exp
  case class Bool(b: Boolean) extends Exp
  case class Lambda(binder: String, body: Exp) extends Exp
  case class Application(left: Exp, right: Exp) extends Exp
  case class Conditional(cond: Exp, conseq: Exp, alter: Exp) extends Exp
  case class Let(st: Stmt, body: Exp) extends Exp
  
  
  sealed trait Stmt
  case class Empty() extends Stmt
  case class Assign(lhs: String, rhs: Exp) extends Stmt
  case class Seq(left: Stmt, right: Stmt) extends Stmt

  sealed trait Type
  case class IntType() extends Type
  case class BoolType() extends Type
  case class VarType(id: String) extends Type
  case class ArrowType(src: Type, dst: Type) extends Type

  type Constraint = Tuple2[Type, Type]
  type Substitution = Map[VarType, Type]
  type Constraints = List[Constraint]

  //(λx.x) 2
  val ex1 = Application(Lambda("x", Var("x")), Num(2))
  
  //if ((λx.λy.x) tru fls) then 
  val tru = Lambda("x", Lambda("y", Var("x")))
  val btru = Application(Application(tru, Bool(true)), Bool(false))
  val x_2 = Let(Assign("x", Num(2)), Var("x"))
  val x_3 = Let(Assign("x", Num(3)), Var("x"))
  val ex2 = Conditional(btru, x_2, x_3)
  
  //  if(true) then ((λx.x)2) else ((λx.x)true) 
  val ex3 = Conditional(btru, Num(2), btru)
  
  //(λx.(if x then 4 else x)) true
  val ex4 = Application(Lambda("x", Conditional(Var("x"), Num(4), Var("x"))), btru)
  
  
val htest1 = Application(Application(Lambda("x", Lambda("y", Var("y"))), Num(1)), Bool(true))
val htest2 = Conditional(Application(Lambda("x", Var("x")), Bool(true)), Num(1), Num(2))
val htest3 = Conditional(Bool(true), Conditional(Bool(false), Num(1), Num(2)), Bool(false))
val htest4 = Let(Assign("x", Num(2)), Application(Var("x"), Num(3)))
val htest5 = Bool(true)
val htest6 = Lambda("x", Var("x"))
val htest7 = Application(Num(0), Bool(true))
val htest8 = Application(Lambda("x", Var("x")), Num(0))
val htest9 = Conditional(Bool(true), Num(0), Num(2))
val htest10 = Conditional(Num(0), Num(0), Num(2))
val htest11 = Application(Lambda("x", Num(2)), Bool(true))
val htest12 = Application(Lambda("x", Application(Var("x"), Num(2))), Lambda("y", Var("y")))
val htest13 = Lambda("x", Conditional(Var("x"), Num(1), Bool(true)))
val htest14 = Lambda("x", Conditional(Var("x"), Num(1), Num(2)))
val htest15 = Let(Seq(Assign("a", Num(1)), Assign("b", Num(2))), Lambda("x", Conditional(Var("x"), Var("a"), Var("b"))))
val htest16 = Let(Seq(Assign("a", Num(1)), Assign("b", Bool(false))), Lambda("x", Conditional(Var("x"), Var("a"), Var("b"))))
val htest17 = Lambda("x", Num(2))
val htest18 = Application(Lambda("x", Application(Var("x"), Var("x"))), Lambda("x", Application(Var("x"), Var("x"))))
val htest19 = Lambda("x", Var("x"))
val htest20 = Conditional( Var("x"), Var("y"), Application( Lambda("x", Var("x")), Var("y")))
val htest21 = Application(Lambda("x", Var("x")), Application(Lambda("x", Var("x")), Application(Lambda("x", Var("x")), Application(Lambda("x", Var("x")), Var("x")))))
val htest22 = Conditional(Var("x"), Application(Var("x"), Num(1)), Application(Var("x"), Num(2)))
val htest23 = Application(Application(Lambda("x", Lambda("x", Application(Var("x"), Num(1)))),Lambda("y", Var("y"))), Num(2))
val htest24 = Conditional(Conditional(Bool(true),Bool(false),Bool(true)),Num(5),Num(10))
val htest25 = Application(Lambda("x",Var("x")),Num(3))
val htest26 = Conditional(Conditional(Bool(true),Num(3),Num(5)),Var("y"),Num(2))
val htest27 = Application(Lambda("y",Var("z")),Var("w"))
val htest28 = Application(Application(Lambda("x",Lambda("b",Let(Seq(Assign("c", Var("b")), Assign("d", Var("c"))),Conditional(Var("d"), Var("x"), Num(4))))),Num(2)),Bool(true))
val htest29 = Let(Seq(Assign("n", Num(2)),Assign("x",Lambda("y",Application(Var("y"),Bool(true))))),Application(Var("x"),Lambda("b",Conditional(Var("b"),Num(3),Var("n")))))
val htest30 = Application(Lambda("x",Application(Var("x"), Var("x"))),Lambda("x",Application(Var("x"), Var("x"))))
val htest31 = Let(Assign("x", Var("y")),Lambda("x",Application(Var("y"),Num(5))))
val htest32 = Application(Application(Lambda("x", Lambda("y", Application(Var("y"), Var("x")))),Num(5)),Lambda("x0", Var("x0")))
val htest33 = Application(Lambda("x", Application(Var("x"), Var("x"))), Lambda("y", Application(Var("y"), Var("y"))))
val htest34 = Application(Conditional(Lambda("x",Var("x")), Num(1), Num(2)), Var("z"));
val htest35 = Let(Assign("x", Var("y")), Var("x")) 
  

  

  
  
  def testCases(): Unit = {
    //Typable 
    testTypeChecker(ex1)
    testTypeChecker(ex2)
    
    //Fail Type Check
    testTypeChecker(ex3)
    testTypeChecker(ex4)
  }
  
  def testTypeChecker(e: Exp): Unit = {
    
    val (_, c) = inferType(e, Map())
    val sub = unify(c, Map())
    val texp = typeCheck(e)
    
    println("\n-----------")
    println("\nExpression\n" + e);
    println("\nConstraints");
    if (c == Nil) {
      println("None")
    } else {
      c.map(ci => println("(" + ci._1 + ",\t" + ci._2 + ")"))  
    }
    
    println("\nSubstitutions");
    sub match {
      case None => println("None")
      case Some(submap) => {
        if (submap.isEmpty) {
          println("None")
        } else {
          submap.foreach{case (k,v) => println( k + "->" + v)}  
        }
      }
    }
    
    print("\nTypeCheck\n");
    texp match {
      case None => println("None")
      case Some(exp) => println(exp)
    }
    println("\n-----------")
  }
  
  def getConstraints(e: Exp): List[Constraint] = {
    inferType(e, Map())_2
  }
  
  def inferType(e: Exp, env: Map[Exp, Type]): (Type, Constraints) = {
   e match {
     
     case Var(id) => {
       env get Var(id) match {
         case None => (VarType(FreeVar().get),Nil) // Generate Fresh Variable
         case Some(t) => (t, Nil)         
       }
     }
     
     case Num(n) => (IntType(), Nil)
     
     case Bool(b) => (BoolType(), Nil)
     
     case Lambda(binder, body) => {
       val fresh =  VarType(FreeVar().get) // Generate Fresh Variable
       val (t2, c2) = inferType(body, env + (Var(binder)-> fresh))
       (ArrowType(fresh, t2), c2)          
     }
     
     case Application(left, right) => {
       val fresh = VarType(FreeVar().get) // Generate Fresh Variable
       val (t1, c1) = inferType(left, env)
       val (t2, c2) = inferType(right, env)
       
       (fresh, c1:::c2:::List((ArrowType(t2, fresh), t1)))
     }
     
     case Conditional(cond: Exp, conseq: Exp, alter: Exp) => {
       val (t1, c1) = inferType(cond, env)
       val (t2, c2) = inferType(conseq, env)
       val (t3, c3) = inferType(alter, env)
       (t2, c1:::c2:::c3:::List( (t1, BoolType()), (t2, t3) ) )
     }
     
     case Let(st: Stmt, body: Exp) => {
       val (env1, c1) = inferStmt(st, env)
       val (t2, c2) = inferType(body, env1)
       (t2, c1:::c2)
     }
   }
  }
  
  
  def inferStmt(st: Stmt, env: Map[Exp, Type]): (Map[Exp, Type], Constraints) = {
    st match {
      case Empty() => (env, Nil)
      case Assign(lhs: String, rhs: Exp) => {
        val (t, c) = inferType(rhs, env)
        (env + (Var(lhs) -> t), c)
      }
      case Seq(left: Stmt, right: Stmt) => {
        val (env1, c1) = inferStmt(left, env)
        val (env2, c2) = inferStmt(right, env1)
        (env2, c1 ::: c2)
      }
    }
  }
  
  
  
  def unify(c: Constraints, st: Substitution): Option[Substitution] = {
    
    c match {
      case Nil => Some(st)
      case (s: Type, t: Type)::tail =>  {
        if(s == t) {
          unify(tail, st)
        } else {
          (s, t) match {
            
            case (VarType(x : String), t) => {
              if (occurcheck(x, t)) {
                None
              } else {
                val subC = substituteC((s, t))(tail)
                unify(subC, st + (VarType(x) -> t))
              }                
            }
            
            case (s, VarType(x : String)) => {
              if (occurcheck(x, s)) {
                None
              } else {
                val subC = substituteC((t, s))(tail)
                unify(subC, st + (VarType(x) -> s))
              }  
            }
            
            case (ArrowType(s1, s2), ArrowType(t1, t2)) => {
              unify(tail:::List((s1, t1), (s2, t2)), st)  
            }
            
            case _ => None
          }
        }
      }
    }
    
  }
  
  def substituteC(c1: Constraint)(c: Constraints): Constraints = {
      c match {
      case Nil => c
      case (s, t)::tail => (substitute(c1)(s),substitute(c1)(t))::substituteC(c1)(tail) 
    }
  }
  
  def substitute (c1: Constraint)(ct: Type): Type = {
    val (VarType(id1), t) = c1
    ct match {
      case IntType() => ct
      case BoolType() => ct
      case VarType(id2: String) => if (id1 == id2) t else ct
      case ArrowType(src, dst) => ArrowType(substitute(c1)(src), substitute(c1)(dst))
    }
  }

  
  def occurcheck(x: String, t: Type): Boolean = {
    freevar(t).contains(x)
  }
  
  def freevar(t: Type): List[String] = {
    t match {
      case IntType() => Nil
      case BoolType() => Nil
      case VarType(id: String) => List(id)
      case ArrowType(src: Type, dst: Type) => freevar(src):::freevar(dst)
    }
  }
  
  
  def typeCheck(e: Exp): Option[Type] = {
    val (t0, c) = inferType(e, Map())    
    val sub = unify(c, Map())
    sub match {
      case None => None
      case Some(submap) => {
        val tfinal = submap.foldLeft(t0){ case (t, (k,v)) => substitute((k, v))(t) }
        if (containsVar(tfinal)) {
          None
        } else {
          Some(tfinal)  
        }
      }
    }
 
  }
  
  def containsVar(t: Type):Boolean = {
    t match {
      case IntType() => false
      case BoolType() => false
      case VarType(id: String) => true
      case ArrowType(src: Type, dst: Type) => containsVar(src) || containsVar(dst)
    }
  }
  
}