import sexp._

sealed abstract class Exp
case class Literal(v: SExp) extends Exp
case class Call(lhs : Exp, rhs : List[Exp]) extends Exp
case object Null extends Exp
case class If(cond : Exp, t : Exp, f : Exp) extends Exp
case class Let(v : String, rhs: Exp, body : Exp) extends Exp
case class Ref(v : String) extends Exp
case class Equal(rhs: Exp, lhs: Exp) extends Exp
case class Def(name : String, argument : List[String], body : Exp) extends SExp
case class Program(defs : List[SExp], exp : Exp)
case class Quote(variable: SExp) extends Exp
case class Lambda(args : List[String], body: Exp) extends Exp
case class Closure( args : List[String], body: Exp, env : Env) extends SExp
case class Primitive(name: String) extends SExp
class Box[A] {
  var contents : Option[A] = None
}


def parseDef(navn: String, p: SExp, body: SExp, acc: List[String]) : Def =
  p match {
    case SNil => Def(navn, acc.reverse, parseExp(body))
    case SCons(SSymbol(id), rest) => parseDef(navn, rest, body, id :: acc)
  }



def parseProgram(p : SExp) : Program = {
  parseProgramHelp(p, List())
}

def parseProgramHelp(p : SExp, acc : List[Def]) : Program = {
  p match {
    case SList(first) => Program(acc.reverse, parseExp(first))
    case SCons(SList(SSymbol("define"), SCons(SSymbol(name), params), body), rest) =>
      parseProgramHelp(rest, parseDef(name, params, body, List()) :: acc)
    case SCons(SList(SSymbol("define"), SList(SSymbol(name), body)), rest) =>
      parseProgramHelp(rest, parseDef(name, SNil, body, List()) :: acc)
    case _ => throw new IllegalArgumentException("DUUUUDE, not a valid program: " + p)
  }
}
def parseExp(e: SExp) : Exp = {
    e match {
      case STrue() => Literal(STrue())
      case SFalse() => Literal(SFalse())
      case SInt(v) => Literal(SInt(v))
      case SNil => Literal(SNil)
      case SSymbol("null") => Null
      case SSymbol(id) => Ref(id)
      case SList(SSymbol("let"), SList(SList(SSymbol(id), rhs)), body) =>
        Let(id, parseExp(rhs), parseExp(body))
      case SList(SSymbol("if"), condition, lhs, rhs) =>
        If(parseExp(condition), parseExp(lhs), parseExp(rhs))
      case SList(SSymbol("quote"), lhs) =>
        Quote(lhs)
      case SList(SSymbol("lambda"),args,body) => {
        parseLambda(args, body, List())
      }
      case SCons(id, args) =>
        parseCall(args, parseExp(id), List())
      case _ => throw new IllegalArgumentException("not a valid expression: " + e)
    }
}

def parseCall(args: SExp, id: Exp, acc: List[Exp]) : Call ={
  args match{
    case SNil => Call(id, acc.reverse)
    case SCons(first, rest) => parseCall(rest, id, parseExp(first) :: acc)
  }
}
def parseLambda(id: SExp, body: SExp, acc: List[String]): Lambda =
  id match{
    case SNil => Lambda(acc.reverse, parseExp(body))
    case SCons(SSymbol(id), rest) => parseLambda(rest, body, id::acc)

}


type Env = Map[String, Box[SExp]]


def interpExp(e: Exp, env : Env) : SExp = {
    e match {
        case Literal(v) => v
        case Null => SNil
        case Call(e1, e2) => {
            e1 match{
              case Lambda(params, body) => interpExp(Lambda(params, body), env) match {
                case Closure(params, body, ending) =>
                  interpExp(body, argsToEnv(params zip e2, ending, env))
                }
              case Ref(ref) => env.get(ref) match {
                case Some(box) => box.contents.get match{
                  case Closure(args, body, boxEnv) => interpExp(body, argsToEnv(args zip e2, boxEnv, env))
                    // interpExp(Call(e1, rest), env)

                  case Primitive(v) =>
                    v match{
                      case "+" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SInt(v), SInt(v2)) => SInt(v + v2)
                          case _ => throw new RuntimeException("Failed addition operation")
                        }
                      case "-" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SInt(v), SInt(v2)) => SInt(v - v2)
                          case _ => throw new RuntimeException("Failed subtraction operation")
                        }
                      case "/" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SInt(v), SInt(v2)) => SInt(v / v2)
                          case _ => throw new RuntimeException("Failed division operation")
                        }
                      case "*" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SInt(v), SInt(v2)) => SInt(v * v2)
                          case _ => throw new RuntimeException("Failed multiplication operation")
                        }
                      case ">" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SInt(v), SInt(v2)) => if (v > v2) STrue() else SFalse()
                          case _ => throw new RuntimeException("Failed greater than operation")
                        }
                      case "<" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SInt(v), SInt(v2)) => if (v < v2) STrue() else SFalse()
                          case _ => throw new RuntimeException("Failed less than operation")
                        }
                      case ">=" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SInt(v), SInt(v2)) => if (v >= v2) STrue() else SFalse()
                          case _ => throw new RuntimeException("Failed greater than or equal to operation")
                        }
                      case "<=" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SInt(v), SInt(v2)) => if (v <= v2) STrue() else SFalse()
                          case _ => throw new RuntimeException("Failed less than or equal to operation")
                        }
                      case "cons" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(head: SExp, tail: SExp) => SCons(head, tail)
                          case _ => throw new RuntimeException("Failed cons opertaion")
                        }
                      case "car" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SCons(head, tail)) => head
                          case _ => throw new RuntimeException("Failed car operation")
                        }
                      case "cdr" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SCons(head, tail)) => tail
                          case v => throw new RuntimeException("Failed cdr operation: "+ v)
                        }
                      case "equal?" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(l: Closure, r: Closure) => throw new RuntimeException("Cannot compare function")
                          case List(l: Primitive, r: Primitive) => throw new RuntimeException("Cannot compare primitives")
                          case List(l: SExp, r: SExp) => if (l == r) STrue() else SFalse()
                          case _ => throw new RuntimeException("Failed comparison")
                        }
                      case "pair?" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SCons(head, tail)) => STrue()
                          case _ => SFalse()
                        }
                      case "null?" =>
                        e2.map(e => interpExp(e, env)) match {
                          case List(SNil) => STrue()
                          case _ => SFalse()
                        }
                      }
                        case _ => throw new RuntimeException("Something went horribly wrong")
                    }

                    case _ => throw new RuntimeException(
                      "tried to call a non-function: " + e1
                    )
                  }
            case exp: Exp => interpExp(exp, env) match {
              case Closure(params, body, boxEnv) =>
                interpExp(body, argsToEnv(params zip e2, boxEnv, env))
            }
            case _ => throw new RuntimeException("This reference does not exist: " + e1.toString)

                }
          }

        case Ref(id) => env.get(id) match {
          case None => throw new RuntimeException("unbound variable: " + id)
          case Some(box) => box.contents match{
            case Some(v) => v
            case _ => throw new RuntimeException("This variable was not found in the enviorment")
          }
        }
        case Let(id, rhs, body) => {
            val rhsVal = interpExp(rhs, env)
            val box = new Box[SExp]
            val contents = box.contents = Some(rhsVal)
            val newEnv = env + (id -> box)
            // newEnv = env ++ env1
            interpExp(body, newEnv)
          }
        case If(condition, lhs, rhs) => {
          val conVal = interpExp(condition, env)
          conVal match{
            case STrue() => interpExp(lhs, env)
            case SFalse() => interpExp(rhs, env)
            case _ => interpExp(lhs, env)
          }
        }


      case Quote(v) => {
        v
      }

      case Lambda(id, body) => Closure(id, body, env)


      case _ => throw new RuntimeException("Can only compare Int")

      }

    }


def evaluate (arg : List[Exp], env: Env) : List[SExp] = {
  arg match {
    case Nil => List()
    case first :: rest =>
      interpExp(first, env) :: evaluate(rest, env)
  }
}

def interpProgram(p : Program) : SExp = {
  p match{
    case Program(l, e) =>
    // This is an error at l in the interpExp
      interpExp(e, defToEnv(p, initialEnv()))
    case _ => throw new RuntimeException("Nil program reached")
  }
}

def initialEnv() : Env = {
  val plus = (new Box[SExp])
  plus.contents=(Some(Primitive("+")))
  val min = (new Box[SExp])
  min.contents=(Some(Primitive("-")))
  val div = (new Box[SExp])
  div.contents=(Some(Primitive("/")))
  val mult = (new Box[SExp])
  mult.contents=(Some(Primitive("*")))
  val greater = (new Box[SExp])
  greater.contents=(Some(Primitive(">")))
  val less = (new Box[SExp])
  less.contents=(Some(Primitive("<")))
  val greaterEqual = (new Box[SExp])
  greaterEqual.contents=(Some(Primitive(">=")))
  val lessEqual = (new Box[SExp])
  lessEqual.contents=(Some(Primitive("<=")))
  val cons = (new Box[SExp])
  cons.contents=(Some(Primitive("cons")))
  val car = (new Box[SExp])
  car.contents=(Some(Primitive("car")))
  val cdr = (new Box[SExp])
  cdr.contents=(Some(Primitive("cdr")))
  val equal = (new Box[SExp])
  equal.contents=(Some(Primitive("equal?")))
  val pair = (new Box[SExp])
  pair.contents=(Some(Primitive("pair?")))
  val nullHuh = (new Box[SExp])
  nullHuh.contents=(Some(Primitive("null?")))
  var env : Map[String, Box[SExp]] = Map("+" -> plus)
  env = env + ("-" -> min)
  env = env + ("/" -> div)
  env = env + ("*" -> mult)
  env = env + (">" -> greater)
  env = env + ("<" -> less)
  env = env + (">=" -> greaterEqual)
  env = env + ("<=" -> lessEqual)
  env = env + ("cons" -> cons)
  env = env + ("car" -> car)
  env = env + ("cdr" -> cdr)
  env = env + ("equal?" -> equal)
  env = env + ("pair?" -> pair)
  env = env + ("null?" -> nullHuh)
  env


  }

def argsToEnv(parToArgs: List[(String, Exp)], acc: Env, env: Env) : Env =
  parToArgs match {
    case Nil => acc
    case first :: rest =>{
      var box = new Box[SExp]
      box.contents = Some(interpExp(first._2, env))
      val newEnv = (first._1 -> box)
      argsToEnv(rest, acc + newEnv, env)
    }
  }
def initialDefs(p: Program, env: Env) : Env = {
  p.defs match {
    case Nil => env
    case Def(name, args, body) :: rest => {
      val newEnv = env + (name -> new Box[SExp])
      initialDefs(Program(rest, p.exp), newEnv)
    }
  }
}
def defToEnv(p: Program, env: Env) : Env ={
  defToEnvHelper(p.defs, initialDefs(p, env))
}
def defToEnvHelper(defs: List[SExp], env: Env) : Env ={
  defs match{
    case List() => env
    case Def(name, args, body) :: rest => env.get(name) match{
      case None => throw new RuntimeException("Your defines couldn't be add to the enviorment")
      case Some(v) => {
        v.contents = Some(Closure(args, body, env))
        defToEnvHelper(rest, env)
      }
    }
  }
}
def evalExp(e : String) : SExp =
  interpExp(parseExp(parseSExp(e)), initialEnv())

def evalProgram(p : String) : SExp =
  interpProgram(parseProgram(parseSExp(p)))


  // Arithmetic tests
  val arithEx1 = parseExp(parseSExp("(* (+ 5 2) 1)"))

  // Let tests
  val letEx1 = parseExp(parseSExp(
    """(let ((x 5))
         (+ x 7))"""))

// Contributers to this code were Aj, Ian, Bridger, Kristian, Zack
// With major help from Thomas







println(interpExp(letEx1, initialEnv()))
  assert(interpExp(letEx1, initialEnv()) == SInt(12))

 val letEx2 = parseExp(parseSExp(
    """(let ((x (+ 5 6)))
         (+ x 7))"""))
  assert(interpExp(letEx2, initialEnv()) == SInt(18))

  val letEx3 = parseExp(parseSExp(
    """(let ((x (let ((y 1))
                   (+ y 2))))
          (+ x 3))"""))
  assert(interpExp(letEx3, initialEnv()) == SInt(6))

  // Program parsing tests. We'll represent a program
  // as a sequence of defintions, followed by a main
  val progEx1 = parseProgram(parseSExp(
  """
  ((define (square n)
     (* n n))
   (square 5))
  """))
println(progEx1)

  // assert(progEx1 ==
  //   Program(List(
  //     Closure(List("n"), Multiply(Ref("n"), Ref("n")), Map())
  //   ),
  //   Call("square", List(Literal(SInt(5)))))
  // )

  val progEx2 = parseProgram(parseSExp(
  """
  ((define (square n)
     (* n n))
   (define (cube n)
     (* n (square n)))
   (cube 5))
  """))
/*
  // assert(progEx2 ==
  //   Program(List(
  //     Def("square", List("n"), Multiply(Ref("n"), Ref("n"))),
  //     Def("cube", List("n"), Multiply(Ref("n"), Call("square", List(Ref("n")))))
  //   ),
  //   Call("cube", List(Literal(SInt(5)))))
  // )

val greaterProgTest = interpExp(parseExp(parseSExp(
  """
    (> 5 6)
  """

)), Map())
assert(greaterProgTest == SFalse())
*/
val progEx3 = parseProgram(parseSExp(
  """
        ((define (merge l r)
                (if (null? l) r
                        (if (null? r) l
                                (if (< (car l) (car r))
                                        (cons (car l) (merge (cdr l) r))
                                        (cons (car r) (merge (cdr r) l))))))

        (define (split l)
                (cons (oddLength l) (cons (evenLength l) `())))

        (define (oddLength l)
                (if (null? l) '
                        (if (null? (cdr l)) (list (car l))
                                (cons (car l) (oddLength (cddr l))))))

        (define (evenLength l)
                (if (null? l) '()
                        (if (null? (cdr l)) '()
                                (cons (cadr l) (evenLength (cddr l))))))

        (define (mergesort l)
                (if (null? l) l
                        (if (null? (cdr l)) l
                                (merge
                                        (mergesort (car (split l)))
                                        (mergesort (cadr (split l))))))))
"""
))


// val BinaryToDecimal = parseProgram(parseSExp(
// """
//   ((define (append (l, s))
//    (if (null? l)
//      s
//      (cons (car l) (append (cdr l) s))))
//   (define (BinaryConverter (v))
//     (let x (cons null null)
//     (append (append x (% v 2)) (BinaryConverter((/ v 2))))))
//   (BinaryConverter (64)))
// """
// ))
//
// assert(progEx3 == Program(List(),Ref(define)))


// vim: set ts=2 sw=2 et:
