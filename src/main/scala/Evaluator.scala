
object Evaluator {

    import Translator._
    import Translator.Tokenizer._

    case class EvaluatorException(why: String) extends Exception(why)
    
    
    abstract class Environment {

        def lookup(n: String): Data
        
        def extendMulti(ps: List[String], vs: List[Data]): Environment = (ps, vs) match {
            case (List(), List()) => this
            case (p :: ps1, arg :: args1) =>
                extend(p, arg).extendMulti(ps1, args1)
            case _ => throw EvaluatorException("wrong number of arguments")
        }

        def extendRec(name: String, expr: Environment => Data) = new Environment {
            def lookup(n: String): Data =
                if (n == name) expr(this) else Environment.this.lookup(n)

            override def toString = {
                Environment.this.toString + " " + "(" + name + ", " + expr(this) + ")"
            }
        }

        def extend(name: String, v: Data) = extendRec(name, env1 => v)
        
        override def toString = "Env:"
        
    }
    
    object Environment {
    
        val emptyEnvironment = new Environment {
            def lookup(n: String): Data = throw EvaluatorException("undefined: " + n)
        }
    
        def apply() = emptyEnvironment
    }
    
    val globalEnv = Environment()
        .extend("=", Lambda {
            case List(arg1, arg2) => if (arg1 == arg2) 1 else 0
        }).extend("+", Lambda {
            case List(arg1: Int, arg2: Int) => arg1 + arg2
            case List(arg1: String, arg2: String) => arg1 + arg2
        }).extend("-", Lambda {
            case List(arg1: Int, arg2: Int) => arg1 - arg2
        }).extend("*", Lambda {
            case List(arg1: Int, arg2: Int) => arg1 * arg2
        }).extend("/", Lambda {
            case List(arg1: Int, arg2: Int) => arg1 /arg2
        }).extend("nil", Nil).extend("cons", Lambda {
            case List(arg1, arg2) => arg1 :: asList(arg2)
        }).extend("car", Lambda {
            case List(x :: xs) => x
        }).extend("cdr", Lambda {
            case List(x :: xs) => xs
        }).extend("null?", Lambda {
            case List(Nil) => 1
            case _ => 0
        })

    private case class Lambda(f: List[Data] => Data)
    
    private def mkLambda(ps: List[String], body: Data, env: Environment) =
        Lambda{ args => eval(body, env.extendMulti(ps, args))}
        
        
    private def getSymbolName(symbol: Data): String = symbol match {
        case Symbol(name) => name
        case _ => throw EvaluatorException("Not a Symbol")
    }

    def apply(fn: Data, args: List[Data]): Data = fn match {
        case Lambda(f) => f(args)
        case _ => throw EvaluatorException("application of non-function: " + fn + " to " + "args")
    }

    private def asList(x: Data): List[Data] = x match {
        case xs: List[_] => xs
        case _ => throw EvaluatorException("makformed list: " + x)
    }

    def eval(x: Data, env: Environment): Data = x match {
        case _: String => x
        case _: Int => x
        case Symbol(name) => env lookup name
        case Symbol("val") :: Symbol(name) :: expr :: rest :: Nil =>
            eval(rest, env.extend(name, eval(expr, env)))
        case Symbol("def") :: Symbol(name) :: expr :: rest :: Nil =>
            eval(rest, env.extendRec(name, env1 => eval(expr, env1)))
        case Symbol("if") :: cond :: thenpart :: elsepart :: Nil =>
            if (eval(cond, env) != 0) eval(thenpart, env)
            else eval(elsepart, env)
        case Symbol("quote") :: y :: Nil => y
        case Symbol("lambda") :: params :: body :: Nil =>
            mkLambda((asList(params).map(getSymbolName)), body, env)
        case operator :: operands =>
            apply(eval(operator, env), operands map (x => eval(x, env)))
    }

    def evaluate(x: Data): Data = eval(x, globalEnv)

    def evaluate(s: String): Data = list2string(evaluate(string2list(s)))
}
