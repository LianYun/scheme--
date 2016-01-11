
object Translator {

    /**
     *
     * 定义基本的数据结构
     * 定义简单的函数，将文本转换为scala内部数据结构表示
     * 为求值做好准备。
     */

    type Data = Any

    case class Symbol(name: String) {
        override def toString() = "'" + name
        def getName = name
    }
    
    case class TokenizerException(why: String) extends Exception(why)

    class Tokenizer(s: String) extends Iterator[String] {

        private var i = 0
        
        /**
        * 是否是分隔符号
        */
        private def isDelimiter(ch: Char) = ch <= ' ' || ch == '(' || ch == ')' // ' ' 在ascii中为32，小于这个数值的char都认为是分隔符。
        def hasNext: Boolean = {
            while (i < s.length() && s.charAt(i) <= ' ') { i = i + 1}
            i < s.length()
        }

        def next: String = {
            if(hasNext) {
                val start = i
                var ch = s.charAt(i); i = i + 1
                if (ch == '(') "("
                else if (ch == ')') ")"
                else {
                    while (i < s.length() && !isDelimiter(s.charAt(i))){i = i + 1}
                    s.substring(start, i)
                }
            } else {
                throw TokenizerException("more input expected")
            }
        }
    }
    //使用简单的方法即可解析Lisp的语法

    object Tokenizer {
        def apply(context: String) = new Tokenizer(context)
    
        def string2list(s: String): Data = {
            val it = Tokenizer(s)
            def parseExpr(token: String): Data = {
                if (token == "(") parseList
                else if (token == ")") throw TokenizerException("unmatched parenthesis")
                else if (token.charAt(0).isDigit) token.toInt
                else if (token.charAt(0) == '\"' && token.charAt(token.length()-1)=='\"')
                    token.substring(1, token.length - 1)
                else Symbol(token)

            }
            def parseList: List[Data] = {
                val token = it.next
                if (token == ")") Nil else parseExpr(token) :: parseList
            }
            parseExpr(it.next)
        }

        def list2string(expr: Data): String = {
            def convExpr(expr: Data): String = expr match {
                case l: List[_] => "(" + convList(l) + ")"
                case x: Int => x.toString
                case Symbol(token) => token
            }

            def convList(l: List[_]): String = l match {
                case Nil => ""
                case x :: Nil => convExpr(x)
                case x :: xs => convExpr(x) + " " + convList(xs)
            }
            convExpr(expr)
        }
    }
}
