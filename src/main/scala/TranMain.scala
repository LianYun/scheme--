import Translator._
import Evaluator._

object TranTest extends App {
    
    import Tokenizer._

    println("scheme-- REPL version 0.1.1")
    val replEnv = globalEnv
    
    var line = readLine(">>")
    
    while(line != "q") {
        val list = string2list(line)
        val v = eval(list, globalEnv)
        println(s": $v")
        line = readLine(">>")
    }
}
