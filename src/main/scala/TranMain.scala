import Translator._
import Evaluator._

object TranTest extends App {
    
    import Tokenizer._

    println("Interpreter test")


    def factDef =
        """def factorial (lambda (n)
             (if (= n 0)
                 1
                 (* n (factorial (- n 1)))))
        """
    val code = "(" + factDef + "(factorial 4))"
    println("result: " + evaluate(code))
}
