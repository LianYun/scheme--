
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EvaluatorApiTest extends FunSuite {
    
    import Evaluator._
    import Translator.Tokenizer._
    
    test("Environment: emptyEnvironment") {
        val env = Environment().extend("a", 1).extend("b", "d")
        
        assert(env.toString == "Env: (a, 1) (b, d)")
    }
    
    
    test("evaluete test: program factorial") {
        def factDef =
        """def factorial (lambda (n)
             (if (= n 0)
                 1
                 (* n (factorial (- n 1)))))
        """
        val code = "(" + factDef + "(factorial 4))"
        
        val result = evaluate(code)
        
        assert(result == "24")
    }
    
    test("evaluete test: program fabnacci") {
        def factDef =
        """def fabnacci (lambda (n)
             (if (= n 0)
                 1
                 (if (= n 1)
                     1                
                     (+ (fabnacci (- n 1)) (fabnacci (- n 2)) ) )))
        """
        val code = "(" + factDef + "(fabnacci 4))"
        
        val result = evaluate(code)
        
        assert(result == "5")
    }
    
    test("evaluate test: cons cdr car") {
        assert(evaluate("(cons 1 nil)") == "(1)")
        assert(evaluate("(cons 1 (cons 2 nil))") == "(1 2)")
        assert(evaluate("(car (cons 3 (cons 1 (cons 2 nil))))") == "3")
        assert(evaluate("(cdr (cons 3 (cons 1 (cons 2 nil))))") == "(1 2)")
    }
    
    test("evaluate test: cons cdr car val") {
        val prog = """
            (val list (cons 1 (cons 2 (cons 3 (cons 4 nil))))
                (cdr list))"""
        assert(evaluate(prog) == "(2 3 4)")
    }
    
    test("evaluate test: val, cdr car cons def lambda") {
        val code = """
              (val list (cons 3 (cons 2 (cons 1 nil)))
                (def map (lambda (list func) 
                                 (if (= (null? list) 1)
                                     nil
                                     (cons (func (car list)) (map (cdr list) func) ) ) )
                   (def sum1 (lambda (x) (+ x 1))
                     (map list sum1))))))"""
        
        val result = evaluate(code)
        
        assert(result == "(4 3 2)")
    }   

}