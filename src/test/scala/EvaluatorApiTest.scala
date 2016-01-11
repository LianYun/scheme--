
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EvaluatorApiTest extends FunSuite {
    
    import Evaluator._
    
    test("Environment: emptyEnvironment") {
        val env = Environment().extend("a", 1).extend("b", "d")
        
        assert(env.toString == "Env: (a, 1) (b, d)")
    }
    
}