
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TranslatorApiTest extends FunSuite {
    
    import Translator._
    import Tokenizer._
    
    test("Symbol class test") {
        assert(Symbol("def").toString == "'def")
    }
    
    test("class Tokenizer test success") {
        val code: String = "(val v 1)"
        val tk: Tokenizer = Tokenizer(code)
        assert(tk.hasNext)
        assert(tk.next == "(")
        assert(tk.hasNext)
        assert(tk.next == "val")
        assert(tk.hasNext)
        assert(tk.next == "v")
        assert(tk.hasNext)
        assert(tk.next == "1")
        assert(tk.hasNext)
        assert(tk.next == ")")
        assert(!tk.hasNext)
    }
    
    test("class Tokenizer test failed") {
        val code: String = "(val v 1)"
        val tk: Tokenizer = Tokenizer(code)
        assert(tk.hasNext)
        assert(tk.next == "(")
        assert(tk.hasNext)
        assert(tk.next == "val")
        assert(tk.hasNext)
        assert(tk.next == "v")
        assert(tk.hasNext)
        assert(tk.next == "1")
        assert(tk.hasNext)
        assert(tk.next == ")")
        assert(!tk.hasNext)
        intercept[TokenizerException] {
            tk.next
        }
    }
    
    /**
     * 测试字符串到链表的转换
     */
    test("object Tokenizer test: string2list") {
        val code: String = "(def v (lambda (x) (+ x 1)))"
        
        val tkCode = List(Symbol("def"), Symbol("v"), List(Symbol("lambda"),
            List(Symbol("x")), List(Symbol("+"), Symbol("x"), 1)))
        
        assert(string2list(code) == tkCode)
    }
    
    /**
     * 测试链表到字符串的转换
     */
    test("object Tokenizer test: list2string") {
        val code: String = "(def v (lambda (x y) (* (+ x 1) y)))"
        
        val tkCode = List(Symbol("def"), Symbol("v"), List(Symbol("lambda"), 
            List(Symbol("x"), Symbol("y")), List(Symbol("*"), List(Symbol("+"),
            Symbol("x"), 1), Symbol("y")) ))
        
        assert(list2string(tkCode) == code)
    }
    
    /**
     * 完备性测试
     */
    test("object Tokenizer test: full function") {
        val code: String = "(def v (lambda (x y) (+ x y)))"
        assert(list2string(string2list(code)) == code)
        
        val tkCode = List(Symbol("def"), Symbol("v"), List(Symbol("lambda"),
            List(Symbol("x"), Symbol("y")), List(Symbol("+"), Symbol("x"), Symbol("y"))))
        
        assert(string2list(list2string(tkCode)) == tkCode)
    }
}