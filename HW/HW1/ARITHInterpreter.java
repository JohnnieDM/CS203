import static java.lang.System.out;

/** Abstract Syntax Tree */
class AST {
  public AST left;
  public AST right;
  public String type;
  public int data;

  public AST(AST left, AST right, String type) {
    this.left = left;
    this.right = right;
    this.type = type;
  }

  public AST(int data) {
    this.type = "IntExp";
    this.data = data;
  }

  public String toString() {
    switch (this.type) {
      case "i"     :
      case "IntExp": return "" + this.data;
      case "+"     :
      case "SumExp": return "(" + this.left + " + " + this.right + ")";
      case "*":
      case "MulExp": return this.left + " * " + this.right;
      case "!":
      case "FacExp": AST node = this.left == null? this.right : this.left;
                     return node + "!";
      case "C":
      case "BinExp": return "bin(" + this.left + ", " + this.right + ")";
    }
    return "";
  }
}

public class ARITHInterpreter {
  public static void main(String[] args) {
    int i1 = new Integer(args[0]);
    int i2 = new Integer(args[1]);
    int out = new Integer(args[2]);
    // Input
    AST t1 = new AST(i1);
    AST t2 = new AST(i2);
    test(i1, 0, "i", 1, i1);
    test(i2, 0, "i", 2, i2);

    

    // i1 + i2
    AST t3 = test(i1, i2, "+", 3, i1+i2);
    // i1 * i2
    AST t4 = test(i1, i2, "*", 4, i1*i2);
    // i1 + i1 * i2
    AST t5 = test(i1, t4, "+", 5, i1+i1*i2);
    // i1 * i2 + i2
    AST t6 = test(t4, t2, "+", 6, i1*i2+i2);

    // (((i2 + i2) * i2) + i2)
    AST tree7 = new AST(new AST(new AST(t2, t2, "+"), t2, "*"), t2, "+");
    out.println("Test 7: " + tree7);
    out.println((((i2+i2)*i2)+i2) == eval(tree7));
    out.println();

    // i2!
    test(null, i2, "!", 8, factorial(i2));

    // bin(5, i2)
    test(5, i2, "C", 9, binomial(5, i2));
  }

  public static AST test(int i1, int i2, String opr, int testNum, int answer) {
    // For IntExp, ignore i2.
    AST t;
    if (opr == "i" || opr == "IntExp") t = new AST(i1);
    else t = new AST(new AST(i1), new AST(i2), opr);
    out.println("Test " + testNum + ": " + t);
    out.println(answer == eval(t));
    out.println();
    return t;
  }

  public static AST test(AST t1, AST t2, String opr, int testNum, int answer) {
    AST t = new AST(t1, t2, opr);
    out.println("Test " + testNum + ": " + t);
    out.println(answer == eval(t));
    out.println();
    return t;
  }

  public static AST test(int i1, AST t2, String opr, int testNum, int answer) {
    AST t = new AST(new AST(i1), t2, opr);
    out.println("Test " + testNum + ": " + t);
    out.println(answer == eval(t));
    out.println();
    return t;
  }

  public static AST test(AST t1, int i2, String opr, int testNum, int answer) {
    AST t = new AST(t1, new AST(i2), opr);
    out.println("Test " + testNum + ": " + t);
    out.println(answer == eval(t));
    out.println();
    return t;
  }

  /** Evaluate expression tree.
   *  8 expressions: int, sum, subtract, multiply, binomial, factorial, division, exponential.
   */
  public static int eval(AST tree) {
    switch (tree.type) {
      case "i"     :
      case "IntExp": return tree.data;
      case "+"     :
      case "SumExp": return eval(tree.left) + eval(tree.right);
      case "-"     :
      case "SubExp": return eval(tree.left) - eval(tree.right);
      case "^"     :
      case "PowExp": return power(eval(tree.left), eval(tree.right));
      case "*"     :
      case "MulExp": return eval(tree.left) * eval(tree.right);
      case "/"     :
      case "DivExp": return eval(tree.left) / eval(tree.right);
      case "C"     :
      case "BinExp": return binomial(eval(tree.left), eval(tree.right));
      case "!"     :
      case "FacExp": AST node = tree.left == null ? tree.right : tree.left;
                     return factorial(eval(node));
    }
    //TODO handle exception
    return 0;
  }

  /** Compute x^a recursively. */
  private static int power(int x, int a) {
    if (a == 0) return 1;
    if (a == 1) return x;
    int half = power(x, a/2);
    if (x % 2 == 0) return half * half;
    else return x * half * half;
  }

  /** Compute bin(n, k) recursively. */
  private static int binomial(int n, int k) {
    if ((n < k) || (n < 0 || k < 0)) return 0;
    if ((n == k) || (k == 0))
      return 1;
    else
      return binomial(n - 1, k) + binomial(n - 1, k - 1);
  }

  /** Compute n! in a loop. */
  private static int factorial(int n) {
    if (n < 0) return 0;
    if (n == 0) return 1;
    int f = n;
    for (int i = n-1; i > 1; i--) f *= i;
    return f;
  }
}
