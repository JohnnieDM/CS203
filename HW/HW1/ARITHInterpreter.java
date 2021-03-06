/**
 * Program: ARITHInterpreter.java
 * Authors: Johnnie Chang and Wei-Lin Wu
 * On this homework, we worked together for 2 hours;
 * Johnnie worked independently for 1 hour,
 * and Wei-Lin worked independently for 1 hour.
 * Besides addition and multiplication,
 * we implemented other common operations, namely,
 * subtraction, division, exponentiation, factorial, and binomial coefficients,
 * for additional features.
 */

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
  public AST(AST left, int right, String type) {
    this.left = left;
    this.right = new AST(right);
    this.type = type;
  }
  public AST(int left, AST right, String type) {
    this.left = new AST(left);
    this.right = right;
    this.type = type;
  }
  public AST(int left, int right, String type) {
    this.left = new AST(left);
    this.right = new AST(right);
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
      case "-"     :
      case "SubExp": return "(" + this.left + " - " + this.right + ")";
      case "*"     :
      case "MulExp": return this.left + " * " + this.right;
      case "/"     :
      case "DivExp": return this.left + " / " + this.right;
      case "^"     :
      case "PowExp": return this.left + " ^ " + this.right;
      case "!"     :
      case "FacExp": return this.left + "!";
      case "C"     :
      case "BinExp": return "bin(" + this.left + ", " + this.right + ")";
    }
    return "";
  }
}

public class ARITHInterpreter {
  public static void main(String[] args) {
    // 8 ^ 7 - 6! + C(5 * 3,  4 / 2 - 1)
    AST test1 = new AST(new AST(new AST(8, 7, "^"), new AST(6, 0, "!"), "-"), new AST(new AST(5, 3, "*"), new AST(new AST(4, 2, "/"), 1, "-"), "C"), "+");
    out.println(test1);
    out.println(2096447 == eval(test1));

    // (2! ^ 4) * C(7+1-5, 6/3)
    AST test2 = new AST(new AST(new AST(2, 0, "!"), 4, "^"), new AST(new AST(7, new AST(1, 5, "-"), "+"), new AST(6, 3, "/"), "C"), "*");
    out.println(test2);
    out.println(48 == eval(test2));
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
      case "FacExp": return factorial(eval(tree.left));
    }

    //TODO handle exception
    return 0;
  }

  /** Compute x^a recursively. */
  private static int power(int x, int a) {
    if (a == 0) return 1;
    if (a == 1) return x;
    int half = power(x, a/2);
    if (a % 2 == 0) return half * half;
    else return x * half * half;
  }

  /** Compute bin(n, k) recursively. */
  private static int binomial(int n, int k) {
    // Bad arguments
    if ((n < k) || (n < 0 || k < 0)) return -1;

    if ((n == k) || (k == 0))
      return 1;
    else
      return binomial(n - 1, k) + binomial(n - 1, k - 1);
  }

  /** Compute n! in a loop. */
  private static int factorial(int n) {
    if (n < 0) return 0;
    if (n == 0 || n == 1) return 1;
    return n * factorial(n-1);
  }
}
