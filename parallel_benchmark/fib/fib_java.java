class fib_java {

  public static int fib (int n) {
    if (n < 2) return n;
    return fib(n-2) + fib(n-1);
  }

  public static int iterFib(int n) {
    if (n < 2) return n;
    int y=0,x=1;
    for (int i=0; i <= n-2; i++) {
      int temp = x; x +=y; y=temp;
    }
    return x;
  }

  public static void main (String[] args) {
    System.out.println("answer: " + fib(Integer.parseInt(args[0])));
  }
}
