/*
 * Test higher order functions.
 */
public class HigherOrder1 {


  /*
   * Add x to the absolute value of y (in a really dumb way).
   * The array argument is useless.
   */
  public int psuedo_add(int x, int y, int[] array)
  {
    int z = 0;
    int y_hack = y;

    if (y < 0) {
      y_hack = -y;
    }

    int i = 0;
    for (i = 0; i < y_hack; i++) {
      z += 1;
    }

    z += x;
    return z;
  }


  /*
   * The array should have length at least 2, and i should be positive.
   */
  public int g(int ret_arg1)(int i, int[] array)
  {
    // Create some assignments.

    int useless_val = 0;
    array[0] = 0;
    array[1] = 1;

    // Build the function we are supposed to return.
    // This one adds 10 to its argument.

    int return_fun(int c)
    {
      int j = 23;
      useless_val = -5;
      array[0] = 117; // a good math class
      array[1] = 116; // another good math class

      c = psuedo_add(c, -10, array);
      println("23 = " + j);
      return c;
    }

    // Do some other random things.

    void bogofun(String dummy_argument)
    {
      int b = 0;
      for (b = 0; b < i; b++) {
        array[0]++;
        array[1]++;
        useless_val++;
      }
      b += 5890; // just for fun.
    }

    bogofun("hi world!");

    if (array[0] == i && array[1] == i + 1) {
      println("good so far");
    } else {
      println("SOMETHING WENT WRONG!!!!");
    }

    array[0] = 134;
    array[1] = 141;
    println("134 = " + array[0] + "; 141 = " + array[1]);
    println("i = " + i + " = useless_val = " + useless_val);
    return return_fun;
  }


  /*
   * Main function.
   */
  public static void main(String[] argv)
  {
    int i = -2;
    int x = 1, y = 2;
    int[] array = new int[2];
    array[0] = 8;
    array[1] = 9;
    println("8 = " + array[0] + "; 9 = " + array[1]);

    int f(int k) = g(x+y, array);

    array[0] = 51;  // painful EE/CS class... (-:
    array[1] = 52;  // painful EE/CS class... (-:

    println("51 = " + array[0] + "; 52 = " + array[1]);
    println("i = -2 = " + i);
    println("33 = " + f(23));
    println("117 = " + array[0] + "; 116 = " + array[1]);
  }

}
// end of class: HigherOrder1
