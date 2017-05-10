def fib(int x): int {
    if (x < 2) {
        return 1;
    } else {
        return fib(x - 1) + fib(x - 2);
    }
}

  print_int(fib(0));
  print_int(fib(1));
  print_int(fib(2));
  print_int(fib(3));
  print_int(fib(4));
  print_int(fib(5));