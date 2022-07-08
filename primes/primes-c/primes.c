#include <stdbool.h>
#include <stdio.h>

typedef unsigned long nt;

nt sqrt_nt(nt n) {
  nt x0 = n / 2; // Initial estimate
                 // Avoid overflow when s is the maximum representable value

  // Sanity check
  if (x0 != 0) {
    nt x1 = (x0 + n / x0) / 2;

    while (x1 < x0) {
      x0 = x1;
      x1 = (x0 + n / x0) / 2;
    }
    return x0 + 1;
  } else
    return n + 1;
}

bool isPrime(nt n) {
  int i = 0;
  if (n % 2 == 0)
    return false;
  for (i = 3; i <= sqrt_nt(n); i += 2) {
    if (n % i == 0)
      return false;
  }
  return true;
}

int main() {
  int i = 1;
  for (i = 1; i < 200000; i++) {
    if (isPrime((nt)i)) {
      printf("%d\n", i);
    }
  }
  return 0;
}
