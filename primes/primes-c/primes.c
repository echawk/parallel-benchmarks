#include <stdbool.h>
#include <stdio.h>

typedef unsigned long nt;

/* TODO: add sqrt optimization */
/* nt sqrt_nt(nt val) { */
/*   int i = 0; */

/*   for (i = 3; i < val; i += 2) { */
/*   } */
/* } */

bool isPrime(nt n) {
  int i = 0;
  if (n % 2 == 0)
    return false;
  for (i = 3; i < (n - 1); i += 2) {
    if (n % i == 0)
      return false;
  }
  return true;
}

int main() {
  int i = 1;
  for (i = 1; i < 20; i++) {
    printf("%d is prime? %s\n", i, isPrime((nt)i) ? "true" : "false");
  }
  return 0;
}
