
#include <stdio.h>

// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "/opt/riscv/bin/riscv32-unknown-elf-gcc -O0"
// rmsbolt-disassemble: nil
// End:


int mul(int a, int b) {
  int c = 0;
  int ii = 0;
  for(int ii = 0; ii < a; ii++){
    c += b;
  }
  return c;
}

int square(int a){
  return mul(a, a);
}

int main() {
  int a = 3;
  int b = 0xFFFFFFFE;
  int c = 0x7FFFFFFF;
  int d = 1234;

  if(square(a+b) > square(c + d))
    return a;
  else
    return c;
}
