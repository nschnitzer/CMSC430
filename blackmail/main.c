#include <stdio.h>
#include <inttypes.h>

int64_t nice();

int main(int argc, char** argv) {
  int64_t val;
  val = nice();
  
  printf("%" PRId64 "\n", val);
  return 0;
}
