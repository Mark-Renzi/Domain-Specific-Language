#include "mylib"
int32_t x = 5;
int flag = 1;
uint32_t count = 42;
char * a = "forty-two";
a = "zero";
float PI = 3.1415;
uint8_t myFunc(uint32_t price, char * item) {
uint32_t amt = price;
return amt;
}

count = 1;
uint8_t ret = myFunc(count, a);
char * b = "one";
char * c = "two";
if (a == b) {
a = c;
} else if (0) {
b = a;
a = b;
} else {
a = a;
} 
c[42] = a;


while (flag) {
count = count + 1;
if (count == 42) {
flag = 0;
} else {
flag = 1;
} 
}
for (uint8_t i = 0; i < 10; i = i + 1) {
count = count + 1;
if (count == 42) {
flag = 0;
} else {
flag = 1;
} 
}