/* Kata: https://www.codewars.com/kata/57356c55867b9b7a60000bd7 */

int basic_op(char op, int v1, int v2) {
    switch(op) {
        case '+':   return v1+v2;
        case '-':   return v1-v2;
        case '*':   return v1*v2;
        case '/':   return v1/v2;
    }
}
