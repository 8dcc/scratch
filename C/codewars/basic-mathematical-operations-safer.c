/* Kata: https://www.codewars.com/kata/57356c55867b9b7a60000bd7 */
/* This one is safer but larger */

int basic_op(char op, int v1, int v2) {
    int r = 0;
    switch(op) {
        case '+':       r = v1+v2;      break;
        case '-':       r = v1-v2;      break;
        case '*':       r = v1*v2;      break;
        case '/':       r = v1/v2;      break;
        default:        break;
    }
    return r;
}
