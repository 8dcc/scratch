/* Kata: https://www.codewars.com/kata/52fba66badcd10859f00097e */

char *disemvowel(const char *str)
{
  char *output = malloc(sizeof(char) * strlen(str) + 1);;
  int output_pos = 0;  // Used to count the character position
  
  for (int n = 0; str[n] != '\0'; n++) {
    if (str[n] != 'a' && str[n] != 'e' && str[n] != 'i' && str[n] != 'o' && str[n] != 'u' &&
       str[n] != 'A' && str[n] != 'E' && str[n] != 'I' && str[n] != 'O' && str[n] != 'U') {
      output[output_pos] = str[n];
      output_pos++;
    }
  }
  output[output_pos] = '\0';
  return output;
}
