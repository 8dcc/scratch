#include <stdio.h>

int main() {
    typedef struct Person {
        const char* name;
        int age;
    } person;

    person foo = { "Foo", 69 };
    person* foo_p = &foo;

    printf("Name:  %s | Age:  %d\n", foo.name, foo.age);
    printf("PName: %s | PAge: %d\n", foo_p->name, foo_p->age);
    foo_p->age = 420;
    printf("PName: %s | PAge: %d\n", foo_p->name, foo_p->age);
    (&foo)->age = 1337;
    printf("PName: %s | PAge: %d\n", foo_p->name, foo_p->age);
    (*foo_p).age = 6969;
    printf("PName: %s | PAge: %d\n", (*foo_p).name, (*foo_p).age);

    return 0;
}
