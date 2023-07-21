#include <stdio.h>

typedef struct Person {
    const char* name;
    int age;
} person_t;

person_t get_person(int age) {
    age += 5;
    return (person_t){ "Unknown", age };
}

// Used to test declaration of structs in function parameters
int get_age(person_t person) {
    return person.age;
}

int main() {
    person_t foo    = { "Foo", 69 };
    person_t* foo_p = &foo;

    printf("Name:  %s | Age:  %d\n", foo.name, foo.age);
    printf("pName: %s | pAge: %d\n", foo_p->name, foo_p->age);
    foo_p->age = 420;
    printf("pName: %s | pAge: %d\n", foo_p->name, foo_p->age);
    (&foo)->age = 1337;
    printf("pName: %s | pAge: %d\n", foo_p->name, foo_p->age);
    (*foo_p).age = 6969;
    printf("pName: %s | pAge: %d\n", (*foo_p).name, (*foo_p).age);

    person_t bar = get_person(10);
    printf("\nName:  %s | Age:  %d\n", bar.name, bar.age);

    int age_test = get_age((person_t){ "Peter", 123 });
    printf("Age test: %d\n", age_test);

    printf("\nTesting dereferences:\n");
    person_t bar2 = { "Bar", 55 };
    person_t tmp  = *foo_p;
    *foo_p        = bar2;
    printf("pName: %s | pAge: %d\n", foo_p->name, foo_p->age);
    *foo_p = tmp;
    printf("pName: %s | pAge: %d\n", foo_p->name, foo_p->age);

    return 0;
}
