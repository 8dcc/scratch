#include <stdio.h>

struct Vector {
    float x, y, z;
};

struct studiohdr_t {
    int id;
    int version;
    int checksum;

    inline const char* pszName(void) const { return "Hello from pszName!\n"; }

    char name[64];
    int length;
    Vector eyeposition;
    Vector illumposition;
    Vector hull_min;
    Vector hull_max;
    Vector view_bbmin;
    Vector view_bbmax;
    int flags;
    int numbones;
    int boneindex;

    inline void* pBone(int i) const {
        printf("i: %d\n", i);
        return (void*)0xDEADBEEF;
    };

    int RemapSeqBone(int iSequence, int iLocalBone) const;
};

int main() {
    studiohdr_t test = {
        .id            = 123,
        .version       = 123,
        .checksum      = 123,
        .name          = "",
        .length        = 123,
        .eyeposition   = { 0, 0, 0 },
        .illumposition = { 0, 0, 0 },
        .hull_min      = { 0, 0, 0 },
        .hull_max      = { 0, 0, 0 },
        .view_bbmin    = { 0, 0, 0 },
        .view_bbmax    = { 0, 0, 0 },
        .flags         = 123,
        .numbones      = 123,
        .boneindex     = 123,
    };

    test.pszName();
    test.boneindex = 123;
    test.pBone(1337);

    return 0;
}
