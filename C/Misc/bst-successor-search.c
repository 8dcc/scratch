#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * BST Successor Search.
 * ---------------------
 *
 * In a Binary Search Tree (BST), an Inorder Successor of a node is defined as
 * the node with the smallest key greater than the key of the input node (see
 * examples below). Given a node `inputNode' in a BST, you're asked to write a
 * function `findInOrderSuccessor' that returns the Inorder Successor of
 * `inputNode'. If `inputNode' has no Inorder Successor, return `null'.
 *
 * Explain your solution and analyze its time and space complexities.
 *
 * For example:
 *
 *                        <20>
 *                        /  \
 *                      <9>  <25>
 *                      / \
 *                    <5> <12>
 *                        /  \
 *                     <11>  <14>
 *
 * In this diagram, the Inorder Successor of 9 is 11, and the Inorder Successor
 * of 14 is 20.
 */

typedef struct Node {
    int key;
    struct Node* parent;
    struct Node* left;
    struct Node* right;
} Node;

Node* findInOrderSuccessor(Node* input) {
    /* TODO */
    return input;
}

/*----------------------------------------------------------------------------*/
/* Setup for the exercise */

static Node* newNode(Node* parent, bool left, int key) {
    Node* ret   = malloc(sizeof(Node));
    ret->key    = key;
    ret->parent = parent;
    ret->left   = NULL;
    ret->right  = NULL;

    if (parent != NULL) {
        if (left)
            parent->left = ret;
        else
            parent->right = ret;
    }

    return ret;
}

static Node* treeCreate(void) {
    Node* node20 = newNode(NULL, false, 20);
    Node* node9  = newNode(node20, true, 9);
    newNode(node20, false, 25);
    newNode(node9, true, 5);
    Node* node12 = newNode(node9, false, 12);
    newNode(node12, true, 11);
    newNode(node12, false, 14);

    return node20;
}

static void treeFree(Node* tree) {
    if (tree == NULL)
        return;

    if (tree->right != NULL)
        treeFree(tree->right);

    if (tree->left != NULL)
        treeFree(tree->left);

    free(tree);
}

static void treePrint(Node* tree) {
    static int indent      = 0;
    static int indent_step = 3;

    if (tree == NULL)
        return;

    for (int i = 0; i < indent; i++)
        putchar(' ');

    printf("%d\n", tree->key);

    indent += indent_step;

    if (tree->right != NULL)
        treePrint(tree->right);

    if (tree->left != NULL)
        treePrint(tree->left);

    indent -= indent_step;
}

int main(void) {
    Node* tree = treeCreate();

    puts("Printing tree:");
    treePrint(tree);

    treeFree(tree);
    return 0;
}
