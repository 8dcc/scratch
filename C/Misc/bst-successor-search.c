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
    /* Invalid input */
    if (input == NULL)
        return NULL;

    /* If we have a node in the right, there is a group of keys greater than the
     * input. */
    if (input->right != NULL) {
        /* Return the smallest (leftmost) value in that group. */
        for (input = input->right; input->left != NULL; input = input->left)
            ;

        return input;
    }

    Node* parent = input->parent;

    /* If we came from the left, the parent has a greater key, so we can
     * immediately return it. If however, we came from the right, keep going up
     * until we came from the right.
     *
     * In other words, we have to keep going up until we come from the left
     * side. */
    while (parent->right == input) {
        /* We reached the top, no node was found */
        if (parent->parent == NULL)
            return NULL;

        input  = parent;
        parent = parent->parent;
    }

    return parent;
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
    /* Obviously a bad and tedious method of inserting, but short enough */
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

    printf("\nTests:\n");
    Node *solution, *input, *returned;

    input    = tree->left->right->left;
    solution = tree->left->right;
    returned = findInOrderSuccessor(input);
    printf("%2d -> %2d (%s)\n", input->key, (returned) ? returned->key : -1,
           (returned == solution) ? "ok" : "wrong");

    input    = tree->right;
    solution = NULL;
    returned = findInOrderSuccessor(input);
    printf("%2d -> %2d (%s)\n", input->key, (returned) ? returned->key : -1,
           (returned == solution) ? "ok" : "wrong");

    input    = tree;
    solution = tree->right;
    returned = findInOrderSuccessor(input);
    printf("%2d -> %2d (%s)\n", input->key, (returned) ? returned->key : -1,
           (returned == solution) ? "ok" : "wrong");

    input    = tree->left->left;
    solution = tree->left;
    returned = findInOrderSuccessor(input);
    printf("%2d -> %2d (%s)\n", input->key, (returned) ? returned->key : -1,
           (returned == solution) ? "ok" : "wrong");

    treeFree(tree);
    return 0;
}
