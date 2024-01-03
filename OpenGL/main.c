
#include <stdbool.h>
#include <stdio.h>
#include "include/glad/glad.h"
#include <GLFW/glfw3.h>

#define MY_WIDTH  640
#define MY_HEIGHT 480

/* Every time the screen size changes, this function will be called */
static void framebuffer_size_callback(GLFWwindow* window, int w, int h) {
    /* Unused */
    (void)window;

    /* We set the OpenGL viewport to specify what is the new area for rendering
     * (in this case the entire window). */
    glViewport(0, 0, w, h);
}

static void process_input(GLFWwindow* window) {
    /* Close window when pressing ESC */
    if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
        glfwSetWindowShouldClose(window, true);
}

/*----------------------------------------------------------------------------*/

int main() {
    glfwInit();

    /* See https://www.glfw.org/docs/latest/window.html#window_hints */
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    /* Create window object using GLFW */
    GLFWwindow* window =
      glfwCreateWindow(MY_WIDTH, MY_HEIGHT, "Test window", NULL, NULL);
    if (!window) {
        fprintf(stderr, "Failed to create GLFW window\n");
        glfwTerminate();
        return 1;
    }
    glfwMakeContextCurrent(window);

    /* Initialize GLAD, for managing the OpenGL function pointers. Instead of
     * passing the OS-secific function for finding symbols (e.g. `dlsym' in
     * linux), we pass `glfwGetProcAddress', which does the job for us. */
    if (!gladLoadGLLoader((GLADloadproc)(glfwGetProcAddress))) {
        fprintf(stderr, "Failed to initialize glad\n");
        return 1;
    }

    /* Register our own callback function that will be called when the window
     * size changes. */
    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    /* Main render loop */
    while (!glfwWindowShouldClose(window)) {
        /* Process the user input */
        process_input(window);

        /* Render. Uses double buffering to avoid tearing. */
        glfwSwapBuffers(window);

        glfwPollEvents();
    }

    /* GLFW cleanup */
    glfwTerminate();
    return 0;
}
