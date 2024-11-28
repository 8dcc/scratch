/*
 * FIXME: This program has problems when compiling (e.g. try '-Wpedantic') and
 * when executing (e.g. try resizing the window fast).
 */

#include <stdbool.h>
#include <stdio.h>
#include "deps/glad.h"
#include <GLFW/glfw3.h>

#define COMPILATION_CHECK

#define MY_WIDTH  640
#define MY_HEIGHT 480

/* TODO: Read from file */
static const char* vertex_shader_source =
  "#version 330 core\n"
  "layout (location = 0) in vec3 aPos;\n"
  "void main()\n"
  "{\n"
  "   gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n"
  "}";

static const char* fragment_shader_source =
  "#version 330 core\n"
  "out vec4 FragColor;\n"
  "void main()\n"
  "{\n"
  "    FragColor = vec4(0.1f, 1.0f, 0.1f, 1.0f);\n"
  "}";

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

    /*--------------------------------------------------------------------*/
    /* Shaders */

    /* Create a shader object for the vertex (position) */
    uint32_t vertex_shader = glCreateShader(GL_VERTEX_SHADER);

    /* Attach the shader source to the shader object and compile it */
    glShaderSource(vertex_shader, 1, &vertex_shader_source, NULL);
    glCompileShader(vertex_shader);

#ifdef COMPILATION_CHECK
    int success;
    char info_log[512];
    glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &success);

    if (!success) {
        glGetShaderInfoLog(vertex_shader, 512, NULL, info_log);
        printf("Vertex shader compilation failed\n"
               "%s\n",
               info_log);
    }
#endif

    /* Now the fragment shader (color) */
    uint32_t fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragment_shader, 1, &fragment_shader_source, NULL);
    glCompileShader(fragment_shader);

#ifdef COMPILATION_CHECK
    glGetShaderiv(fragment_shader, GL_COMPILE_STATUS, &success);

    if (!success) {
        glGetShaderInfoLog(fragment_shader, 512, NULL, info_log);
        printf("Fragment shader compilation failed\n"
               "%s\n",
               info_log);
    }
#endif

    /* Now we link those shaders into a Shader Program */
    uint32_t shader_program = glCreateProgram();
    glAttachShader(shader_program, vertex_shader);
    glAttachShader(shader_program, fragment_shader);
    glLinkProgram(shader_program);

#ifdef COMPILATION_CHECK
    glGetProgramiv(shader_program, GL_LINK_STATUS, &success);
    if (!success) {
        glGetProgramInfoLog(shader_program, 512, NULL, info_log);
        printf("Program linking failed\n"
               "%s\n",
               info_log);
    }
#endif

    /* Now that we linked our program, we can delete the shaders */
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    /*--------------------------------------------------------------------*/
    /* VBO and VAO */

    /* For drawing a triangle, we specify the 3 vertices in X,Y,Z positions
     * from -1.f to 1.f */
    float vertices[][3] = {
        { -0.5f, -0.5f, 0.0f },
        { 0.5f, -0.5f, 0.0f },
        { 0.0f, 0.5f, 0.0f },
    };

    /* We can use a Vertex Array Object (VAO) to cache the data associated to
     * the calls to `glVertexAttribPointer' and `glEnableVertexAttribArray'. */
    unsigned int vao;
    glGenVertexArrays(1, &vao);

    /* First, we bind the VAO to the OpenGL context */
    glBindVertexArray(vao);

    /* For sending the data to the graphics pipeline, we need to create a
     * OpenGL buffer called Vertex Buffer Object (VBO) */
    uint32_t vbo;
    glGenBuffers(1, &vbo);

    /* Bind the VBO we just created to the GL_ARRAY_BUFFER target */
    glBindBuffer(GL_ARRAY_BUFFER, vbo);

    /* Copy our vertex data into the buffer's memory. The last argument
     * should change depending on the type of data we are drawing:
     * - GL_STREAM_DRAW: Data is set only once, used by the GPU a few times.
     * - GL_STATIC_DRAW: Data is set only once, used many times.
     * - GL_DYNAMIC_DRAW: Data is changed a lot, used many times. */
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    /* Specify:
     *  - Number of vertex attribute. See (location = 0) in vertex_shader_source
     *  - Size of the vertex attribute: X,Y,Z floats
     *  - Type of the data: Float(s)
     *  - Normalize: False
     *  - Stride (space in bytes between consecutive vertex attributes): Index 1
     *    is 3 floats appart from index 0.
     *  - Offset where the data begins, has to be (void*)
     */
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float),
                          (void*)0);
    glEnableVertexAttribArray(0);

    /* Main render loop */
    while (!glfwWindowShouldClose(window)) {
        /* Process the user input */
        process_input(window);

        /* Use the program we created, with the vertex shader and the fragment
         * shader data. Bind the VAO (and the VBO, therefore). Draw the arrays
         * as triangles from offset 0 using 3 vertices. */
        glUseProgram(shader_program);
        glBindVertexArray(vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);

        /* Render. Uses double buffering to avoid tearing. */
        glfwSwapBuffers(window);

        glfwPollEvents();
    }

    /* GLFW cleanup */
    glfwTerminate();
    return 0;
}
