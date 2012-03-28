// afdog log collector
// listens on a well-defined socket and spits out log messages to stdout

#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define SOCKET_ADDR "ipc:///tmp/afdog-logging"
//#define SOCKET_ADDR "tcp://127.0.0.1:5555"

// source: http://stackoverflow.com/questions/122616/how-do-i-trim-leading-trailing-whitespace-in-a-standard-way
char *trim_whitespace(char *str)
{
    char *end;

    // Trim leading space
    while(isspace(*str)) str++;

    if(*str == 0)  // All spaces?
        return str;

    // Trim trailing space
    end = str + strlen(str) - 1;
    while(end > str && isspace(*end)) end--;

    // Write new null terminator
    *(end+1) = 0;

    return str;
}

int string_empty(char *str) {
    return strlen(str) == 0;
}

char *s_recv(void *socket) {
    zmq_msg_t message;
    zmq_msg_init(&message);
    zmq_recv(socket, &message, 0);
    int size = zmq_msg_size(&message);
    char *string = malloc((size+1) * sizeof(char));
    memcpy(string, zmq_msg_data(&message), size);
    zmq_msg_close(&message);
    string[size] = 0;
    return string;
}

int main(void) {
    void *context = zmq_init(1);

    void *socket = zmq_socket(context, ZMQ_SUB);
    zmq_setsockopt(socket, ZMQ_SUBSCRIBE, "", 0);
    zmq_connect(socket, SOCKET_ADDR);

    while(1) {
        char *r_str = s_recv(socket);
        char *str = trim_whitespace(r_str);
        if (!string_empty(str)) {
            printf("%s\n", str);
        }
        free(r_str);
    }
    zmq_close(socket);
    zmq_term(context);
    return 0;
}
