// afdog log collector
// listens on a well-defined socket and spits out log messages to stdout

#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define SOCKET_ADDR "ipc:///tmp/afdog-logging-collect"
//#define SOCKET_ADDR "tcp://127.0.0.1:5555"

static int
s_send (void *socket, char *string) {
    int rc;
    zmq_msg_t message;
    zmq_msg_init_size (&message, strlen (string));
    memcpy (zmq_msg_data (&message), string, strlen (string));
    rc = zmq_send (socket, &message, 0);
    zmq_msg_close (&message);
    return (rc);
}

int main(int argc, char *argv[]) {
    void *context = zmq_init(1);

    void *socket = zmq_socket(context, ZMQ_PUB);
    zmq_connect(socket, SOCKET_ADDR);

    char msg[20];
    strlcpy(msg, argv[1], 20);

    while(1) {
      s_send(socket, msg);
    }
    zmq_close(socket);
    zmq_term(context);
    return 0;
}
