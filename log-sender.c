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

int main(void) {
    void *context = zmq_init(1);

    void *socket = zmq_socket(context, ZMQ_PUB);
    zmq_setsockopt(socket, ZMQ_LINGER, "250", sizeof("250"));
    zmq_bind(socket, SOCKET_ADDR);

    while(1) {
      char msg[20];
      sprintf(msg, "(TRACE) foo");
      s_send(socket, msg);
    }
    zmq_close(socket);
    zmq_term(context);
    return 0;
}
