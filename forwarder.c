// afdog log collector
// listens on a well-defined socket and spits out log messages to stdout

#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define SOCKET_ADDR "ipc:///tmp/afdog-logging-collect"
#define SOCKET2_ADDR "ipc:///tmp/afdog-logging"
//#define SOCKET_ADDR "tcp://127.0.0.1:5555"

int main(int argc, char *argv[]) {
    void *context = zmq_init(1);

    void *sub = zmq_socket(context, ZMQ_SUB);
    zmq_setsockopt(sub, ZMQ_SUBSCRIBE, "", 0);
    zmq_bind(sub, SOCKET_ADDR);

    void *pub = zmq_socket(context, ZMQ_PUB);
    zmq_bind(pub, SOCKET2_ADDR);

    zmq_device(ZMQ_FORWARDER, sub, pub);

    zmq_close(sub);
    zmq_close(pub);
    zmq_term(context);
    return 0;
}
