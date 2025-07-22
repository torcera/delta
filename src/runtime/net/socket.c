#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define PORT 8080
#define BACKLOG 5

// TODO: Move to IO
void pputs(char *str) {
    printf("%s\n", str);
}

int create_socket() {
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        perror("Failed to create socket");
        return -1;
    }
    printf("Socket created successfully: %d\n", sockfd);
    return sockfd;
}

int bind_socket(int sockfd) {
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    server_addr.sin_port = htons(PORT);

    if (bind(sockfd, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
        perror("Failed to bind socket");
        return -1;
    }
    printf("Socket bound to port %d\n", PORT);
    return 0;
}

int listen_socket(int sockfd) {
    printf("SOCKET: %d\n", sockfd);
    if (listen(sockfd, BACKLOG) < 0) {
        perror("Failed to listen on socket");
        return -1;
    }
    printf("Socket is now listening\n");
    return 0;
}

int accept_connection(int sockfd) {
    struct sockaddr_in client_addr;
    socklen_t addr_len = sizeof(client_addr);
    int client_sock = accept(sockfd, (struct sockaddr *)&client_addr, &addr_len);
    if (client_sock < 0) {
        perror("Failed to accept connection");
        return -1;
    }
    return client_sock;
}

int send_data(int sockfd, const char *data) {
    int sent = send(sockfd, data, strlen(data), 0);
    if (sent < 0) {
        perror("Failed to send data");
        return -1;
    }
    return sent;
}

int receive_data(int sockfd, char *buffer, size_t buffer_size) {
    int received = recv(sockfd, buffer, buffer_size, 0);
    if (received < 0) {
        perror("Failed to receive data");
        return -1;
    }
    buffer[received] = '\0';
    return received;
}

int close_socket(int sockfd) {
    close(sockfd);
}