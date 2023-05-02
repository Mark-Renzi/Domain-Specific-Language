#include <wiringPiI2C.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <json.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <unistd.h>

#define MAX_BUFFER_SIZE 4096

int connectServer(char *server_ip, char *server_port){
	struct addrinfo hints;
	struct addrinfo *results, *curr;
	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_flags = AI_PASSIVE;
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags |= AI_NUMERICSERV;
	
	int s;
	int status = getaddrinfo(server_ip, server_port, &hints, &results);
	if(status!=0){
		fprintf(stderr, "Error: %s\n", gai_strerror(status));
	}
	for(curr=results; curr!=NULL; curr=curr->ai_next){
		s = socket(curr->ai_family, curr->ai_socktype, curr->ai_protocol);
		if(s==-1){
			perror("Socket Error");
			continue;
		}
		if(connect(s, curr->ai_addr, curr->ai_addrlen)==0) break;
		perror("Connect Error");
		close(s);
	}
	
	return s;
}

int parseResponse(char* response){
	
	printf("Response:\n%s", response);
	//printf("Header length: %d\n", strlen(response));
	char *pos = strstr(response, "HTTP/1.0 ");
	if (pos != NULL) {
		int status_code = atoi(pos+9);
		if (status_code == 200) {
			printf("Success!\n");
			pos = strstr(response, "Content-Length: ");
			printf("%p\n",pos);
			//int content_length = atoi(pos+16);
			//printf("Content Length: %d\n", content_length);
			//printf("Payload: %s\n", response + (strlen(response)-content_length));
			// TODO- parse server esponse based on return type of execution chain, Client should have knowledge of this
		} else {
			printf("Error: HTTP status code %d\n", status_code);
			return -1;
		}
	}
        
}
#include "mylib"
int32_t x = 5;
int flag = 1;
uint32_t count = 42;
char * a = "forty-two";
a = "zero";
float PI = 3.1415;
uint32_t myTarget = wiringPiI2CSetup(25);
int32_t babab = wiringPiI2CRead(myTarget);
uint8_t myFunc(uint32_t price, char * item) {
	uint32_t amt = price;
	return amt;
}
count = 1;
uint8_t ret = myFunc(count, a);
char * b = "one";
char * c = "two";
if (a == b) {
	a = c;
} else if (0) {
	b = a;
	a = b;
} else {
	a = a;
} 
c[42] = a;
uint32_t report_serv = connectServer("127.0.0.1", "9999");
char *report_serv_add_report_template = "{"params": {"reading": %f}, "body": int result = add_data(reading);
if (result) {
	return sum_data();
} else {
	err_report();
	return -1;
} }"
while (flag) {
	count = count + 1;
	if (count == 42) {
	flag = 0;
} else {
	flag = 1;
} 
}
for (uint8_t i = 0; i < 10; i = i + 1) {
	count = count + 1;
	if (count == 42) {
	flag = 0;
} else {
	flag = 1;
} 
}