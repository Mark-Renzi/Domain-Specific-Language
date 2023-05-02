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

//TODO- function to put values into defined execution chain,
//		function to put execution chain into HTTP Header string
//		function to initialize I2C device given address/pin


void main(){
	char http_header[1000] = "POST / HTTP/1.0\r\nHost: 127.0.0.1\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s";
	int target = wiringPiI2CSetup(0x19);
	
	printf("%f\n", target);
	int s;
	char result = 0;
	
	char *server_ip = "127.0.0.1";
	char *server_port = "9999";
	
	s = connectServer(server_ip, server_port);
	
	int length;
	struct json_object *json_body;
	char payload[1200];
	//char message[200] = "{\"params\": \'{\"param1\": {\"type\":\"type\", \"val\":\"literal_value\"}, \"tempReading\": \"32.3\"}\',\"body\": \"print(\"Hey, this works!\")\nadd_data()\"}";
	char message_missing_param[250] = "{\"params\": {\"humidity\": {\"type\": \"int\", \"val\": %d}, \"body\": \"print(\'Hey, this worked!\')\nresult = sum_data(\'humidity\')\"}";
	char message[300];
	char response[MAX_BUFFER_SIZE];
	int received = 0;
	for(int i = 0; i < 1 ; i++){
		result = wiringPiI2CRead(target);
		printf("Got %d\n",result);
		//message = json_object_to_json_string_ext(json_body)
		sprintf(message, message_missing_param, result);
		length = strlen(message);
		sprintf(payload, http_header, length, message);
		printf("%s\n\n", payload);
		length = strlen(payload);
		send(s, payload, length,0);
		//sleep(2);
		received = recv(s, response, MAX_BUFFER_SIZE, 0);
		printf("Received %d bytes\n", received);
		if(received == 123){
			received = recv(s, response + 123, MAX_BUFFER_SIZE - 123,0);
			printf("Received %d bytes\n", received);
		}
		parseResponse(response);
		close(s);
		s = connectServer(server_ip, server_port);
	}
}
