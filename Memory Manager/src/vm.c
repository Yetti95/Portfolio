/**
 *
 * Aj Mortensen 
 * 351 Homework 5 
 * */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

//fseak find the page number from the offset


// number of characters to read for each line from input file
#define BUFFER_SIZE         10

#define TOTAL_NUMBER_OF_FRAMES 256
// number of bytes to read
// this is the size of the pages
#define CHUNK               256

//mask for getting the address
#define ADDRESS_MASK 0xFFFF

//mask for getting the offset
#define OFFEST_MASK 0xFF

//size fo the frame given in assingment
#define FRAME_SIZE 256

//size of the tbl given in assignment
#define TLB_SIZE 16

//size of the page table given in assignment
#define PAGE_TABLE_SIZE 256
//

FILE    *address_file;
FILE    *backing_store;

// how we store reads from input file
char    address[BUFFER_SIZE];

int     logical_address;

// the buffer containing reads from backing store
signed char     buffer[CHUNK];

// the value of the byte (signed char) in memory
signed char     value;

// our arrays to hold all the stuff
int pageTableNumbers[PAGE_TABLE_SIZE];  
int pageTableFrames[PAGE_TABLE_SIZE];   
int TLBPageNumber[TLB_SIZE]; 
int TLBFrameNumber[TLB_SIZE]; 
int physicalMemory[TOTAL_NUMBER_OF_FRAMES][FRAME_SIZE];

//counters for stuff we need later
int pageFaults = 0;
int hits = 0;
int firstAvailableFrame = 0;
int firstAvailablePageTableNumber = 0;
int numInTLB = 0;
void insertTLB(int pageNumber, int frameNumber);
//this does the work of the main method
void getPage(int logical_address){
    //gets the pageNumber and offest by anding it with the mask
        // need to shift by 10 so we hit the right part
    int pageNumber = ((logical_address & ADDRESS_MASK)>>8); 
    int offset = ((logical_address & OFFEST_MASK));
    int frameNumber = -1;

    for(int i = 0; i < TLB_SIZE; i++){
        if(TLBPageNumber[i] == pageNumber){
            frameNumber = TLBFrameNumber[i];
            hits++;
        }
    }
    if(frameNumber == -1){
        for(int i = 0; i < firstAvailableFrame; i++){
            if(pageTableNumbers[i]==pageNumber){
                frameNumber = pageTableFrames[i];
            }
        }   
        if(frameNumber == -1){
            if (fseek(backing_store, pageNumber * CHUNK, SEEK_SET) != 0) {
                fprintf(stderr, "Error in backing store\n");
            }
            if (fread(buffer, sizeof(signed char), CHUNK, backing_store) == 0) {
                fprintf(stderr, "Error reading backing store\n");        
            }
            for(int i = 0; i < CHUNK; i++){
                physicalMemory[firstAvailableFrame][i] = buffer[i];
            }
        pageTableNumbers[firstAvailablePageTableNumber] = pageNumber;
        pageTableFrames[firstAvailablePageTableNumber] = firstAvailableFrame;
        firstAvailableFrame++;
        firstAvailablePageTableNumber++;   
        pageFaults++;
        frameNumber = firstAvailableFrame - 1;
        }
    }
    insertTLB(pageNumber, frameNumber);
    value = physicalMemory[frameNumber][offset];
    printf("frame number: %d\t", frameNumber);
    printf("offset: %d\n", offset);
    printf("Virtual address: %d\t Physical address: %d \tValue: %d\n",
        logical_address, (frameNumber << 8) | offset, value);
}

void insertTLB(int pageNumber, int frameNumber){
    
    int i; 
    for(i = 0; i < numInTLB; i++){
        if(TLBPageNumber[i] == pageNumber){
            break;
        }
    }
    
    if(i == numInTLB){
        if(numInTLB < TLB_SIZE){  
            TLBPageNumber[numInTLB] = pageNumber;
            TLBFrameNumber[numInTLB] = frameNumber;
        }
        else{                                            
            for(i = 0; i < TLB_SIZE - 1; i++){
                TLBPageNumber[i] = TLBPageNumber[i + 1];
                TLBFrameNumber[i] = TLBFrameNumber[i + 1];
            }
            TLBPageNumber[numInTLB-1] = pageNumber;  
            TLBFrameNumber[numInTLB-1] = frameNumber;
        }        
    }
    
    else{
        for(i = i; i < numInTLB - 1; i++){       
            TLBPageNumber[i] = TLBPageNumber[i + 1];      
            TLBFrameNumber[i] = TLBFrameNumber[i + 1];
        }
        if(numInTLB < TLB_SIZE){                
            TLBPageNumber[numInTLB] = pageNumber;
            TLBFrameNumber[numInTLB] = frameNumber;
        }
        else{                                             
            TLBPageNumber[numInTLB-1] = pageNumber;
            TLBFrameNumber[numInTLB-1] = frameNumber;
        }
    }
    if(numInTLB < TLB_SIZE){   
        numInTLB++;
    }    
}

int main(int argc, char *argv[])
{
int i = 0;

    if (argc != 3) {
        fprintf(stderr,"Usage: ./vm [backing store] [input file]\n");
        return -1;
    }
    backing_store = fopen(argv[1], "rb");    
    if (backing_store == NULL) { 
        fprintf(stderr, "Error opening %s\n",argv[1]);
        return -1;
    }
    address_file = fopen(argv[2], "r");
    if (address_file == NULL) {
        fprintf(stderr, "Error opening %s\n",argv[2]);
        return -1;
    }
    while ( fgets(address, BUFFER_SIZE, address_file) != NULL) {
        logical_address = atoi(address);

        if (fseek(backing_store, CHUNK * i, SEEK_SET) != 0) {
            fprintf(stderr, "Error seeking in backing store\n");
            return -1;
        }

        if (fread(buffer, sizeof(signed char), CHUNK, backing_store) == 0) {
            fprintf(stderr, "Error reading from backing store\n");
            return -1;
        }
       
        value = buffer[50];
         
        printf("%d \t %d\n",logical_address, value);

        //this does our work
        getPage(logical_address);
        // just so we seek to 10 different spots in the file
        i = (i + 1) % 10;
    }
    printf("Hitts: %d\n", hits);
    printf("Faults: %d\n", pageFaults);

    fclose(address_file);
    fclose(backing_store);

    return 0;
}

