#include <cstdio>
#include <iostream>
#include<cmath>
#include <algorithm>
#include <string>

#define TABLE_WIDTH 20
#define ADDR_SIZE 16
#define DATA_SIZE 28
#define TOTAL_BITS 900
#define MISS 19
#define INVALID -1
#define SMALLEST_BLOCK 4
#define TOTAL_BYTES pow(2, (int)log2((TOTAL_BITS/8)))
#define pcpi(c) printf("CPI = %f \n\n\n",cycles/(float)DATA_SIZE)
#define cpi(c) cycles/(float)DATA_SIZE

/**************************************************************************************
 **************************************************************************************
 * NOTE TO GRADERS:
 *
 * When computing the cycles for a cache miss it was to my
 * 	understanding that this meant it it was: 1+MISS+(1*BLOCK_SIZE_IN_BYTES)
 * 	in professor Jensens study solution notes he does not include the '1+'.
 * Therefore, if that is not the case feel free to change the above macro MISS
 * 	to accurrately reflect what it should be and the results will be as desired.
 *
 *************************************************************************************
 *************************************************************************************/
using namespace std;

void b_sort(int* aa, int ind){
	int i = ind+1;
	while(--i){
		swap(aa[i], aa[i-1]);
	}
}

bool find(int* aa, int size, int n){
	for(int i = 0; i < size; i++){
		if(aa[i] == n){ 
			b_sort(aa, i);
			return true;
		}
	}
	aa[size-1] = n;
	b_sort(aa, size-1);
	return false;
}

string Set_Associative(int DATA[]){ 
	string tableContents = "";


	string header = "SET-ASSOCIATIVE";
	return header + string(TABLE_WIDTH-header.size(), ' ') + "CPI" + "\n" + tableContents;
}

string Fully_Associative(int DATA[]){ 
	string tableContents = "";
	int* cache = new int[(int)TOTAL_BYTES]; // Create our 'cache'

	for(int b = SMALLEST_BLOCK; b <= TOTAL_BYTES; b*=2){
		int num_rows = TOTAL_BYTES/b; // Calculate number of rows
		std::fill_n(cache,num_rows, INVALID); // Initialize to -1's as our valid bit.
		int blckBits = log2(b);
		int LRU = ceil(log2(num_rows));
		int total_bits = num_rows*(b*8+ADDR_SIZE-blckBits+LRU); // Make sure we aren't going over memory
		if(total_bits > TOTAL_BITS) break;

		printf("SIMULATING FULLLY ASSOCIATIVE CACHE WITH %d BYTE BLOCKS AND %d ROWS\n",b, num_rows);
		printf("ADDR: tag:[%d-%d], offset:[%d-%d]\n",ADDR_SIZE,blckBits,blckBits-1, 0);

		for(int i = 0; i < DATA_SIZE; i++){ // Simulate data fetching
			int tag = DATA[i]/b;
			find(cache, num_rows, tag);
		}
		int cycles = 0;
		printf("... after one iteration ...\n");
		for(int i = 0; i < DATA_SIZE; i++){ // Second round through to similuate time average.
			int tag = DATA[i]/b;
			printf("Accessing %d(tag %d): ",DATA[i], tag);
			cycles++;
			if(!find(cache, num_rows, tag)){
				printf("miss\n");
				cycles += (MISS + b);
			}else{ printf("hit\n"); }
		}
		pcpi(cycles);
		string temp = "[R: " + to_string(num_rows) + ", B: " + to_string(b) + "]"; 
		tableContents += temp + string(TABLE_WIDTH-temp.size(), ' ') + to_string(cpi(cycles)) + "\n";
	}

	delete []cache;
	string header = "FULLY-ASSOCIATIVE";
	return header + string(TABLE_WIDTH-header.size(), ' ') + "CPI" + "\n" + tableContents;
}

string Direct_Mapped(int DATA[]){ 
	string tableContents = "";
	int* cache = new int[(int)TOTAL_BYTES]; // Create our 'cache'
	for(int b = SMALLEST_BLOCK; b <= TOTAL_BYTES; b*=2){
		int num_rows = TOTAL_BYTES/b; // Calculate number of rows
		std::fill_n(cache,num_rows, INVALID); // Initialize to -1's as our valid bit.
		int blckBits = log2(b);
		int rwBits = (log2(num_rows)==0) ? 1: log2(num_rows);
		int total_bits = num_rows*(b*8+ADDR_SIZE-blckBits+rwBits); // Make sure we aren't going over memory
		if(total_bits > TOTAL_BITS) break;

		printf("SIMULATING DIRECT MAPPED CACHE WITH %d BYTE BLOCKS AND %d ROWS\n",b, num_rows);
		printf("ADDR: tag:[%d-%d], row:[%d-%d], offset:[%d-%d]\n",ADDR_SIZE,blckBits+rwBits,blckBits+rwBits-1,blckBits,blckBits-1, 0);

		for(int i = 0; i < DATA_SIZE; i++){ // Simulate data fetching
			int tag = DATA[i]/(b*num_rows);
			int row = (DATA[i]/b)%num_rows;		
			if(cache[row] == INVALID || cache[row] != tag) cache[row] = tag;
		}
		int cycles = 0;
		printf("... after one iteration ...\n");
		for(int i = 0; i < DATA_SIZE; i++){ // Second round through to similuate time average.
			int tag = DATA[i]/(b*num_rows);
			int row = (DATA[i]/b)%num_rows;		
			printf("Accessing %d(tag %d): ",DATA[i], tag);
			cycles++;
			if(cache[row] == INVALID || cache[row] != tag){
				printf("miss - cached to row %d\n", row);
				cache[row] = tag;
				cycles += (MISS + b);
			}else{ printf("hit from row %d\n",row); }
		}
		pcpi(cycles);
		string temp = "[R: " + to_string(num_rows) + ", B: " + to_string(b) + "]"; 
		tableContents += temp + string(TABLE_WIDTH-temp.size(), ' ') + to_string(cpi(cycles)) + "\n";
	}
	delete []cache;
	string header = "DIRECT-MAPPED";
	return header + string(TABLE_WIDTH-header.size(), ' ') + "CPI" + "\n" + tableContents;
}

int main(){
	int STREAM[DATA_SIZE] = {16,20,24,28,32,36,60,64,56,60,64,68,56,60,64,72,76,92,96,100,104,108,112,120,124,128,144,148};

	int BLOCK_SIZE = 4;
	string dM = Direct_Mapped(STREAM) + "\n";
	string fA = Fully_Associative(STREAM) + "\n";
	string sA = Set_Associative(STREAM) + "\n";
	cout << dM << fA << sA;
	return 0;
}
