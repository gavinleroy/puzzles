#include <cstdio>
#include<cmath>
#include <algorithm>
#include <string>

#define TABLE_WIDTH 25
#define ADDR_SIZE 16
#define DATA_SIZE 28
//#define DATA_SIZE 32
#define TOTAL_BITS 900
//#define TOTAL_BITS 840
#define MISS 19
//#define MISS 10
#define INVALID -1
#define SMALLEST_BLOCK 4
#define TOTAL_BYTES pow(2, (int)log2((TOTAL_BITS/8)))
#define pcpi(c) printf("CPI = %f \n\n\n",c/(float)DATA_SIZE)
#define cpi(c) c/(float)DATA_SIZE

/**************************************************************************************
 **************************************************************************************
 * NOTE TO GRADERS:
 *
 * When computing the cycles for a cache miss it was to my
 * 	understanding that this meant it it was: 1+MISS+(1*BLOCK_SIZE_IN_BYTES)
 * 	in professor Jensens study solution notes he does not include the '1+'.
 * Therefore, if that is not the case feel free to change the above macro MISS
 * 	to one less than it currently is and the results will(should) be as desired.
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
	int** cache = new int*[(int)TOTAL_BYTES];
	// Create a large matrix just so we can have enough storage.
	for(int i = 0; i < TOTAL_BYTES; i++) cache[i] = new int[(int)TOTAL_BYTES];
	// Simulate different block sizes.
	for(int b = SMALLEST_BLOCK; b <= TOTAL_BYTES; b*=2){
		// Simulate different set sizes
		int sets = 1;
		while(++sets){ // Start the number of sets at 2, because 1-way set associative is boring.
			int num_rows = pow(2, (int)log2((TOTAL_BITS/sets/8)))/b;
			int LRU = ceil(log2(sets));
			int blckBits = log2(b);
			int rwBits = (log2(num_rows)==0) ? 1: log2(num_rows);
			// Make sure we aren't going over memory
			int total_bits = sets*num_rows*(b*8+ADDR_SIZE-blckBits-rwBits+LRU); 
			if(num_rows <= 0 || total_bits > TOTAL_BITS) break;
				
			printf("SIMULATING %d WAY SET-ASSOCIATIVE CACHE WITH %d BYTE BLOCKS AND %d ROWS\n",sets, b, num_rows);
			printf("ADDR: tag:[%d-%d], row:[%d-%d], offset:[%d-%d]\n",ADDR_SIZE,blckBits+rwBits,blckBits+rwBits-1,blckBits,blckBits-1, 0);
			for(int i = 0; i < num_rows; i++) fill_n(cache[i], sets, INVALID);
			for(int i = 0; i < DATA_SIZE; i++){
				int tag = DATA[i]/(b*num_rows);
				int row = (DATA[i]/b)%num_rows;		
				find(cache[row], sets, tag);
			}
			int cycles = 0;
			printf("... after one iteration ...\n");
			for(int i = 0; i < DATA_SIZE; i++){
				int tag = DATA[i]/(b*num_rows);
				int row = (DATA[i]/b)%num_rows;		
				printf("Accessing %d(tag %d): ",DATA[i], tag);
				cycles++;
				if(!find(cache[row], sets, tag)){
					printf("miss - cached to row %d\n", row);
					cycles += (MISS + b);
				}else{ printf("hit from row %d\n", row); }
			}
			// Append our results to the tableContents.
			pcpi(cycles);
			string temp = "[S: " + to_string(sets)  +  " R: " + to_string(num_rows) + ", B: " + to_string(b) + "]"; 
			tableContents += temp + string(TABLE_WIDTH-temp.size(), ' ') + to_string(cpi(cycles)) + "\n";
		}
	}

	for(int i = 0; i < TOTAL_BYTES; i++) delete []cache[i];
	delete []cache;
	string header = "SET-ASSOCIATIVE";
	return header + string(TABLE_WIDTH-header.size(), ' ') + "CPI" + "\n" + tableContents;
}

string Fully_Associative(int DATA[]){ 
	string tableContents = "";
	int* cache = new int[(int)TOTAL_BYTES]; // Create our 'cache'

	for(int b = SMALLEST_BLOCK; b <= TOTAL_BYTES; b*=2){
		int num_rows = 0; // Calculate number of rows
		int l_cpi = -1;
		string highest = "";
		while(++num_rows){
			int blckBits = log2(b);
			int LRU = ceil(log2(num_rows));
			int total_bits = num_rows*(b*8+ADDR_SIZE-blckBits+LRU); // Make sure we aren't going over memory
			if(total_bits > TOTAL_BITS) break;

			// Invalidate all the memory
			std::fill_n(cache,num_rows, INVALID); // Initialize to -1's as our valid bit.

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
			l_cpi = cycles;
			highest = "[R: " + to_string(num_rows) + ", B: " + to_string(b) + "]"; 
		}
		tableContents += highest + string(TABLE_WIDTH-highest.size(), ' ') + to_string(cpi(l_cpi)) + "\n";
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
		int blckBits = log2(b);
		int rwBits = (log2(num_rows)==0) ? 1: log2(num_rows);
		int total_bits = num_rows*(b*8+ADDR_SIZE-blckBits-rwBits); // Make sure we aren't going over memory
		if(total_bits > TOTAL_BITS) break;

		printf("SIMULATING DIRECT MAPPED CACHE WITH %d BYTE BLOCKS AND %d ROWS\n",b, num_rows);
		printf("ADDR: tag:[%d-%d], row:[%d-%d], offset:[%d-%d]\n",ADDR_SIZE,blckBits+rwBits,blckBits+rwBits-1,blckBits,blckBits-1, 0);

		std::fill_n(cache,num_rows, INVALID); // Initialize to -1's as our valid bit.

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
//	int STREAM[DATA_SIZE] = { 4, 8, 12, 16, 20, 32, 36, 40, 44, 20, 32, 36, 40, 44, 64, 68, 4, 8, 12, 92, 96, 100, 104, 108, 112, 100, 112, 116, 120, 128, 140, 144 };
	string dM = Direct_Mapped(STREAM) + "\n";
	string fA = Fully_Associative(STREAM) + "\n";
	string sA = Set_Associative(STREAM) + "\n";
	printf("%s%s%s", dM.c_str(), fA.c_str(), sA.c_str());
	return 0;
}
