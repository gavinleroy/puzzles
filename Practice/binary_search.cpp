#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include "quick_sort.cpp" 

using namespace std;

int recursive_binary_search(int* aa, int low, int upp, int val){
	if(low > upp){
		throw invalid_argument("Element not found.");
	}else{
		int mid = (low + upp) / 2;
		int curr_val = *(aa + mid);
		if(curr_val == val){
			return mid;
		}else if(curr_val > val){
			return recursive_binary_search(aa, low , mid - 1 , val);
		}else{
			return recursive_binary_search(aa, mid + 1, upp, val);
		}
	}
}

void print(int* aa, int len){
	for(int i = 0; i < len; i++){
		printf("%d ", aa[i]);
	}	
	printf("\n");
}

int main(int argc, char** argv){
	if(argc < 3){
		printf("Not enough arguments were supplied.\n");
		return 0;
	}
	const int SIZE = atoi(argv[1]);
	const int SEARCH_VAL = atoi(argv[2]);
	int* aa = new int[SIZE];
	
	for(int i = 0; i < SIZE; i++){
		aa[i] = rand() % 50;
	}
	print(aa, SIZE);
	recursive_quick_sort(aa, 0, SIZE - 1);
	print(aa, SIZE);
	int answer = recursive_binary_search(aa, 0, SIZE, SEARCH_VAL);
	printf("The ind of %d is: %d.\n", SEARCH_VAL, answer);
	return 0;
}
