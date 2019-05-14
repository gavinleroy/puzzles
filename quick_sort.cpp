#include <cstdio>
#include <cstdlib>
#include <algorithm>

using namespace std;

void swap(int* a, int* b){
	int temp = *a;
	*a = *b;
	*b = temp;
}

int partition(int* aa, int l, int h){
	int i;
	int p;
	int fh;

	p = h;
	fh = l;

	for(i = l; i < h; i++){
		if(aa[i] < aa[p]){
			swap(aa + i, aa + fh);
			fh++;
		}
	}
	swap(aa+p, aa+fh);

	return fh;
}

void recursive_quick_sort(int* aa, int l, int h){
	if((h-l) > 0){ 
		int p = partition(aa, l, h);
		recursive_quick_sort(aa, l, p - 1);
		recursive_quick_sort(aa, p + 1, h);
	}
}
