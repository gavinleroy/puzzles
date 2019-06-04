#include <cstdio>
#include <algorithm>
#define MAX 100001

using namespace std;

int bubble_sort(int* aa, int n){
	int ret = 0;
	for(int i = 1; i <= n; i++){
		int swps = 0;
		for(int j = 2; j <= n; j++){
			if(aa[j] < aa[j-1]){ swap(aa[j], aa[j-1]); swps++; }
		}
		if(swps == 0) return ret;
		ret += swps;
	}
	return ret;
}

int main(void){
	int t;
	scanf("%d", &t);
	int* aa = new int[MAX];
	while(t--){
		int n, ans = 0;
		bool c = true;
		scanf("%d", &n);
		for(int i = 1; i <= n; i++){
			scanf("%d", aa+i);	
			if(aa[i] - i > 2) c = false;
		}
		if(!c) printf("Too chaotic\n");
		else printf("%d\n", bubble_sort(aa, n));
	}
	return 0;
}
