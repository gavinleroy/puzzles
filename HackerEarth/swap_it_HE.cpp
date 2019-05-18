#include <cstdio>
#include <algorithm>

using namespace std;

void solve(int* aa, int n, int s, int k){
	int m, temp;
	while(k > 0 && s < (n-1)){
		m = s;
		for(int i = s; i < min(s+k+1, n); ++i){
			if(aa[i] < aa[m]) m = i;
		}	
		temp = aa[m];
		if(s != m){
			for(int i = m-1; i >= s; --i){
				aa[i+1] = aa[i];
			}		
		}
		aa[s++] = temp;
		k = (k-(m-s)-1);
	}
}

int main(void){
	int t;
	scanf("%d", &t);
	int* aa = new int[1001];

	while(t--){
		int n, k;
		scanf("%d %d", &n, &k);
		for(int i = 0; i < n; ++i){
			scanf("%d", &aa[i]);
		}
		solve(aa, n, 0, k);
		for(int i = 0; i < n; ++i) printf("%d ", aa[i]);
		printf("\n");
	}
	delete []aa;
	return 0;
}
