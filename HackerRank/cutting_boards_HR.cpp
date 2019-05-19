#include <cstdio>
#include <algorithm>
#include <cstdlib>
#include <functional>
typedef long long ll; 

using namespace std;

ll* mm = new ll[1000001];
ll* nn = new ll[1000001];
ll MOD = 1e9 + 7;
ll I_INF = ~(1LL<<63);

ll solve(int m, int n){
	ll ans = 0;
	ll _m = 1, mi = 0, _n = 1, ni = 0;
	bool isM = false;
	while(mi < m-1 || ni < n-1){
		if(mi >= m-1) isM = false;
		else if(ni >= n-1) isM = true;
		else if(mm[mi] >= nn[ni]) isM = true;
		else if(nn[ni] > mm[mi]) isM = false;
		else printf("Something fucked up\n");

		if(isM){ 
			ans = (ans + (mm[mi++] * (_n))) % MOD;
			_m++;
		}
		else{
		       	ans = (ans + (nn[ni++] * (_m))) % MOD;
			_n++;
		}
	}
	return ans;	
}

int main(void){
	int t;
	scanf("%d", &t);

	while(t--){
		int m, n;
		scanf("%d %d", &m, &n);
		for(int i = 0; i < m-1; ++i){
			scanf("%lld", &mm[i]);
		}
		for(int i = 0; i < n-1; ++i){
			scanf("%lld", &nn[i]);
		}
		sort(mm, mm+(m-1), greater<int>());
		sort(nn, nn+(n-1), greater<int>());
		printf("%lld\n", solve(m, n) % MOD);
	}
	delete []mm;
	delete []nn;
	return 0;
}
