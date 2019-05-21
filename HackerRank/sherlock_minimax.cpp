#include <cstdio>
#include <algorithm>
#include <cmath>
typedef long long ll;

using namespace std;

ll solve(ll* nn, ll n, ll p, ll q){
	if(nn[0] > q) return p;
	else if(nn[n-1] < p) return q;
	else{
		ll num = -1, val = -1;	
		if(nn[0] > p && nn[0]-p > val){
			num = p;
			val = nn[0] - p;
		}if(nn[n-1] < q && q-nn[n-1] > val){
			num = q;
			val = q-nn[n-1];
		}
		for(int i = 0; i < n; ++i){
			ll mid = (nn[i] + nn[i+1]) / 2LL;
			if(p <= mid && mid <= q && mid-nn[i] > val){
				num = mid;
				val = mid-nn[i];
			}else if(mid > q && q-nn[i] > val){
				num = q;
				val = q-nn[i];
			}else if(mid < p && nn[i + 1]-p > val){
				num = p;
				val = nn[i + 1]-p;
			}
		}
		return num;
	}	
}

int main(void){
	ll n, p, q;
	scanf("%lld", &n);
	ll* nn = new ll[n+1];
	for(int i = 0; i < n; ++i){
		scanf("%lld", nn+i);
	}
	scanf("%lld %lld", &p, &q);
	sort(nn, nn+n);
	printf("%lld\n", solve(nn, n, p, q));
	delete []nn;	
	return 0;
}
