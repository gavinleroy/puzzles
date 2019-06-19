#include <cstdio>
#include <algorithm>
#define MAX_N 300001
#define INF ~(1LL << 63) 

typedef long long ll;

using namespace std;

struct city{
	ll val;
	ll ind;
};

ll dist(ll a, ll b){
	return ((a > b)? a-b: b-a);
}

auto cmp = [](const city a, const city b){ return a.val < b.val; };

int main(void){
	ll n, k, dist = 0;
	city* aa = new city[MAX_N];
	scanf("%lld %lld", &n, &k);
	for(ll i = 1; i <= n; i++){ 
		scanf("%lld", &aa[i].val); 
		aa[i].ind = i;
		dist += aa[i].val;
	}
	sort(aa+1, aa+n+1, cmp);
	aa[0].val = aa[0].ind = 0;
	ll cap = n+1LL, p_dist = dist, val = INF;
	for(ll i = 1; i <= n; i++){
		dist = p_dist - ((aa[i].val - aa[i-1].val) * (n-(i-1LL))) + ((aa[i].val - aa[i-1].val)*(i-1LL));
		if(dist-k > 0 && dist > p_dist) break;
		else if(dist-k <= 0){
			ll d = (k-dist)%2;
		       	if(d < val || (d == val && cap > aa[i].ind)){
				cap = aa[i].ind; 
				val = d;
			}
		}else if(dist-k < val || (dist-k == val && cap > aa[i].ind)){
			cap = aa[i].ind;
			val = dist-k;
		}		
		p_dist = dist;
	}
	if(cap < 1 || cap > n || val == INF || val < 0) throw;
	printf("%lld %lld\n", cap, val);
	return 0;
}
