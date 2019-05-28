#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include <vector>
#define MAX ~(1LL << 63)

typedef long long ll;

using namespace std;

struct guard{
	ll x;
	ll y;
	bool u = false;
	vector<ll> vv;
};

auto cmp = [](const guard a, const guard b){ return a.vv.size() < b.vv.size(); };

ll find_difference(vector<bool> &unc, vector<ll> &vv){
	ll diff = 0;
	for(auto val : vv){
		if(!unc[val]){
			diff++;
		}
	}
	return diff;
}

ll solve(guard* gg, ll n, vector<bool> &uncovered, ll m){
	ll g_u = 0;
	while(m){
		ll min = -1, max_v = -1;	
		for(ll i = 0; i < n; i++){
			if(!gg[i].u){ 
				ll diff = find_difference(uncovered, gg[i].vv);
				if(diff > max_v){
					max_v = diff;
					min = i;
				}	
			}
		}
		for(auto val : gg[min].vv) uncovered[val]=true;

		m -= max_v;
		g_u++;
		gg[min].u = true;
	}
	return g_u;
}

int main(void){
	ll n, m;
	scanf("%lld %lld", &n, &m);
	guard* gg = new guard[n+1];
	vector<bool> uncovered(m);
	for(int i = 0; i< n; i++){
		scanf("%lld %lld", &gg[i].x, &gg[i].y);
	}
	for(ll i = 0; i < m; i++){
		ll _x, _y;
		uncovered[i] = 0;
		scanf("%lld %lld", &_x, &_y);	
		for(ll j = 0; j < n; j++){
			if(abs(gg[j].y - _y) <= _x - gg[j].x){
				gg[j].vv.push_back(i);
			}
		}
	}
	sort(gg, gg+n, cmp);
	printf("%lld\n", n - solve(gg, n, uncovered, m));
	delete []gg;
	return 0;
}
