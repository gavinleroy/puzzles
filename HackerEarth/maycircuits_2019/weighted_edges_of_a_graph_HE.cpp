#include <cstdio>
#include <algorithm>
#define MOD 987654319
#define MAX_N 300001

typedef long long ll;

using namespace std;

struct node{
	ll i = -1;
	ll w = -1;
	ll a = 0;
};

struct edge{
	ll n1;
	ll n2;
	ll w;
};

ll exp(ll x, ll y, ll p){ 
    ll res = 1LL;
    x = x % p;  
    while (y > 0LL){ 
        if (y & 1LL) 
            res = (res*x) % p; 
        y = y>>1LL;
        x = (x*x) % p;   
    } 
    return res; 
} 

auto cmp = [](const node a, const node b){
	if(a.w == b.w) return a.i < b.i;
	else return a.w < b.w;
};

int main(void){
	ll n, MAX_W, ans = 1;
	scanf("%lld %lld", &n, &MAX_W);
	node* mm = new node[MAX_N];
	edge* ee = new edge[MAX_N];
	for(int i = 1; i < n; i++){
		ll t1, t2;
		scanf("%lld %lld %lld", &t1, &t2, &ee[i].w);
		ee[i].n1 = min(t1, t2);
		ee[i].n2 = max(t1, t2);
		ee[i].w = ((ee[i].w <= MAX_W) ? ee[i].w: MAX_W);
		if(ee[i].w > mm[ee[i].n1].w) mm[ee[i].n1].w = ee[i].w;
		if(ee[i].w > mm[ee[i].n2].w) mm[ee[i].n2].w = ee[i].w;
		mm[ee[i].n2].i = ee[i].n2;
		mm[ee[i].n1].i = ee[i].n1;
	}	
	for(int i = 1; i < n; i++){
		if(mm[ee[i].n1].w > mm[ee[i].n2].w) mm[ee[i].n1].a++;
		else mm[ee[i].n2].a++;
	}
	sort(mm+1, mm+n+1, cmp);
	for(ll i = n; i > 0; i--){
		ll poss = (MAX_W - mm[i].w) + 2LL;
		ll expo = (i - 1LL - mm[i].a);
		ans = (ans * (exp(poss, expo, MOD)) % MOD) % MOD;
	}
	printf("%lld\n", (ans) % MOD);
	delete []mm;
	delete []ee;
	return 0;
}
