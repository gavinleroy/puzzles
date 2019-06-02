#include <cstdio>
#include <utility>
#include <vector>
#include <algorithm>
#define F first
#define S second

typedef long long ll;

const ll MOD = 987654319;

using namespace std;

ll* rr = new ll[300001]; 
ll* pp = new ll[300001]; 
ll* cc = new ll[300001];

ll power(ll x, ll y, ll p){ 
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

ll find(ll n){
	if(pp[n] != n)
		pp[n] = find(pp[n]);
	return pp[n];
}

void u_nion(ll l, ll r){
//	printf("Inside Union, L: %lld, R: %lld\n", l, r);
    	ll l_ = find(l);
    	ll r_ = find(r);
//	printf("The parent of l is: %lld, r: %lld\n", l_, r_); 
//	printf("The Rank of the parents are: rr[l_]: %lld, rr[r_]: %lld\n", rr[l_], rr[r_]);
    	if(l_ == r_) return;
    	if(rr[l_] < rr[r_]){
        	pp[l_] = r_;
        	cc[r_] = cc[r_] + cc[l_];
    	}else if(rr[l_] > rr[r_]){
        	pp[r_] = l_;
        	cc[l_] = cc[l_] + cc[r_];
    	}else{
        	pp[r_] = l_;
        	rr[l_]++;
        	cc[l_] = cc[l_] + cc[r_];
    	}
}

int main(void){
	ll n, MAX_W, ans = 1;
	scanf("%lld %lld", &n, &MAX_W);
	pair<ll, pair<ll, ll>> ii[n-1];
	for(int i = 0; i < n-1; i++){
		ll l, r, w;
		scanf("%lld %lld %lld", &l, &r, &w);
		ii[i] = make_pair(w, make_pair(l, r));
		if(w > MAX_W){ printf("0"); return 0;}	
	}
	sort(ii, ii + (n-1));
	for(int i = 1; i <= n; i++){
		rr[i] = 0LL;
		pp[i] = i;
		cc[i] = 1LL;
	}	
	for(int i = 0; i < n-1; i++){
		ll l = ii[i].S.F, r = ii[i].S.S;
		ll l_ = find(l), r_ = find(r);	
		ll t = (cc[l_]*cc[r_]) - 1;
//		printf("\n\nl: %lld, r: %lld, l_: %lld, r_: %lld, t: %lld\n", l, r, l_, r_, t);
		ans = (ans * power(MAX_W - ii[i].F + 2, t, MOD)) % MOD;
		u_nion(l, r);
	}
	printf("%lld\n", ans);
	delete []rr;
	delete []pp;
	delete []cc;
	return 0;
}
