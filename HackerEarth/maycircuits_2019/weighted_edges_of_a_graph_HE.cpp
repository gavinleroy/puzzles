#include <cstdio>
#include <vector>
#include <algorithm>
#include <string>
#include <unordered_set>
#define MOD 987654319
#define MAX_N 300001

typedef long long ll;

using namespace std;

int main(void){
	ll n, MAX_W, ans = 1;
	scanf("%lld %lld", &n, &MAX_W);
	unordered_set<string> ss;
	ll* mm = new ll[MAX_N];
	for(int i = 0; i <= n; i++) mm[i] = -1;
	for(int i = 1; i < n; i++){
		ll t1, t2, tw;
		scanf("%lld %lld %lld", &t1, &t2, &tw);
		ss.insert(to_string(min(t1, t2)) + "-" + to_string(max(t1, t2)));
		if(tw > mm[t1]) mm[t1] = tw;
		if(tw > mm[t2]) mm[t2] = tw;
	}	
	for(ll i = 1; i <= n; i++){
		for(ll j = i+1; j <= n; j++){
			if(ss.find(to_string(i) + "-" + to_string(j)) == ss.end()){
				ll diff = MAX_W - max(mm[i], mm[j]);
				ans = (ans * ( (MAX_W - max(mm[i], mm[j])) + 2LL) % MOD ) % MOD;
			}
		}
	}
	printf("%lld\n", ans);
	delete []mm;
	return 0;
}
