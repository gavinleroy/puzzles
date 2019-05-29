#include <cstdio>
#include <vector>
#define MOD 987654319
#define MAX_N 300001

typedef long long ll;

using namespace std;

struct edge{
	ll l;
	ll r;
};

auto cmp = [](const edge a, const edge b){
	if(a.l == b.l) return aa.r < bb.r;
	else return aa.l < bb.l;
};

int main(void){
	ll n, MAX_W, ans = 1;
	scanf("%lld %lld", &n, &MAX_W);
// Not sure about this data structure. It doesn't seem to provide me with a fast way of finding the nodes that aren't connected. Need to rethink this model.
	edge* ee = new edge[MAX_N];
	ll* mm = new ll[MAX_N];
	for(int i = 0; i <= n; i++) mm[i] = -1;
	for(int i = 1; i < n; i++){
		ll t1, t2, tw;
		scanf("%lld %lld %lld", &t1, &t2, &tw);
		ee[i].l = min(t1, t2);
		ee[i].r = max(t1, t2);
		if(tw > mm[t1]) mm[t1] = tw;
		if(tw > mm[t2]) mm[t2] = tw;
	}	
	sort(gg+1, gg+n+1, cmp);
	delete []gg;
	return 0;
}
