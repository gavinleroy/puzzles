#include <cstdio>
#include <vector>
#define MOD 987654319
#define MAX_N 300001

typedef long long ll;

using namespace std;

int main(void){
	ll n, MAX_W, ans = 1;
	scanf("%lld %lld", &n, &MAX_W);
	vector<ll>* ee = new vector<ll>[MAX_N];
	ll* mm = new ll[MAX_N];
	for(int i = 0; i <= n; i++) mm[i] = -1;
	for(int i = 1; i < n; i++){
		ll t1, t2, tw;
		scanf("%lld %lld %lld", &t1, &t2, &tw);
		ee[t1].push_back(t2);
		ee[t2].push_back(t1);
		if(tw > mm[t1]) mm[t1] = tw;
		if(tw > mm[t2]) mm[t2] = tw;
	}	
	for(int i = 1; i <= n; i++){
		sort(ee[i].begin(), ee[i].end());
		//Loop through all the nodes and those that aren't adjacent to the ith node we will count all possibilities. 
	}
	delete []ee;
	return 0;
}
