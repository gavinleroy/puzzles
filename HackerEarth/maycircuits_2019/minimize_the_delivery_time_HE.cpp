#include <cstdio>
#include <string>
#include <unsorted_map>
#include <vector>
#include <set>
#include <algorithm>

typedef long long ll;

struct node{
	vector<ll> aa;
	ll* p = NULL;
};

using namespace std;

int main(void){
	ll n, m;
	unsorted_map<string, ll> ee;
	scanf("%lld %lld", &n, &m);
	for(ll i = 1; i <= m; i++){
		ll t1, t2, tw;
		scanf("%lld %lld %lld", &t1, &t2, &tw);
		t1 = min(t1, t2);
		t2 = max(t1, t2);
		//Instead replace edge only if it is less than the previous weight.
		ee.insert_or_assign(to_string(t1) + "-" + to_string(t2), tw);
	}
	return 0;
}
