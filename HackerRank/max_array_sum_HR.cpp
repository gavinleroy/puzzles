#include <cstdio>
#include <algorithm>

typedef long long ll;

using namespace std;

int main(void){
	int n;
	scanf("%d", &n);
	ll t0, t1, tc;
	for(int i = 0; i < n; i++){
		t0 = t1;
		t1 = tc;
		scanf("%lld", &tc);
		if(i > 1) tc = max(max(t0 + tc, tc), t1);
		else if(i == 1) tc = max(tc, t1);

	}
	printf("%lld\n", tc);
	return 0;
}
