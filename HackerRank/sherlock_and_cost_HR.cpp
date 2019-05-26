#include <cstdio>
#include <algorithm>
#define MAX_N 100000
typedef long long ll;

using namespace std;

int main(void){
	int t;
	int* aa = new int[MAX_N];
	scanf("%d", &t);
	
	while(t--){
		int n;
		scanf("%d", &n);
		scanf("%d", aa);
		ll high = 0, low = 0;
		for(int i = 1; i < n; i++){
			scanf("%d", aa+i);
			ll hi_t_low = (aa[i-1] - 1LL);
			ll low_t_hi = (aa[i] - 1LL);
			ll hi_t_hi = ((aa[i-1] > aa[i])? aa[i-1] - aa[i]: aa[i] - aa[i-1]);

			ll n_low = max(low, high+hi_t_low);
			ll n_hi = max(high+hi_t_hi, low+low_t_hi);

			high = n_hi;
			low = n_low;
		}	
		printf("%lld\n", max(low, high));	
	}
	delete []aa;
	return 0;
}
