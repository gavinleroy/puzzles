#include <cstdio>
#include <algorithm>

typedef long long ll;

using namespace std;

int main(void){
	int n;
	scanf("%d", &n);
	ll* aa = new ll[n];

	for(int i = 0; i < n; i++){
		scanf("%lld", aa+i);
		if(i > 1) aa[i] = max(max(aa[i-2] + aa[i], aa[i]), aa[i-1]);
		else if(i == 1) aa[i] = max(aa[i], aa[i-1]);
	}
	printf("%lld\n", aa[n-1]);
	delete []aa;
	return 0;
}
