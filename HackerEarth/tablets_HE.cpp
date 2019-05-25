#include <cstdio>
#include <algorithm>
#define MAX 100001
typedef long long ll;

using namespace std;

int main(void){
	int n;
	scanf("%d", &n);

	ll* vv = new ll[n+2];
	ll* ff = new ll[n+2];
	ll* bb = new ll[n+2];
	ll res = 0;
	vv[0] = ff[0] = bb[0] = ff[n+1] = bb[n+1] = 0;
	vv[n+1] = MAX;

	for(int i = 1; i < n+1; i++){
		scanf("%lld", vv+i);	
		if(vv[i] > vv[i-1]) ff[i] = ff[i-1]+1;
		else ff[i] = 1;
	}
	for(int i = n; i > 0; i--){
		if(vv[i] > vv[i+1]) bb[i] = bb[i+1]+1;
		else bb[i] = 1;	
		res += max(bb[i], ff[i]);
	}

	printf("%lld\n", res);
	delete []vv;
	delete []ff;
	delete []bb;
	return 0;
}
