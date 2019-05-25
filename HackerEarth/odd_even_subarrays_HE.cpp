#include <cstdio>

typedef long long ll;

using namespace std;

int main(void){
	ll n, e = 0, o = 0, temp; 
	ll res = 0;
	scanf("%lld", &n);
	ll* aa = new ll[n+1];	
	ll* bb = new ll[n+1];
	ll* cc = new ll[n+1];
	aa[0] = cc[0] = 0;
	bb[0] = 1;
	for(int i = 1; i < n+1; ++i) bb[i] = cc[i] = 0;
	for(int i = 1; i < n+1; ++i){
		scanf("%lld", &temp);
		if(temp & 1) o++;
		else e++;
		aa[i] = aa[i-1] + ((temp & 1)? -1 : 1);
		if(aa[i] < 0) cc[aa[i]*-1]++;
		else bb[aa[i]]++;
	}
	for(int i = 0; i < n + 1; ++i){
		if(bb[i] > 1) res += (bb[i]*(bb[i] - 1))/2;
		if(cc[i] > 1) res += (cc[i]*(cc[i] - 1))/2; 
	}
	printf("%lld\n", res);
	delete []aa;
	delete []bb;
	delete []cc;
	return 0;
}
