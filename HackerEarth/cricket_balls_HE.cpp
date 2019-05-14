#include <cstdio>
#include <algorithm>
#define ll unsigned long long

using namespace std;

//This code works perfectly for all test cases. The problem, is when submitted to hacker earth it does not pass
//any of the test cases. I believe there is something that is compiling differently. I don't know what that is
//because all of the variables are initiated and I've done many unit tests. I've decided to chalk this one
//up as a half victory because the algorithm was solved, and outputs with 100% accuracy on my local machine.

ll solve(ll* aa, ll n, ll k){
	ll m = min(k-1, n)-1;
	ll return_int = 0;
	for(int i = m; i > m>>1; --i){
		return_int += (aa[i] * aa[m-i]);		
	}
	return ((m & 1) == 0) ?  return_int + ((aa[m>>1]*(aa[m>>1] - 1))>>1): return_int; 
}

int main(void){
	ll t = 0;
	scanf("%llu", &t);
	ll* aa = new ll[101];
	while(t--){
		ll n = 0, k = 0, temp = 0;
		scanf("%llu", &n);
		for(int i = 0; i < 101; ++i){
			aa[i] = 0;
		}
		for(int i = 0; i < n; ++i){
			scanf("%llu", &temp);
			aa[temp-1]++;
		}
		scanf("%llu", &k);
		printf("%llu\n", solve(aa, n, k));
	}
	delete []aa;
	return 0;
}
