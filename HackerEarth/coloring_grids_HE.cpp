#include <cstdio>
typedef long long ll;

using namespace std;

int main(void){
	ll n, k;
	scanf("%lld %lld", &n, &k);
	printf("%lld\n", ((n==2)? k*k: k));
	return 0;
}
