#include <cstdio>
#include <cmath>

typedef long long ll;

using namespace std;

ll MOD = 1e9 + 7;

void mult_matr(ll** aa, int ar, int ac, ll** bb, int br, int bc){
	ll** temp = new ll*[ar];
	for(int i = 0; i < ar; ++i){
		temp[i] = new ll[bc];
		for(int j = 0; j < bc; ++j){
			temp[i][j] = 0;
			for(int k = 0; k < br; ++k){
				temp[i][j] = (temp[i][j] + (aa[i][k]*bb[k][j]) % MOD) % MOD;
			}
		}
	}
	for(int i = 0; i < ar; ++i){
		for(int j = 0; j < bc; ++j){
			aa[i][j] = temp[i][j];
		}
		delete []temp[i];
	}
	delete []temp;
}

ll exponentiate(ll** aa, ll** bb, ll n){
	if(n == 1) return aa[0][0] + aa[0][1];

	exponentiate(aa, bb, n/2LL);
	mult_matr(aa, 3, 3, aa, 3, 3);

	if(n & 1) mult_matr(aa, 3, 3, bb, 3, 3);

	return aa[0][0] + aa[0][1];
}

ll solve(ll n){
	ll** aa = new ll*[3];
	ll** rr = new ll*[3];
	ll** bb = new ll*[3];
	for(int i = 0; i < 3; ++i){
	       	aa[i] = new ll[3]; 
		bb[i] = new ll[3];
		rr[i] = new ll[1];
	}
	bb[0][0] = aa[0][0] = 1;
	bb[0][1] = aa[0][1] = 1;
	bb[0][2] = aa[0][2] = 1;
	bb[1][0] = aa[1][0] = 1;
	bb[1][1] = aa[1][1] = 0;
	bb[1][2] = aa[1][2] = 0;
	bb[2][0] = aa[2][0] = 0;
	bb[2][1] = aa[2][1] = 1;
	bb[2][2] = aa[2][2] = 0;
	rr[0][0] = 2;
	rr[1][0] = 1;
	rr[2][0] = 1;
	return exponentiate(aa, bb, n)%MOD;
}

int main(void){
	int t;
	scanf("%d", &t);
	while(t--){
		ll n;
		scanf("%lld", &n);
		printf("%lld\n", solve(n));
	}		
	return 0;
}
