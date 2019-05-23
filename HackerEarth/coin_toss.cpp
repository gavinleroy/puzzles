#include <cstdio>
#include <cmath>

typedef long long ll;

using namespace std;

ll MOD = 1e9 + 7;

void _print(ll** aa, int ar, int ac){
	for(int i = 0; i < ar; ++i){
		for(int j = 0; j < ac; ++j){
			printf("%lld ", aa[i][j]);
		}
		printf("\n");
	}
	printf("\n");
}
void exponentiate(ll** aa, ll bb[][3], ll n){
	ll** temp = new ll*[3];
	for(int i = 0; i < 3; ++i) temp[i] = new ll[3];
	ll t = n;
	while(t--){
		for(int i = 0; i < 3; ++i){
			for(int j = 0; j < 3; ++j){
				temp[i][j] = 0;
				for(int k = 0; k < 3; ++k){
					temp[i][j] = (temp[i][j] + (aa[i][k]*bb[k][j]) % MOD) % MOD;
				}		
			}
		}
		swap(aa, temp);
		_print(aa, 3, 3);
		printf("temp:\n");
		_print(temp, 3, 3);
	}
	for(int i = 0; i < 3; ++i) delete []temp[i];
	delete []temp;
}

ll** mult_matr(ll** aa, int ar, int ac, ll** bb, int br, int bc){
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
	swap(temp, aa);
	for(int i = 0; i < ar; ++i) delete []temp[i];
	delete []temp;
	return aa;
}

//Create a function that multiplies a matrix by another.
ll solve(ll n){
	ll** aa = new ll*[3];
	ll** rr = new ll*[3];
	for(int i = 0; i < 3; ++i){
	       	aa[i] = new ll[3]; 
		rr[i] = new ll[1];
	}
	ll bb[3][3] = { {1, 1, 1}, {1, 0, 0}, {0, 1, 0} };
	aa[0][0] = 1;
	aa[0][1] = 1;
	aa[0][2] = 1;
	aa[1][0] = 1;
	aa[1][1] = 0;
	aa[1][2] = 0;
	aa[2][0] = 0;
	aa[2][1] = 1;
	aa[2][2] = 0;
//	rr = { {7}, {4}, {2} };
	rr[0][0] = 4;
	rr[1][0] = 2;
	rr[2][0] = 0;
	exponentiate(aa, bb, n-2);
	return mult_matr(aa, 3, 3, rr, 3, 1)[0][0];	
}

//Finish solve function that does the work of [[1, 1, 1],^n    [[fn[2] = 7],
//					       [1, 0, 0],   *   [fn[1] = 4],
//					       [0, 1, 0]]       [fn[0] = 2]]


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
