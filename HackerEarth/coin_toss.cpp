#include <cstdio>
#include <cmath>

typedef long long ll;

using namespace std;

ll MOD = 1e9 + 7;

//Create function that puts an exponent to the nth power.
void exponentiate(){}

//Create a function that multiplies a matrix by another.
void mult_mat(){}

//Finish solve function that does the work of [[1, 1, 1],^n    [[fn[2] = 7],
//					       [1, 0, 0],   *   [fn[1] = 4],
//					       [0, 1, 0]]       [fn[0] = 2]]
ll solve(){}


int main(void){
	int t;
	scanf("%d", &t);
	fill_array();

	while(t--){
		ll n;
		scanf("%lld", &n);
		printf("%lld\n", solve(n));
	}		

	return 0;
}
