#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include <vector>
#define MAX ~(1LL << 63)

typedef long long ll;

using namespace std;

struct point{
	ll x;
	ll y;
	bool r = true;
};

int main(void){
	ll n, m, res;
	scanf("%lld %lld", &n, &m);
	point* gg = new point[n]; 
	res = n;
	for(int i = 0; i < n; i++){
		scanf("%lld %lld", &gg[i].x, &gg[i].y);
	}
	for(int i = 0; i < m; i++){
		ll _x, _y;
		scanf("%lld %lld", &_x, &_y);
		int t = -1; 
		bool bt = true;
		for(int j = n-1; j >= 0; j--){
			if(abs(gg[j].y - _y) <= _x - gg[j].x){
				if(t != -1){
					bt = false;
					break;
				}
			       	t = j;
			}
		}
		if(bt){
			if(gg[t].r) res--;
			gg[t].r = false;
		}
	}
	printf("%lld\n", res);
	delete []gg;
	return 0;
}
