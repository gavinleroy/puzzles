#include <cstdio>
#include <queue>
#include <algorithm>
#define ll long long

using namespace std;

const ll INF = ~(1LL << 63);
const int I_INF = ~(1 << 31);
bool* vv = new bool[3001];
ll* aa = new ll[3001];
ll** ww = new ll*[3001];

auto cmp = [](const int& l, const int& r) -> bool { return aa[l] < aa[r]; };

void solve_dijkstra(int s, int n){
	int ind = 0;
	int* qq = new int[3001];
	for(int i = 0; i < 3001; ++i){
		qq[i] = I_INF;
	}
	vv[s] = true;
	for(int i = 1; i < n+1; ++i){
		if(ww[s][i] != INF) {
			qq[ind++] = i; 
			aa[i] = ww[s][i];
			vv[i] = true;
		}
	}
	while(ind != 0){
		sort(qq, qq+ind, cmp);
		int temp =  qq[0];
		qq[0] = I_INF;
		swap(qq[0], qq[ind - 1]);
		ind--;
		for(int i = 1; i < n+1; ++i){
			if(ww[temp][i] != INF){
				if(aa[i] == -1 || aa[i] > aa[temp] + ww[temp][i]){
					aa[i] = aa[temp] + ww[temp][i];	
				}
				if(!vv[i]){
					vv[i] = true;
					qq[ind++] = i;
				}
			}
		}
	}
	delete []qq;
}

int main(void){
	int t;
	scanf("%d", &t);
	for(int i = 0; i < 3001; ++i){
		ww[i] = new ll[3001];
	}
	while(t--){
		int n, m, s;
		scanf("%d %d", &n, &m);
		for(int i = 0; i < n+1; ++i){
			aa[i] = -1;
			vv[i] = false;
			for(int j = i; j < n+1; ++j){
				ww[i][j] = ww[j][i] = INF;
			}
		}
		for(int i = 0; i < m; ++i){
			int x, y;
		        ll w;
			scanf("%d %d %lld", &x, &y, &w);
			if(ww[x][y] > w) ww[x][y] = ww[y][x] = w;
		}
		scanf("%d", &s);
		solve_dijkstra(s, n);
		for(int i = 1; i < n+1; ++i){
			if(i != s) printf("%lld ", aa[i]);
		}
		printf("\n");
	}
	for(int i = 0; i < 3001; ++i){
		delete []ww[i];
	}
	delete []ww;
	delete []vv;
	delete []aa;
	return 0;
}
