#include <cstdio>
#include <queue>
#include <algorithm>
#define ll long long

using namespace std;

const ll INF = ~(1LL << 63);
bool* vv = new bool[3001];
ll* aa = new ll[3001];
ll** ww = new ll*[3001];

auto cmp = [](const int& l, const int& r) -> bool { return aa[l] < aa[r]; };

void printqueue(priority_queue<int> q)
{
	//printing content of queue 
	while (!q.empty()){
		printf("%d ", q.top());
		q.pop();
	}
	printf("\n");
}

void solve_dijkstra(int s, int n){
	priority_queue<int, vector<int>, decltype(cmp)> qq(cmp);	
	vv[s] = true;
	for(int i = 1; i < n+1; ++i){
		if(ww[s][i] != INF) {
			qq.push(i); 
			aa[i] = ww[s][i];
			vv[i] = true;
		}
	}
	while(!qq.empty()){
		printqueue(qq);
		int temp =  qq.top();
		qq.pop();
		for(int i = 1; i < n+1; ++i){
			if(ww[temp][i] != INF){
		//		if(temp == 14) printf("i is: %d, edgeweight: %lld\n", i, ww[temp][i]);
				if(aa[i] == -1 || aa[i] > aa[temp] + ww[temp][i]){
		//			if(temp == 14) printf("i is: %d, currWeight: %lld, node weight: %lld\n", i, aa[i], aa[temp]);	
					if(i == 14) printf("before: %lld\n", aa[i]);
					aa[i] = aa[temp] + ww[temp][i];	
					if(i == 14) printf("after: %lld\n\n", aa[i]);
		//			if(temp == 14) printf("i is: %d, newWeight: %lld\n\n", i, aa[i]);
				}
				if(!vv[i]){
					vv[i] = true;
					qq.push(i);
				}
			}
		}
	}
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
			if(x == 11 || y == 11) printf("x is: %d, y is: %d, w is: %lld\n", x, y, w);
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
