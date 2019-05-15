#include <cstdio>
#include <queue>
#include <algorithm>
#include <vector>

using namespace std;

struct edge{
	int d;
	int w;
};

struct node{
	vector<edge> ee;
	bool v = false;
	int w = 0;
};

int bfs(node* aa, int r){
	int answer = 0;
	queue<int> qq;
	qq.push(r);
	
	while(!qq.empty()){
		int temp = qq.front();
		qq.pop();
		aa[temp].v = true;
		if(aa[temp].ee.size() == 1 && temp != 1){
			answer += (aa[temp].w & 1);
			continue;
		}
		for(auto vec : aa[temp].ee){
			if(!aa[vec.d].v){
				aa[vec.d].w += (vec.w+aa[temp].w);
				qq.push(vec.d);
			}
		}
	}
	return answer;
}

int main(void){
	int t;
	scanf("%d", &t);
	while(t--){
		int n;
		scanf("%d", &n);
		node* aa = new node[n+1];
		for(int i = 0; i < n-1; ++i){
			int a, b, w;
			scanf("%d %d %d", &a, &b, &w);
			edge temp1 = {.d = a, .w = w};
			edge temp2 = {.d = b, .w = w};
			aa[a].ee.push_back(temp2);
			aa[b].ee.push_back(temp1);
		}
		printf("%d\n", bfs(aa, 1));		
	}
}
